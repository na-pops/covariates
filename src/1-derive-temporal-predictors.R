####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: covariates
# 1-derive-temporal-predictors.R
# Created October 2020
# Last Updated November 2021

####### Import Libraries and External Files #######

library(maptools)
library(here)
library(sf)

source("../utilities/get-data.R")

####### Read Data and Derive Predictors ###########

# Get project names
project_list <- read.table(here::here("../utilities/proj-list"))
n_proj <- nrow(project_list)
bcr <- read_sf("../utilities/shp/bcr",
               layer = "BBS_BCR_strata")
sf::sf_use_s2(FALSE)

for (i in 1:n_proj)
{
  p <- project_list$V1[i]
  
  # Get samples
  data_dir <- paste0("../project-",
                     p,
                     "/output/",
                     p,
                     "_samples.rda")
  project_sample <- data.frame(get_data(data_dir))

  project_sample$date_time <- strptime(project_sample$UTC, 
                                       format = "%Y-%m-%dT%H:%M:%SZ", 
                                       tz = "UTC")
  # Extract Julian day
  project_sample$OD <- (project_sample$date_time$yday)# / 365
  #project_sample$OD2 <- (project_sample$OD)^2
  
  # Create temp df containing no NAs for time or lat/long
  temp <- project_sample[which(!is.na(project_sample$date_time)), ]
  temp <- temp[which(!is.na(temp$Latitude)), ]

  # Calculate time since local sunrise
  coords <- matrix(c(temp$Longitude, temp$Latitude),
                   nrow = nrow(temp))
  
  temp <- cbind(temp, sunriset(coords,
                              as.POSIXct(temp$date_time),
                              direction = "sunrise",
                              POSIXct.out = TRUE))
  
  temp$TSSR <- as.numeric(difftime(temp$date_time,
                                   temp$time,
                                   units = "hours"))
  #temp$TSSR <- temp$TSSR / 24
  #temp$TSSR2 <- (temp$TSSR)^2
  
  # Get BCR
  pts <- st_as_sf(data.frame(coords), coords = 1:2, crs = 4326)
  bcr <- bcr %>% st_transform(st_crs(pts))
  bcr_names <- bcr$ST_12
  intersections <- as.integer(st_nearest_feature(pts, bcr))
  bcr_char <- bcr_names[intersections]
  temp$BCR <- as.numeric(substring(bcr_char, first = 4))
  
  
  # Merge this temp df back into the original, keeping the NAs
  #   where TSSR cannot be calculated.
  project_sample <- merge(x = project_sample,
                          y = temp[, c("Sample_ID", "TSSR", "BCR")],
                          by = "Sample_ID",
                          all = TRUE)
  
  # Output predictors by project
  write.table(x = project_sample[, c("Sample_ID", "OD", "TSSR", "BCR")],
              file = paste0("temporal/project-",
                            p,
                            ".csv"),
              row.names = FALSE,
              sep = ",")
}
