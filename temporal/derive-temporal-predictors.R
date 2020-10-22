####### Script Information ########################
# Brandon P.M. Edwards
# NA-POPS: covariates
# derive-temporal-predictors.R
# Created October 2020
# Last Updated October 2020

####### Import Libraries and External Files #######

library(maptools)
library(here)

source("../utilities/get-data.R")

####### Read Data and Derive Predictors ###########

# Get project names
project_list <- read.table(here::here("../utilities/proj-list"))
n_proj <- nrow(project_list)

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
  project_sample$JD <- project_sample$date_time$yday
  
  # Create temp df containing no NAs for time or lat/long
  temp <- project_sample[which(!is.na(project_sample$date_time)), ]
  temp <- temp[which(!is.na(temp$Latitude)), ]
  if (nrow(temp) == 0)
  {
    project_sample$TSSR <- NA
    write.table(x = project_sample[, c("Sample_ID", "JD", "TSSR")],
                file = paste0("temporal/project-",
                              p,
                              ".csv"),
                row.names = FALSE,
                sep = ",")
    next()
  }

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
  
  # Merge this temp df back into the original, keeping the NAs
  #   where TSSR cannot be calculated.
  project_sample <- merge(x = project_sample,
                          y = temp[, c("Sample_ID", "TSSR")],
                          by = "Sample_ID",
                          all = TRUE)
  
  # Output predictors by project
  write.table(x = project_sample[, c("Sample_ID", "JD", "TSSR")],
              file = paste0("temporal/project-",
                            p,
                            ".csv"),
              row.names = FALSE,
              sep = ",")
}
