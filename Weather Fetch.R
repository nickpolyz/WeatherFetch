# Weather Fetch
# By Nicholas Polyzogopoulos
# July 2nd 2019
# ------------------------------
# REQUIRED:
# - wgrib2
# - rNOMADS package for R
# - Internet connection
# ------------------------------
# DESCRIPTION:
# Fetches Data from Global Forecast System (GFC)
# Gets data for every day of 2018, including:
# Temperature at 2 m above ground
# and Relative Humidity for 800mb
# for Vancouver, BC
# as a 7 day (168 hour) prediction
# where all predictions are taken at 10 AM local time

# set the model parameters:
abbrev <- "gfs4" # model to use (each model has an abbreviation)
model.run <- 18 # which hour the model was run (in UTC time zone (PST = UTC - 8))
preds <- 168 # how many hours in the future it is forecasting (168 = 7 days)
levels <- c("2 m above ground") # What region of the atmosphere to get data for
variables <- c("TMP", "RH") # Temperature and relative humidity (data to return)

# define a region to get data for:
# this speeds up the proccess significantly
region <-  c(-125, -120, 50, 49) # format:  c(LEFT LON, RIGHT LON, NORTH LAT, SOUTH LAT)

# Set location to Vancouver, BC
lat <- 49.2827
lon <- -123.1207

# Set months and days to perform up to:
MaxMonth <- 12
MaxDay <- 31

# record number of datapoints collected:
datapoints <- 0

for(j in 1:MaxMonth){ # every month
  for(i in 1:MaxDay){ # every possible day
    
    # check if real date (since some months only go up to 28, 29, or 30 days)
    # below are the date exceptions for year 2018
    if((j == 2 && i > 28) ||
       (j == 4 && i > 30) ||
       (j == 6 && i > 30) ||
       (j == 9 && i > 30) ||
       (j == 11 && i > 30)
       ){
        break
    }
    
    model.date <- 20180000 + i + (j*100) # date when the model was run (YYYYMMDD)
    
    ## get the data:
      model.info <- ArchiveGribGrab(abbrev, model.date, model.run, preds, file.type = "grib2")
      model.data <- ReadGrib(model.info[[1]]$file.name, levels, variables, forecasts = NULL, 
                             domain = region, domain.type = "latlon",
                             file.type = "grib2", missing.data = 0)

      profile <- BuildProfile(model.data, lon, lat, TRUE) # gets the specific temp & RH for this long/lat
      
      if(j == 1 && i == 1){ #for first run
        datavec <- c(profile[[1]]$profile.data[1,1,1], profile[[1]]$profile.data[1,2,1], model.data[["model.run.date"]][1],
                     profile[[1]]$forecast.date, profile[[1]]$variables, profile[[1]]$levels)
        datapoints <- 1
        next
      }
      
      newdata <- c(profile[[1]]$profile.data[1,1,1], profile[[1]]$profile.data[1,2,1], model.data[["model.run.date"]][1],
                   profile[[1]]$forecast.date, profile[[1]]$variables, profile[[1]]$levels)
      datavec <- c(datavec, newdata)

      #increment the number of datapoints:
      datapoints <- datapoints + 1
  }
}
# end loop

# put data into a nice table:
datatable <- matrix(datavec, nrow=datapoints, ncol = 7, byrow=TRUE,
                    dimnames = list(NULL,
                                    c("Forecast Temp (Kelvin)", "Forecast RH (%)","Date Forecast Ran (UTC)", 
                                      "Forecasted Date (UTC)", "Data Type 1", "Data Type 2", "level")))

# write data to CSV file:
write.csv(datatable, file = "2018-forecasts.csv", row.names=F)
