source("PA_paper/LoadPackages_PA.R")
library(ncdf4)
library(dplyr)

GPS.eReefs<-read.csv("20240304data.crw.csv")%>%filter(data_type==0)


# month variable
# Define the start and end dates
start_date <- ym("2010-09")
end_date <- ym("2023-11")

# Create a sequence of months between the start and end dates
months <- seq(start_date, end_date, by = "month")
GPS.eReefs<-GPS.eReefs%>%mutate(aus.date=as.Date(aus.date))
# create a vector to store the dates in the server files
dates_in_server <- unique(format(as.Date(months), "%Y-%m"))
# Define online repository URL
url_prefix <- "https://thredds.ereefs.aims.gov.au/thredds/dodsC/s3://aims-ereefs-public-prod/derived/ncaggregate/ereefs/gbr4_v2/daily-monthly/EREEFS_AIMS-CSIRO_gbr4_v2_hydro_daily-monthly-"

# Get the file names
file_names <- paste0(url_prefix, dates_in_server, ".nc")

######## hydrodynamic- 4 dimension data
# Define desired variable IDs
desired_varids_4dims <- c("salt", "temp", "mean_cur", "u", "v")
# GPS.eReefs<-GPS.eReefs |> mutate(salt="", temp="", mean_cur="", u="", v="") # first run only

# pull out desired variables
for (file_name in file_names[seq(from=length(file_names), to=76, by=-1)]) {

  print(file_name)

  date=gsub(".*?(\\d{4}-\\d{2}).*", "\\1", file_name)
  # Check if date exists in GPS.eReefs
  if (is.na(any(format(GPS.eReefs$aus.date, "%Y-%m") == date))) {
    next
  }


  # Open netCDF file
  nc_data <- nc_open(file_name)

  # Loop through rows with matching date
  for (row in which(format(GPS.eReefs$aus.date, "%Y-%m") == date)) {
    lon <- GPS.eReefs[row, "lon"]
    lat <- GPS.eReefs[row, "lat"]
    GPS.day<- as.POSIXct(GPS.eReefs[row,]$aus.date)

    # Find closest pixel
    lon_index <- which.min(abs(nc_data$dim$longitude$vals - (lon)))
    lat_index <- which.min(abs(nc_data$dim$latitude$vals - (lat)))

    # match date with nc
# Define the starting date
start_date <- as.POSIXct("1990-01-01 UTC", tz = "UTC")+ hours(10)

date_index<-which(nc_data$dim$time$vals==round(GPS.day-start_date))

    # Extract data for each variable
variable_data<-data.frame()
    for (varid in desired_varids_4dims){
      name<-varid
      print(name)

      attempt <- 1
      while(attempt <= 3 ) {
        an.error.occured <- FALSE
        tryCatch(
          data<-ncvar_get(nc = nc_data, varid = varid,
                          start = c(lon_index, lat_index, 17, date_index),
                          count = c(1,1,1,1)),
          error=function(err) {
            print("AN ERROR OCCURED")
            print(err)
            an.error.occured <<- TRUE
          })

        if (isTRUE(an.error.occured)) {
          attempt <- attempt + 1
          if (attempt > 3) stop("Error reading variable data")
# Sys.sleep(10)
        }
        else {
          break
        }

      }
        data<-cbind(name, data)
        variable_data<-rbind(variable_data, data)
      }

    # Append extracted data to GPS.eReefs
    GPS.eReefs[row, c(desired_varids_4dims)] <- variable_data$data

  }
  write.csv(GPS.eReefs, file="hydro_4dimscrwbackwards.csv")
  # Close netCDF file
nc_close(nc_data)

}

# write.csv(GPS.eReefs, file="hydro_4dimscrwbackwards.csv")

######## hydrodynamic- 3 dimension data
# Define desired variable IDs
desired_varids_3dims <- c("mean_wspeed")
GPS.eReefs<-GPS.eReefs |> mutate(mean_wspeed="")

for (file_name in file_names) {
  print(file_name)
  
  date=gsub(".*?(\\d{4}-\\d{2}).*", "\\1", file_name)
  # Check if date exists in GPS.eReefs
  if (is.na(any(format(as.Date(GPS.eReefs$aus.date), "%Y-%m") == date))) {
    next
  }

  
  # Open netCDF file
  nc_data <- nc_open(file_name)
  
  # Loop through rows with matching date
  for (row in which(format(as.Date(GPS.eReefs$aus.date), "%Y-%m") == date)) {
      # Check if there is already a value for mean_wspeed
  # if (!is.null(GPS.eReefs[row, "mean_wspeed"])) {
  #   # If the value is not NA, skip to the next iteration
  #   next
  # }
    lon <- GPS.eReefs[row, "lon"]
    lat <- GPS.eReefs[row, "lat"]
    GPS.day<- as.Date(GPS.eReefs[row, "aus.date"])
    
    # Find closest pixel
    lon_index <- which.min(abs(nc_data$dim$longitude$vals - (lon)))
    lat_index <- which.min(abs(nc_data$dim$latitude$vals - (lat)))
    
    # match date with nc
    # Define the starting date
    start_date <- as.POSIXct("1990-01-01 UTC", tz = "UTC")+ hours(10)
    
    date_index<-which(nc_data$dim$time$vals==round(as.POSIXct(GPS.day)-start_date))
    
    # Extract data for each variable
    variable_data<-data.frame()
    for (varid in desired_varids_3dims){
      name<-varid
      print(name)
      
      attempt <- 1
      while(attempt <= 3 ) {       
        an.error.occured <- FALSE
        tryCatch(
      data<-ncvar_get(nc = nc_data, varid = varid,
                      start = c(lon_index, lat_index, date_index),
                      count = c(1,1,1)),
          error=function(err) {
            print("AN ERROR OCCURED")
            print(err)
            an.error.occured <<- TRUE
          })

        if (isTRUE(an.error.occured)) {
          attempt <- attempt + 1
          if (attempt > 3) stop("Error reading variable data")
        }
        else {
          break
        }
        
      }

      data<-cbind(name, data)
      variable_data<-rbind(variable_data, data)
    }
    
    # Append extracted data to GPS.eReefs
    GPS.eReefs[row, c(desired_varids_3dims)] <- variable_data$data
  }
  
  # Close netCDF file
  nc_close(nc_data)
  write.csv(GPS.eReefs, file="hydro_3dimscrw.csv")
}
write.csv(GPS.eReefs, file="hydro_3dimscrw.csv")

######### bgc data
GPS.eReefs<-read.csv("hydro_4dimscrw.csv")
start_date <- ym("2010-12")
end_date <- ym("2019-04")
months <- seq(start_date, end_date, by = "month")
dates_in_server <- unique(format(as.Date(months), "%Y-%m"))
# Define online repository URL
url_prefix <- "https://thredds.ereefs.aims.gov.au/thredds/dodsC/s3://aims-ereefs-public-prod/derived/ncaggregate/ereefs/GBR4_H2p0_B3p1_Cq3b_Dhnd/daily-monthly/EREEFS_AIMS-CSIRO_GBR4_H2p0_B3p1_Cq3b_Dhnd_bgc_daily-monthly-"

# Get the file names
file_names <- paste0(url_prefix, dates_in_server, ".nc")

# Define desired variable IDs
desired_varids <- c("ZooL_N", "EFI")
desired_varids_3dims<-c("Secchi")
GPS.eReefs<-GPS.eReefs |> mutate(ZooL_N="", EFI="", Secchi="")

for (file_name in file_names) {
  print(file_name)

  date=gsub(".*?(\\d{4}-\\d{2}).*", "\\1", file_name)
  # Check if date exists in GPS.eReefs
  if (is.na(any(format(as.Date(GPS.eReefs$aus.date), "%Y-%m") == date))) {
    next
  }


  # Open netCDF file
  nc_data <- nc_open(file_name)

  # Loop through rows with matching date
  for (row in which(format(as.Date(GPS.eReefs$aus.date), "%Y-%m") == date)) {
    lon <- GPS.eReefs[row, "lon"]
    lat <- GPS.eReefs[row, "lat"]
    GPS.day<- as.Date(GPS.eReefs[row, "aus.date"])

    # Find closest pixel
    lon_index <- which.min(abs(nc_data$dim$longitude$vals - (lon)))
    lat_index <- which.min(abs(nc_data$dim$latitude$vals - (lat)))

    # match date with nc
    # Define the starting date
    start_date <- as.POSIXct("1990-01-01 UTC", tz = "UTC")+ hours(10)

    date_index<-which(floor(nc_data$dim$time$vals)==round(as.POSIXct(GPS.day)-start_date))

    # Extract data for each variable
    variable_data<-data.frame()
        for (varid in desired_varids){
      name<-varid
      print(name)

      attempt <- 1
      while(attempt <= 3 ) {
        an.error.occured <- FALSE
        tryCatch(
          data<-ncvar_get(nc = nc_data, varid = varid,
                          start = c(lon_index, lat_index, 17, date_index),
                          count = c(1,1,1,1)),
          error=function(err) {
            print("AN ERROR OCCURED")
            print(err)
            an.error.occured <<- TRUE
          })

        if (isTRUE(an.error.occured)) {
          attempt <- attempt + 1
          if (attempt > 3) stop("Error reading variable data")
          Sys.sleep(10)
        }
        else {
          break
        }

      }
      data<-cbind(name, data)
      variable_data<-rbind(variable_data, data)
    }

    # Append extracted data to GPS.eReefs
    GPS.eReefs[row, c(desired_varids)] <- variable_data$data
  }

  # Close netCDF file
  nc_close(nc_data)
  saveRDS(GPS.eReefs, file="bgc_4dimscrw.Rds")
}

# write.csv(GPS.eReefs, file="bgc_4dimscrw.csv")

for (file_name in file_names) {
  print(file_name)
  
  date=gsub(".*?(\\d{4}-\\d{2}).*", "\\1", file_name)
  # Check if date exists in GPS.eReefs
  if (is.na(any(format(as.Date(GPS.eReefs$aus.date), "%Y-%m") == date))) {
    next
  }
  
  
  # Open netCDF file
  nc_data <- nc_open(file_name)
  
  # Loop through rows with matching date
  for (row in which(format(as.Date(GPS.eReefs$aus.date), "%Y-%m") == date)) {
    lon <- GPS.eReefs[row, "lon"]
    lat <- GPS.eReefs[row, "lat"]
    GPS.day<- as.Date(GPS.eReefs[row, "aus.date"])
    
    # Find closest pixel
    lon_index <- which.min(abs(nc_data$dim$longitude$vals - (lon)))
    lat_index <- which.min(abs(nc_data$dim$latitude$vals - (lat)))
    
    # match date with nc
    # Define the starting date
    start_date <- as.POSIXct("1990-01-01 UTC", tz = "UTC")+ hours(10)
    
    date_index<-which(floor(nc_data$dim$time$vals)==round(as.POSIXct(GPS.day)-start_date))
    
    # Extract data for each variable
    variable_data<-data.frame()
    for (varid in desired_varids_3dims){
      name<-varid
      print(name)
      
      attempt <- 1
      while(attempt <= 3 ) {       
        an.error.occured <- FALSE
        tryCatch(
          data<-ncvar_get(nc = nc_data, varid = varid,
                          start = c(lon_index, lat_index, date_index),
                          count = c(1,1,1)),
          error=function(err) {
            print("AN ERROR OCCURED")
            print(err)
            an.error.occured <<- TRUE
          })
        
        if (isTRUE(an.error.occured)) {
          attempt <- attempt + 1
          if (attempt > 3) stop("Error reading variable data")
        }
        else {
          break
        }
        
      }
      
      data<-cbind(name, data)
      variable_data<-rbind(variable_data, data)
    }
    
    # Append extracted data to GPS.eReefs
    GPS.eReefs[row, c(desired_varids_3dims)] <- variable_data$data
  }
  
  # Close netCDF file
  nc_close(nc_data)
  saveRDS(GPS.eReefs, file="bgc_3dimscrw.csv")
}

#write.csv(GPS.eReefs, file="bgc_3dimscrw.csv")