# Calculate the difference in seconds between two datetime strings
time_difference <- function(datetime1, datetime2) {
  # Convert the strings to POSIXct date-time objects
  dt1 <- as.POSIXct(datetime1)
  dt2 <- as.POSIXct(datetime2)
  
  # Calculate the difference in seconds
  difference_in_seconds <- as.numeric(difftime(dt2, dt1, units = "secs"))
  
  return(difference_in_seconds)
}

# Calculate the difference in seconds between two datetime strings
days_difference <- function(datetime1, datetime2) {
  # Convert the strings to POSIXct date-time objects
  dt1 <- as.POSIXct(datetime1)
  dt2 <- as.POSIXct(datetime2)
  
  # Calculate the difference in seconds
  difference_in_days <- as.numeric(difftime(dt2, dt1, units = "days"))
  
  return(difference_in_days)
}

# Convert from 9Apr19 to 2019-04-09 format
convert_date_format <- function(datestring) {
  date_formatted <- as.Date(datestring, format = "%d%b%y")
  return(format(date_formatted, "%Y-%m-%d"))
}

convert_exiftool_datetime <- function(datetime) {
  dt <- as.POSIXct(datetime, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
  return(format(dt, "%Y-%m-%d %H:%M:%S"))
}

# Add a number of seconds to a datetime string
add_seconds_to_exiftool_datetime <- function(datetime, seconds) {
  dt <- as.POSIXct(datetime, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
  dt <- dt + seconds
  return(format(dt, "%Y-%m-%d %H:%M:%S"))
}

convert_to_deployment_id_format <- function(datestring) {
  date <- as.Date(datestring)
  formatted_date <- format(date, "%d%b%y")
  
  # Remove leading zer0
  formatted_date <- sub("^0", "", formatted_date)
  
  return(formatted_date)
}