# Calculate the difference in seconds between two datetime strings
time_difference <- function(datetime1, datetime2) {
  # Convert the strings to POSIXct date-time objects
  dt1 <- as.POSIXct(datetime1)
  dt2 <- as.POSIXct(datetime2)
  
  # Calculate the difference in seconds
  difference_in_seconds <- as.numeric(difftime(dt2, dt1, units = "secs"))
  
  return(difference_in_seconds)
}

# Convert from 9Apr19 to 2019-04-09 format
convert_date_format <- function(date) {
  date_formatted <- as.Date(date, format = "%d%b%y")
  return(format(date_formatted, "%Y-%m-%d"))
}