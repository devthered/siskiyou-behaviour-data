# README
# This file combines all the BORIS behavioural data CSVs into a single CSV.
# This must be run to generate combined_boris_data.csv before running setup_boris_data.R
# PATH_TO_TAG_BORIS must be set properly below.

# Set Working Directory to ./TAG_BORIS
PATH_TO_TAG_BORIS <- "/Volumes/One Touch/Siskiyou/CamTrap/XR6/TAG_BORIS"
setwd(csv_dir)

# Get all CSV files
csv_files <- list.files(pattern = "\\.csv$")

# Read all CSVs into data frames
csv_data_frames <- list()
weird_data_frames <- list()
for (file in csv_files) {
  df <- read.csv(file)
  
  # remove Description column if present (only on some files)
  df <- df[, !colnames(df) == "Description", drop = FALSE]
  
  # include only if data includes "Subject" column
  # some CSVs followed a different format and will be ignored
  if ("Subject" %in% colnames(df)) {
    csv_data_frames[[file]] <- df
  } else {
    weird_data_frames[[file]] <- df
  }
}

# Combine data frames into a single data frame
combined_data <- do.call(rbind, csv_data_frames)

# Write data to a new, combined CSV in this directory
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_dir)
write.csv(combined_data, "combined_boris_data.csv", row.names = FALSE)