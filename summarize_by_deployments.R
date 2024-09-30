library(dplyr)
library(tidyr)

source("datetime_string_fns.R")

summarize_by_deployments <- function(boris_data, metadata_filename = "XR6 Cameras.csv") {
  # Get and clean up metadata from deployments
  deployment_metadata <- read.csv(metadata_filename) %>%
    # convert setup date to standard format
    mutate(Setup.date = sprintf("%04d-%02d-%02d", Setup_year, Setup_month, Setup_day)) %>%
    # convert pickup date to standard format (if exists)
    mutate(Pickup.date = ifelse(
      !is.na(Pickup_year) & !is.na(Pickup_month) & !is.na(Pickup_day),
      sprintf("%04d-%02d-%02d", Pickup_year, Pickup_month, Pickup_day),
      NA)) %>%
    # rename some columns
    mutate(Setup.crew = Setup_crew) %>%
    mutate(Puma.id = Puma_ID) %>%
    # fix error in CSV
    mutate(Puma.id = ifelse(Puma.id == "9F ", "9F", Puma.id)) %>%
    # preferred order
    select(
      Puma.id, 
      Setup.date,
      Pickup.date,
      Lat,
      Long,
      Carcass.Species,
      Carcass.Age.Sex,
      Cam_ID,
      Setup.crew,
      General.Location,
      Comments,
      -Setup_year,
      -Setup_month,
      -Setup_day,
      -Pickup_year,
      -Pickup_month,
      -Pickup_day,
      -Setup_crew) %>%
    arrange(Puma.id, Setup.date)
  
  # Get counts and lists of pumas and species at each deployment
  source("summarize_observed_subjects.R")
  observed_subjects <- summarize_observed_subjects(boris_data) %>%
    # extract puma name from Deployment.id
    mutate(Puma.id = sapply(strsplit(Deployment.id, "_"), '[', 1)) %>%
    # extract date from Deployment.id 
    mutate(Setup.date = convert_date_format(
      sapply(strsplit(Deployment.id, "_"), '[', 2))) %>%
    # count values in list columns
    mutate(Unique.pumas = sapply(Pumas, length)) %>%
    mutate(Species.richness = sapply(Species, length)) %>%
    # preferred order
    select(
      Puma.id,
      Setup.date,
      Deployment.id,
      Unique.pumas,
      Species.richness,
      Pumas,
      Species) %>%
    arrange(Puma.id, Setup.date)
  
  # Join metadata to species counts
  deployment_data <- full_join(
    deployment_metadata, 
    observed_subjects, 
    by = c("Puma.id", "Setup.date"))
  
  # join non-feeding puma behaviour counts to deployment data
  source("summarize_puma_non_feeding_behaviours.R")
  puma_non_feeding_behaviours <- summarize_puma_non_feeding_behaviours(boris_data)
  deployment_data <- full_join(
    deployment_data, 
    puma_non_feeding_behaviours, 
    by = "Deployment.id")
  
  # join non-puma carnivore feeding times to deployment data
  source("summarize_carnivore_feeding_times.R")
  carnivore_feeding_times <- summarize_carnivore_feeding_times(boris_data)
  deployment_data <- full_join(
    deployment_data, 
    carnivore_feeding_times, 
    by = "Deployment.id")
  
  # join puma feeding statistics to deployment data
  source("summarize_puma_feeding.R")
  puma_feeding_stats_by_deployment <- summarize_puma_feeding(boris_data)
  deployment_data <- full_join(
    deployment_data, 
    puma_feeding_stats_by_deployment, 
    by = "Deployment.id")
  
  # remove entries with no camera footage
  deployment_data <- deployment_data %>%
    filter(!is.na(Deployment.id)) %>%
    select(-Deployment.id)
  
  return(deployment_data)
}