library(dplyr)
library(tidyr)

# README
# This file summarizes behavioural data by camera deployment
# and joins this with deployment metadata from XR6 Cameras.csv
# BEFORE START:
# 1. boris_data must be read in (boris_tidy.csv from setup_boris_data)
# 2. DEPLOYMENT_METADATA_FILENAME must be set correctly

# TODO: incorporate video times

source("datetime_string_fns.R")

####################################################
# 1. Join deployment metadata to basic BORIS stats #
####################################################

DEPLOYMENT_METADATA_FILENAME = "XR6 Cameras.csv"

# Get and clean up metadata from deployments
deployment_metadata <- read.csv(DEPLOYMENT_METADATA_FILENAME) %>%
  # fix error in CSV
  mutate(Puma.name = ifelse(Puma_ID == "9F ", "9F", Puma_ID)) %>%
  # convert setup date to standard format
  mutate(Setup.date = sprintf("%04d-%02d-%02d", Setup_year, Setup_month, Setup_day)) %>%
  # create Deployment.id to match boris_data
  mutate(Deployment.id = paste(Puma.name, convert_to_deployment_id_format(Setup.date), sep = "_")) %>%
  # convert pickup date to standard format (if exists)
  mutate(Pickup.date = ifelse(
    !is.na(Pickup_year) & !is.na(Pickup_month) & !is.na(Pickup_day),
    sprintf("%04d-%02d-%02d", Pickup_year, Pickup_month, Pickup_day),
    NA)) %>%
  # calculate days deployed
  mutate(Days.deployed = as.integer(days_difference(Setup.date, Pickup.date))) %>%
  # preferred order
  select(
    Deployment.id,
    Puma.name, 
    Setup.date,
    Pickup.date,
    Days.deployed,
    Lat,
    Long,
    Carcass.species = Carcass.Species,
    Carcass.age.sex = Carcass.Age.Sex,
    Cam.id = Cam_ID,
    Setup.crew = Setup_crew,
    General.location = General.Location,
    Comments) %>%
  arrange(Deployment.id)

if (!exists("boris_data")) {
  stop("Error: boris_data must exist in order to summarize by deployments")
}

# Summarize timespan and number of observations
deployment_observations_summary <- boris_data %>%
  group_by(Deployment.id) %>%
  summarize(
    First.behaviour.start = first(Video.start),
    Last.behaviour.end = last(Video.end),
    Days.observed = days_difference(First.behaviour.start, Last.behaviour.end),
    N.observations = n()
  ) %>%
  ungroup()

deployment_data <- full_join(deployment_metadata, 
                             deployment_observations_summary, 
                             by = "Deployment.id") %>%
  mutate(N.observations = ifelse(is.na(N.observations), 0, N.observations)) %>%
  select(
    Deployment.id,
    Setup.date,
    Pickup.date,
    First.behaviour.start,
    Last.behaviour.end,
    Days.deployed,
    Days.observed,
    N.observations,
    everything()
  )

#############################################################
# 2. Generate behavioural observation stats for all species #
#############################################################

# Get counts and lists of pumas and species at each deployment
# then converts Deployment.id into puma name and date for matching
source("summarize_observed_subjects.R")
observed_subjects <- summarize_observed_subjects(boris_data) %>%
  # extract puma name from Deployment.id
  mutate(Puma.id = sapply(strsplit(Deployment.id, "_"), '[', 1)) %>%
  # extract date from Deployment.id 
  mutate(Setup.date = convert_date_format(
    sapply(strsplit(Deployment.id, "_"), '[', 2))) %>%
  # preferred order
  select(Puma.id, Setup.date, everything()) %>%
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
  filter(!is.na(Deployment.id))

# Replace NA values with 0 except for certain columns
deployment_data <- deployment_data %>%
  mutate(across(
    -c("Pickup.date",
       "Std Dev Feeding Bout Duration (s) Puma", 
       "Std Dev Feeding Bout Duration (s) Kitten"), 
    ~ replace(., is.na(.), 0)))     

# write.csv(deployment_data, "puma_kill_camera_deployments.csv", row.names = FALSE)