library(dplyr)
library(tidyr)

# README
# This file combines deployment metadata with basic boris stats
# and joins this with deployment metadata from XR6 Cameras.csv.
# Then these datasets are pruned from deployments deployed for insufficient time.
# Outputs are boris_data_pruned.csv and deployment_data_pruned.csv.
# deployment_data_for_barchart.csv is also output, which is before final pruning.
# BEFORE START:
# 1. boris_tidy.csv must be generated and available from setup_boris_data
# 2. DEPLOYMENT_METADATA_FILENAME must be set correctly

source("datetime_string_fns.R")

######################################################
# 1. Join deployment metadata to stats of BORIS data #
######################################################

DEPLOYMENT_METADATA_FILENAME = "XR6 Cameras.csv"

species_list = c("Mule Deer", "Elk", "Unknown")
age_list = c("Calf", "Fawn", "Yearling", "Adult", "Unknown")
sex_list = c("Female", "Male", "Unknown")

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
  # fix missing pickup date for 1F_1Aug17 (confirmed on video)
  mutate(Pickup.date = ifelse(Deployment.id == "1F_1Aug17", "2017-09-01", Pickup.date)) %>%
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
  arrange(Deployment.id) %>%
  
  # clean up prey demographics
  mutate(Carcass.species = factor(ifelse(is.na(Carcass.species), 
                                         "Unknown", 
                                         ifelse(tolower(trimws(Carcass.species)) == "mule deer", 
                                                "Mule Deer", "Elk")), 
                                  levels = species_list)) %>%
  mutate(Carcass.age.sex = tolower(trimws(Carcass.age.sex))) %>%
  mutate(Carcass.age = factor(ifelse(grepl("adult", Carcass.age.sex), 
                                     "Adult", 
                                     ifelse(grepl("yearling", Carcass.age.sex), 
                                            "Yearling", 
                                            ifelse(grepl("fawn", Carcass.age.sex), 
                                                   "Fawn", 
                                                   ifelse(grepl("calf", Carcass.age.sex), 
                                                          "Calf", "Unknown")))), 
                              levels = age_list)) %>%
  mutate(Carcass.sex = factor(ifelse(grepl("male|buck|bull", Carcass.age.sex), 
                                     "Male", 
                                     ifelse(grepl("female|doe|cow", Carcass.age.sex), 
                                            "Female", "Unknown")), 
                              levels = sex_list)) %>%
  select(-Carcass.age.sex)

boris_data <- read.csv("boris_tidy.csv")

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

# remove deployments < 21 days and 6M (just one deployment with no observations)
deployment_data <- deployment_data %>%
  filter(!is.na(Days.deployed) & Days.deployed >= 21)

write.csv(deployment_data, "deployment_data_for_barchart.csv", row.names = FALSE)

###############################################################################################
# 2. Prune BORIS observations from underdeployed cameras and remove observations past 21 days #
###############################################################################################

deployment_data <- deployment_data %>%
  filter(Puma.name != "6M")

boris_data_pruned <- boris_data %>%
  # remove deployments already removed from deployment_data
  filter(Deployment.id %in% deployment_data$Deployment.id) %>%
  # remove behaviours past the cutoff time (21 days)
  left_join(deployment_data %>% select(Deployment.id, Setup.date), by = "Deployment.id") %>%
  filter(is_before(Video.start, get_behaviour_time_cutoff(Setup.date))) %>%
  select(-Behaviour.start, -Behaviour.end, -Setup.date)

# recreate deployment data
pruned_deployments_summary <- boris_data_pruned %>%
  group_by(Deployment.id) %>%
  summarize(
    First.behaviour.start = first(Video.start),
    Last.behaviour.end = last(Video.end),
    Days.observed = days_difference(First.behaviour.start, Last.behaviour.end),
    N.observations = n(),
    .groups = 'drop'
  )
deployment_data_pruned <- full_join(deployment_metadata,
                                    pruned_deployments_summary, 
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
  ) %>%
  filter(!is.na(Days.deployed) & Days.deployed >= 21) %>%
  filter(Puma.name != "6M")

write.csv(deployment_data_pruned, "deployment_data_pruned.csv", row.names = FALSE)
write.csv(boris_data_pruned, "boris_data_pruned.csv", row.names = FALSE)
