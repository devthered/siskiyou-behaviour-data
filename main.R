library(dplyr)
library(tidyr)

source("datetime_string_fns.R")
source("aggregate_subjects.R")
source("puma_non_feeding_behaviours.R")
source("summarize_carnivore_feeding_times.R")

# Read in all boris data and clean up
boris_data <- read.csv("combined_boris_data.csv")
boris_data <- boris_data %>%
  # extract Deployment.id (first two parts of Observation.id)
  mutate(Deployment.id = 
           sapply(strsplit(Observation.id, "_"), 
                  function(x) paste(x[1:2], collapse = "_"))) %>%
  select(-Observation.id) %>%
  # fix some errors in the IDs
  mutate(Deployment.id = ifelse(Deployment.id == "1F_30Sept17", "1F_30Sep17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "1F_7Octo17", "1F_7Oct17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "1f_15Oct17", "1F_15Oct17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "1F_4Nov171", "1F_4Nov17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "1F_Apr19", "1F_9Apr19", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "5M_5Sept17", "5M_5Sep17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "5_22Nov17", "5M_22Nov17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "5M_22Nov17)B14", "5M_22Nov17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "7M_5Feb18", "7M_15Feb18", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "8F_Mar18", "8F_28Mar18", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "8F_14Dec19", "8F_14Dec18", Deployment.id)) %>%
  # remove unnecessary columns
  select(Deployment.id, everything(), -Behavioral.category, -FPS)


# Get and clean up metadata from deployments
deployment_metadata <- read.csv("XR6 Cameras.csv") %>%
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
deployment_subjects <- aggregate_subjects(boris_data) %>%
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
  deployment_subjects, 
  by = c("Puma.id", "Setup.date"))

# join non-feeding puma behaviour counts to deployment data
puma_non_feeding_behaviours <- summarize_puma_non_feeding_behaviours(boris_data)
deployment_data <- full_join(
  deployment_data, 
  puma_non_feeding_behaviours, 
  by = "Deployment.id")

# join non-puma carnivore feeding times to deployment data
carnivore_feeding_times <- summarize_carnivore_feeding_times(boris_data)
deployment_data <- full_join(
  deployment_data, 
  carnivore_feeding_times, 
  by = "Deployment.id")
