library(dplyr)
library(tidyr)

source("datetime_string_fns.R")
source("aggregate_subjects.R")
source("puma_non_feeding_behaviours.R")

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

# regular expression to match puma names
PUMA_REGEX <- "\\d+[MF]|PUMA.*|KITTEN[123]"

# Sum up feeding times for all non-puma individuals
# NOTE: May overinflate feeding times for group feeders, 
# as it assumes all group members are feeding simultaneously for 100% of recorded time.
non_puma_feeding_times <- boris_data %>%
  # NZ spelling
  rename(Behaviour = Behavior) %>%
  # not interested in pumas here
  filter(!grepl(PUMA_REGEX, Subject)) %>%
  # only interested in feeding behaviours
  filter(Behaviour == 'feed' | Behaviour == 'feed (group)') %>%
  # clean up modifiers
  mutate(Modifiers = ifelse(Modifiers == "None", "0", Modifiers)) %>%
  mutate(Modifiers = ifelse(Modifiers == "", "0", Modifiers)) %>%
  # multiply feeding time by number of individuals when multiple individuals are present
  mutate(Feeding.Time.Per.Individual = ifelse(is.na(Modifiers), Duration..s., as.numeric(Modifiers) * Duration..s.)) %>%
  # sum behaviour counts and durations for each subject and behaviour
  group_by(Deployment.id, Subject) %>%
  summarise(
    Total.Feeding.Time = sum(Feeding.Time.Per.Individual),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  arrange(Deployment.id, Subject)

carnivore_class_feeding_times <- non_puma_feeding_times %>%
  mutate(Subject = ifelse(Subject == "BEAR black", "Black Bear", Subject)) %>%
  mutate(Subject = ifelse(Subject == "BIRD (other)", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "BOBCAT", "Mesocarnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "CHIPMUNK", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "COW domestic", "Domestic", Subject)) %>%
  mutate(Subject = ifelse(Subject == "COYOTE", "Mesocarnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "DOG domestic", "Domestic", Subject)) %>%
  mutate(Subject = ifelse(Subject == "EAGLE bald", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "EAGLE golden", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "FISHER", "Mesocarnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "FLICKER northern", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "FOX gray", "Mesocarnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "GOSHAWK northern", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "GROUSE", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "HAWK red-tailed", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "JAY scrub", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "JAY Stellar's", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "LAGOMORPH", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "MAGPIE", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "MOUSE", "Small Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "RACCOON", "Mesocarnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "RAVEN common", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "REPTILE", "Reptile", Subject)) %>%
  mutate(Subject = ifelse(Subject == "ROBIN American", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SKUNK striped", "Small Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SKUNK western spotted", "Small Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SQUIRREL California ground", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SQUIRREL Douglas", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SQUIRREL golden-mantled ground", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SQUIRREL western gray", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "THRUSH varied", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "UNKNOWN", "Unknown", Subject)) %>%
  mutate(Subject = ifelse(Subject == "VULTURE turkey", "Bird", Subject)) %>%
  mutate(Subject = ifelse(Subject == "WOODRAT white-footed", "Small Carnivore", Subject))

# pivot to shift behavioural counts into their own columns
non_puma_feeding_summary <- carnivore_class_feeding_times %>%
  group_by(Deployment.id, Subject) %>%
  summarize(Total.Feeding.Time = sum(Total.Feeding.Time), .groups = 'drop') %>%
  pivot_wider(names_from = Subject,
              values_from = Total.Feeding.Time,
              values_fill = 0) %>%
  select(order(colnames(.))) %>% # sort columns alphabetically
  select(
    Deployment.id,
    everything(),
    -Domestic,
    -`Non-Carnivore`,
    -Reptile,
    -Unknown,
    ) %>%
  rename(`Total Feeding Time Black Bears` = `Black Bear`) %>%
  rename(`Total Feeding Time Birds` = Bird) %>%
  rename(`Total Feeding Time Mesocarnivores` = Mesocarnivore) %>%
  rename(`Total Feeding Time Small Carnivores` = `Small Carnivore`)