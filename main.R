library(dplyr)
library(tidyr)

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
  select(Deployment.id, everything(), -Behavioral.category, -FPS) %>%
  # NZ spelling
  rename(Behaviour = Behavior)

source("summarize_by_deployments.R")
deployment_data <- summarize_by_deployments(boris_data)

# Summarize data by named puma
named_puma_feeding_data <- boris_data %>%
  filter(grepl("\\d+[MF]", Subject)) %>%
  filter(Behaviour == "feed") %>%
  group_by(Subject) %>%
  summarize(
    Total.Feeding.Time.s = sum(Duration..s.),
    Mean.Feeding.Bout.Duration.s = sum(Duration..s.),
    Std.Dev.Feeding.Bout.Duration.s = sum(Duration..s.),
  ) %>%
  ungroup()

# Other data by named puma, from deployment_data
named_puma_data <- deployment_data %>%
  group_by(Puma.id) %>%
  summarize(
    Deployments = n(),
    Deployments.With.Uncollared.Puma = sum(Uncollared.puma.present == TRUE),
    Deployments.With.Bear = sum(Bear.present == TRUE),
    First.Deployment.Setup = first(Setup.date),
    Last.Deployment.Pickup = last(na.omit(Pickup.date)),
    Kittens = max(Kittens),
    Kitten.alert.count = sum(`Kitten alert`),
    Kitten.cache.count = sum(`Kitten cache`),
    Puma.alert.count = sum(`Puma alert`),
    Puma.cache.count = sum(`Puma cache`)
  ) %>%
  ungroup()

# Join all named puma data together
named_puma_data <- full_join(
  named_puma_data,
  named_puma_feeding_data,
  by = c("Puma.id" = "Subject")
)

# Replace NA values with 0
named_puma_data <- named_puma_data %>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))      
