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