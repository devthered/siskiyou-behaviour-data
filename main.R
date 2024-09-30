library(dplyr)

source("aggregate_subjects.R")

# Read in all boris data and clean up
boris_data <- read.csv("combined_boris_data.csv")
boris_data <- boris_data %>%
  # extract Deployment.id (first two parts of Observation.id)
  mutate(Deployment.id = 
           sapply(strsplit(Observation.id, "_"), 
                  function(x) paste(x[1:2], collapse = "_"))) %>%
  # remove unnecessary columns
  select(Deployment.id, everything(), -Behavioral.category, -FPS)

deployment_subjects = aggregate_subjects(boris_data)