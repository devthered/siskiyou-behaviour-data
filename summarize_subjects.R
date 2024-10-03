library(dplyr)

# README
# Summarizes subjects observed by deployment.
# Creates variable subjects_by_deployment
# BEFORE START:
# - boris_data_pruned.csv and deployment_data_pruned.csv must be present
# - generate these with prine_boris_and_deployment.R

boris_data <- read.csv("boris_data_pruned.csv")
deployments <- read.csv("deployment_data_pruned.csv")

#############################
# String matching functions #
#############################

# function to get count of puma kittens from list of subjects
get_num_kittens <- function(subjects) {
  puma_indices <- grep("KITTEN[123]", subjects)
  length(subjects[puma_indices])
}

# regular expression to match any puma names
PUMA_REGEX <- "\\d+[MF]|PUMA.*|KITTEN[123]"

# function to get list of puma individuals from subjects list
get_puma_list <- function(subjects) {
  puma_indices <- grep(PUMA_REGEX, subjects)
  pumas <- subjects[puma_indices]
  subjects <- lapply(pumas, function(s) gsub("\\s+$", "", s)) # remove whitespace
}

# function to get list of unique species from subject list
get_species_list <- function(subjects) {
  # remove UNKNOWN from list
  # subjects <- subjects[subjects != "UNKNOWN"]
  
  # consolidate different puma subjects
  puma_indices <- grep(PUMA_REGEX, subjects)
  if (length(puma_indices) > 0) {
    subjects <- subjects[-puma_indices] # removes matching strings from list
    subjects <- c(subjects, "PUMA") # adds a single "PUMA" entry
  }
  
  subjects <- lapply(subjects, function(s) gsub("\\s+$", "", s)) # remove whitespace
  subjects
}


##########################################
# Aggregate subjects for each deployment #
##########################################

# Aggregate subjects for each deployment
subjects_by_deployment <- boris_data %>%
  group_by(Deployment.id) %>%
  summarize(Subjects = list(unique(Subject))) %>%
  ungroup()

# create puma and species lists from subject lists
# then calculate number of unique pumas and species richness
subjects_by_deployment <- subjects_by_deployment %>%
  mutate(Pumas = lapply(Subjects, get_puma_list)) %>%
  mutate(Species = lapply(Subjects, get_species_list)) %>%
  mutate(Species.richness = sapply(Species, length)) %>%
  mutate(N.kittens = sapply(Pumas, get_num_kittens)) %>%
  mutate(Uncollared.puma.present = sapply(Pumas, function(s) "PUMA uncollared" %in% s)) %>%
  mutate(Bear.present = sapply(Species, function(s) "BEAR black" %in% s)) %>%
  # preferred order
  select(Deployment.id, everything(), -Subjects)
  