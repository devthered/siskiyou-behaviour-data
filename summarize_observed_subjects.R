library(dplyr)

summarize_observed_subjects <- function(boris_data) {
  # Aggregate subjects for each deployment
  deployment_subjects <- boris_data %>%
    group_by(Deployment.id) %>%
    summarize(Subjects = list(unique(Subject))) %>%
    ungroup()
  
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
  
  # create puma and species lists from subject lists
  # then calculate number of unique pumas and species richness
  deployment_subjects <- deployment_subjects %>%
    mutate(Pumas = lapply(Subjects, get_puma_list)) %>%
    mutate(Species = lapply(Subjects, get_species_list)) %>%
    # count values in list columns
    mutate(Kittens = sapply(Pumas, get_num_kittens)) %>%
    mutate(Uncollared.puma.present = sapply(Pumas, function(s) "PUMA uncollared" %in% s)) %>%
    mutate(Species.richness = sapply(Species, length)) %>%
    mutate(Bear.present = sapply(Species, function(s) "BEAR black" %in% s)) %>%
    # preferred order
    select(Deployment.id, everything(), -Subjects)
  
  return(deployment_subjects)
}