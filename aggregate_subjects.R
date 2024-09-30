library(dplyr)

aggregate_subjects <- function(boris_data) {
  # Aggregate subjects for each deployment
  deployment_subjects <- boris_data %>%
    group_by(Deployment.id) %>%
    summarize(Subjects = list(unique(Subject))) %>%
    ungroup()
  
  # regular expression to match puma names
  PUMA_REGEX <- "\\d+[MF]|PUMA.*|KITTEN[123]"
  
  # function to get list of puma individuals from subjects list
  get_puma_list <- function(subjects) {
    puma_indices <- grep(PUMA_REGEX, subjects)
    subjects[puma_indices]
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
    
    subjects
  }
  
  # create puma and species lists from subject lists
  # then calculate number of unique pumas and species richness
  deployment_subjects <- deployment_subjects %>%
    mutate(Pumas = lapply(Subjects, get_puma_list)) %>%
    mutate(Species = lapply(Subjects, get_species_list)) %>%
    select(-Subjects) # remove Subjects column as it is not needed
  
  return(deployment_subjects)
}