summarize_carnivore_feeding_times <- function(boris_data) {
  
  # regular expression to match puma names
  PUMA_REGEX <- "\\d+[MF]|PUMA.*|KITTEN[123]"
  
  # Sum up feeding times for all non-puma individuals
  # NOTE: May overinflate feeding times for group feeders, 
  # as it assumes all group members are feeding simultaneously for 100% of recorded time.
  non_puma_feeding_times <- boris_data %>%
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
      Total.Feeding.Time.s = sum(Feeding.Time.Per.Individual),
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
    summarize(Total.Feeding.Time.s = sum(Total.Feeding.Time.s), .groups = 'drop') %>%
    pivot_wider(names_from = Subject,
                values_from = Total.Feeding.Time.s,
                values_fill = 0,
                names_sort = TRUE) %>%
    select(
      Deployment.id,
      everything(),
      -Domestic,
      -`Non-Carnivore`,
      -Reptile,
      -Unknown,
    ) %>%
    rename(`Total Feeding Time (s) Black Bears` = `Black Bear`) %>%
    rename(`Total Feeding Time (s) Birds` = Bird) %>%
    rename(`Total Feeding Time (s) Mesocarnivores` = Mesocarnivore) %>%
    rename(`Total Feeding Time (s) Small Carnivores` = `Small Carnivore`)
}