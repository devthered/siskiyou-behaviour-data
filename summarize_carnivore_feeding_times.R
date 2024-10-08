# README
# Summarizes puma feeding times by deployment
# Creates variable puma_feeding_times
# BEFORE START:
# - boris_data_pruned.csv deployment_data_prined.csv must be present
# - generate these with prune_boris_and_deployment.R

boris_data <- read.csv("boris_data_pruned.csv")

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
  mutate(Feeding.Time.Per.Individual = ifelse(is.na(Modifiers), Behaviour.duration, as.numeric(Modifiers) * Behaviour.duration)) %>%
  # sum behaviour counts and durations for each subject and behaviour
  group_by(Deployment.id, Subject) %>%
  summarise(
    Total.Feeding.Time.s = sum(Feeding.Time.Per.Individual),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  arrange(Deployment.id, Subject) %>%
  mutate(Subject = ifelse(Subject == "BEAR black", "Black Bear", Subject)) %>%
  mutate(Subject = ifelse(Subject == "BIRD (other)", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "BOBCAT", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "CHIPMUNK", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "COW domestic", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "COYOTE", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "DOG domestic", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "EAGLE bald", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "EAGLE golden", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "FISHER", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "FLICKER northern", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "FOX gray", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "GOSHAWK northern", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "GROUSE", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "HAWK red-tailed", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "JAY scrub", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "JAY Stellar's", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "LAGOMORPH", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "MAGPIE", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "MOUSE", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "RACCOON", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "RAVEN common", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "REPTILE", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "ROBIN American", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SKUNK striped", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SKUNK western spotted", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SQUIRREL California ground", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SQUIRREL Douglas", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SQUIRREL golden-mantled ground", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "SQUIRREL western gray", "Non-Carnivore", Subject)) %>%
  mutate(Subject = ifelse(Subject == "THRUSH varied", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "UNKNOWN", "Unknown", Subject)) %>%
  mutate(Subject = ifelse(Subject == "VULTURE turkey", "Scavenger", Subject)) %>%
  mutate(Subject = ifelse(Subject == "WOODRAT white-footed", "Scavenger", Subject)) %>%
  group_by(Deployment.id, Subject) %>%
  summarize(Total.Feeding.Time.s = sum(Total.Feeding.Time.s), .groups = 'drop') %>%
  pivot_wider(names_from = Subject,
              values_from = Total.Feeding.Time.s,
              values_fill = 0,
              names_sort = TRUE) %>%
  mutate(`Total Feeding Time (m) Black Bears` = `Black Bear` / 60) %>%
  mutate(`Total Feeding Time (m) Scavengers` = Scavenger / 60) %>%
  select(Deployment.id,
         everything(),
         -`Black Bear`,
         -`Non-Carnivore`,
         -Scavenger)



