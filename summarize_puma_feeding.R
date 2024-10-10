library(dplyr)

# README
# Summarizes puma feeding times by deployment
# Creates variable puma_feeding_times
# BEFORE START:
# - boris_data_pruned.csv deployment_data_prined.csv must be present
# - generate these with prune_boris_and_deployment.R

boris_data <- read.csv("boris_data_pruned.csv")

puma_feeding_times <- boris_data %>%
  # consolidate puma and kitten behaviours
  mutate(Subject = ifelse(grepl("\\d+[MF]|PUMA.*", Subject), "Adult Pumas", Subject)) %>%
  mutate(Subject = ifelse(grepl("KITTEN[123]", Subject), "Kittens", Subject)) %>%
  # filter out non-puma behaviours
  filter(Subject == "Adult Pumas" | Subject == "Kittens") %>%
  # interested only in feeding
  filter(Behaviour == "feed") %>%
  # get statistics on feeding times
  group_by(Deployment.id, Subject) %>%
  summarise(
    `Total Feeding Time (s)` = sum(Behaviour.duration),
    `Mean Feeding Bout Duration (s)` = mean(Behaviour.duration),
    `Std Dev Feeding Bout Duration (s)` = sd(Behaviour.duration),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  arrange(Deployment.id, Subject) %>%
  pivot_wider(names_from = Subject,
              values_from = c(
                "Total Feeding Time (s)",
                "Mean Feeding Bout Duration (s)",
                "Std Dev Feeding Bout Duration (s)"),
              values_fill = 0,
              names_sep = " ") %>%
  mutate(`Total Feeding Time (m) Pumas` = (`Total Feeding Time (s) Adult Pumas` + `Total Feeding Time (s) Kittens`) / 60)
  
