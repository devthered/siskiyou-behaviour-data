library(dplyr)

# README
# Summarizes puma feeding times by deployment
# Creates variable puma_feeding_times
# BEFORE START:
# - boris_data_pruned.csv must be present
# - generate these with prune_boris_and_deployment.R

boris_data <- read.csv("boris_data_pruned.csv")

puma_feeding_times <- boris_data %>%
  # consolidate puma and kitten behaviours
  mutate(Subject = ifelse(grepl("\\d+[MF]|PUMA.*", Subject), "Puma", Subject)) %>%
  mutate(Subject = ifelse(grepl("KITTEN[123]", Subject), "Kitten", Subject)) %>%
  # filter out non-puma behaviours
  filter(Subject == "Puma" | Subject == "Kitten") %>%
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
  mutate(`Total Feeding Time (m)` = (`Total Feeding Time (s) Puma` + `Total Feeding Time (s) Kitten`) / 60)
