library(dplyr)

# README
# Summarizes puma non-feeding behaviours by deployment
# Creates variable puma_non_feeding_behaviours
# BEFORE START:
# - boris_data_pruned.csv must be present
# - generate it with prune_boris_and_deployment.R

boris_data <- read.csv("boris_data_pruned.csv")

# Count up puma non-feeding behaviours
puma_non_feeding_behaviours <- boris_data %>%
  # consolidate puma and kitten behaviours
  mutate(Subject = ifelse(grepl("\\d+[MF]|PUMA.*", Subject), "Puma", Subject)) %>%
  mutate(Subject = ifelse(grepl("KITTEN[123]", Subject), "Kitten", Subject)) %>%
  # filter out non-puma behaviours
  filter(Subject == "Puma" | Subject == "Kitten") %>%
  # filter out unspecified or feeding behaviours
  filter(
    Behaviour != "feed"
    & Behaviour != "repeat feeding"
    & Behaviour != "other"
    & Behaviour != "unknown") %>%
  # sum behaviour counts
  group_by(Deployment.id, Subject, Behaviour) %>%
  summarise(
    Behaviour.Count = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  arrange(Deployment.id, Subject, Behaviour) %>%
  # pivot to shift behavioural counts into their own columns
  pivot_wider(names_from = c(Subject, Behaviour),
              values_from = Behaviour.Count,
              values_fill = 0,
              names_sep = " ",
              names_sort = TRUE) %>%
  group_by(Deployment.id) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(`Total Alert Behaviour Counts` = `Puma alert` + `Kitten alert`)
