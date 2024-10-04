library(ggplot2)
library(dplyr)

# README
# This file generates figures for the Data Summary Report.
# Requires outputs from a number of files

#####################################
# 1. Barchart: Deployments per puma #
#####################################

observations_by_puma <- read.csv("deployment_data_for_barchart.csv") %>%
  mutate(Behaviours.observed = factor(N.observations > 0,
                                      levels = c(FALSE, TRUE),
                                      labels = c("No behaviours observed", "Behaviours observed"))) %>%
  group_by(Puma.name, Behaviours.observed) %>%
  summarise(Deployments = n(), .groups = 'drop') %>%
  mutate(Sex = gsub("[0-9]", "", Puma.name)) %>%
  mutate(Id = as.integer(gsub("[MF]", "", Puma.name))) %>%
  arrange(Sex, Id) %>%
  mutate(Puma.name = factor(Puma.name, levels = unique(Puma.name)))
    
# Create the stacked bar chart
ggplot(observations_by_puma, aes(x = Puma.name, y = Deployments, fill = Behaviours.observed)) +
  geom_bar(stat = "identity") +
  labs(title = "Observed Kills per Puma",
       x = "Puma Name (F = female, M = male)",
       y = "Number of Observed Kills") +
  scale_fill_manual(values = c("No behaviours observed" = "grey", "Behaviours observed" = "#2E8B57")) +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.9),
        legend.justification = c("right", "top"),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_blank()) +
  geom_text(aes(label = Deployments), 
            position = position_stack(vjust = 1),
            vjust = -0.45,
            color = "black")


###################################
# 2. Pie Chart: Puma Demographics #
###################################

deployments_with_subjects <- deployments %>%
  right_join(subjects_by_deployment, by = "Deployment.id")

pumas <- deployments_with_subjects %>%
  group_by(Puma.name) %>%
  summarize(N.kittens = max(N.kittens), .groups = 'drop') %>%
  mutate(Reproductive.status = ifelse(
    grepl("M", Puma.name),
    "Solitary Male",
    ifelse(
      N.kittens == 0,
      "Solitary Female",
      "Female with Kittens"
    )))

ggpie(data = pumas, 
      group_key = "Reproductive.status", 
      count_type = "full",
      fill_color = c("Solitary Male" = "#FF8C00", "Solitary Female" = "#1D5C3F", "Female with Kittens" = "#2E8B57"),
      label_color = "white",
      label_info = "count",
      label_type = "horizon",
      label_split = NULL,
      label_size = 4.5, 
      label_pos = "in") +
  theme(legend.title = element_blank())


####################################
# 3. Pie Charts: Prey Demographics #
####################################

prey_demographics <- deployments %>%
  select(Deployment.id, Puma.name, Carcass.species, Carcass.age.sex) %>%
  mutate(Carcass.species = ifelse(tolower(trimws(Carcass.species)) == "mule deer", "Mule Deer", Carcass.species)) %>%
  mutate(Carcass.age.sex = tolower(trimws(Carcass.age.sex)))

ggpie(data = prey_demographics, 
      group_key = "Carcass.species", 
      count_type = "full",
      border_size = 0.5,
      fill_color = c("Mule Deer" = "#FF8C00", "Elk" = "#1D5C3F", "NA" ),
      label_color = "black",
      label_info = "count",
      label_type = "horizon",
      label_split = NULL,
      label_size = 4.5, 
      label_pos = "in",
      label_threshold = 10) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.5),
        legend.title = element_blank())

ggpie(data = prey_demographics %>% filter(Carcass.species == "Mule Deer"), 
      group_key = "Carcass.age.sex", 
      count_type = "full",
      border_size = 0.5,
      #fill_color = c("Mule Deer" = "#FF8C00", "Elk" = "#1D5C3F", "NA" ),
      label_color = "black",
      label_info = "count",
      label_type = "horizon",
      label_split = NULL,
      label_size = 4.5, 
      label_pos = "in",
      label_threshold = 10) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.5),
        legend.title = element_blank())
