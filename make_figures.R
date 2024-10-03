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


################################################
# 2. Pie charts: summarize deployment subjects #
################################################

