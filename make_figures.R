library(dplyr)
library(tidyr)
library(ggpie)
library(ggplot2)
library(scales)

source("datetime_string_fns.R")

# Running these produces the boris (behaviour) and deployment data for this file to work with
# Some take a long time and require external information, so outputs should be stored as CSVs
#source("combine_boris_csvs.R")
#source("extract_mp4_metadata.R")
#source("setup_boris_data.R")
#source("prune_boris_and_deployments.R")

# README
# This file generates figures for the Data Summary Report.
# Requires outputs from a number of files, listed above

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
    
# Create stacked bar chart
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
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_blank()) +
  geom_text(aes(label = Deployments), 
            position = position_stack(vjust = 1),
            vjust = -0.45,
            color = "black")


###########################################
# 2. Pie and Bar Chart: Prey Demographics #
###########################################

source("summarize_subjects.R")

species_list <- c("Mule Deer", "Elk", "Unknown")
age_list <- c("Calf", "Fawn", "Yearling", "Adult", "Unknown")
sex_list <- c("Unknown", "Male", "Female")

prey_demographics <- deployments %>%
  select(Deployment.id, Puma.name, Carcass.species, Carcass.age, Carcass.sex) %>%
  arrange(Carcass.species, Carcass.age, Carcass.sex)

# Species pie chart
ggpie(data = prey_demographics, 
      group_key = "Carcass.species", 
      count_type = "full",
      border_size = 0.5,
      fill_color = c("Mule Deer" = "#FF8C00", "Elk" = "#2E8B57", "Unknown" = "grey" ),
      label_color = "black",
      label_info = "count",
      label_type = "horizon",
      label_split = NULL,
      label_size = 4.5, 
      label_pos = "in",
      label_threshold = 10) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 0.625),
        legend.justification = c("right", "top"),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_blank())


prey_demographics_summary <- prey_demographics %>%
  group_by(Carcass.species, Carcass.age, Carcass.sex) %>%
  summarize(Deployments = n(), .groups = 'drop')

# Age and sex stacked bar chart for Mule Deer
ggplot(prey_demographics_summary %>% filter(Carcass.species == "Mule Deer"), aes(x = Carcass.age, y = Deployments, fill = Carcass.sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Observed Kills by Prey Age and Sex (Mule Deer)",
       x = "Age Class",
       y = "Number of Observed Kills",
       fill = "Sex") +
  scale_fill_manual(values = c("Female" = "#FF8C00", "Male" = "#2E8B57", "Unknown" = "grey" )) +
  theme_minimal() +
  theme(legend.justification = c("right", "top"),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 10)) +
  geom_text(aes(label = ifelse(Carcass.sex == "Unknown", "", Deployments)),
            position = position_stack(vjust = 1),
            vjust = -0.5,
            color = "black")

# Age and sex stacked bar chart for Elk
ggplot(prey_demographics_summary %>% filter(Carcass.species == "Elk"), aes(x = Carcass.age, y = Deployments, fill = Carcass.sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Observed Kills by Prey Age and Sex (Elk)",
       x = "Age Class",
       y = "Number of Observed Kills",
       fill = "Sex") +
  scale_fill_manual(values = c("Female" = "#FF8C00", "Male" = "#2E8B57", "Unknown" = "grey" )) +
  theme_minimal() +
  theme(legend.justification = c("right", "top"),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 10)) +
  geom_text(aes(label = ifelse(Carcass.sex == "Unknown", "", Deployments)),
            position = position_stack(vjust = 1),
            vjust = -0.5,
            color = "black")

##############################################
# 3. Bar Charts: Scavenger species breakdown #
##############################################

deployments_with_subjects <- deployments %>%
  left_join(subjects_by_deployment, by = "Deployment.id")

scavenger_species_breakdown <- deployments_with_subjects %>%
  select(Deployment.id, Species) %>%
  unnest(Species) %>%
  mutate(Species = unlist(Species)) %>%
  filter(Species != "UNKNOWN")

species_classes <- list(
  "Large Carnivore" = c(
    "PUMA", "BEAR black"
  ),
  Mesocarnivore = c(
    "COYOTE", "BOBCAT", "FISHER", "FOX gray"
  ),
  Omnivore = c(
    "RACCOON", "RAT", "MOUSE", "SKUNK striped", "SKUNK western spotted"
  ),
  Bird = c(
    "BIRD (other)", "EAGLE bald", "EAGLE golden", 
    "FLICKER northern", "GOSHAWK northern", "GROUSE", 
    "HAWK red-tailed", "JAY scrub", "JAY Stellar's", 
    "MAGPIE", "ROBIN American", "RAVEN common", 
    "THRUSH varied", "VULTURE turkey", "WOODPECKER"
  ),
  Herbivore = c(
    "CHIPMUNK", "DEER Mule", "ELK", "LAGOMORPH",
    "SQUIRREL California ground", "SQUIRREL Douglas", 
    "SQUIRREL golden-mantled ground", "SQUIRREL western gray",
    "WOODRAT white-footed"
  ),
  Reptile = c("REPTILE"),
  "Human / Domestic" = c(
    "HUMAN", "COW domestic", "DOG domestic"
  ),
  Other = c("UNKNOWN", "OTHER")
)

scavenger_species_breakdown <- scavenger_species_breakdown %>%
  mutate(Class = unlist(lapply(Species, function(x) {
    # Find the class based on species
    class <- names(species_classes)[sapply(species_classes, function(species) x %in% species)]
    if (length(class) == 0) return(NA) else return(class)
  })))

# Count abundance of each species
species_counts <- scavenger_species_breakdown %>%
  count(Species, Class) %>%
  mutate(Percent = n / 121.0)

# Create a custom order for classes
class_order <- names(species_classes)
species_order <- unlist(species_classes)

# Convert Class to a factor with the specified order
species_counts$Class <- factor(species_counts$Class, levels = class_order)

# Create a combined factor for ordering species within their class
species_counts$Species <- factor(species_counts$Species, levels = rev(species_order))

# Create horizontal bar chart with grouped bars by Class
ggplot(species_counts, aes(x = Species, y = Percent, fill = Class)) +
  geom_bar(stat = 'identity') +
  coord_flip() +  # Make it horizontal
  labs(x = 'Species',
       y = 'Percent of puma kills where detected',
       title = 'Species presence at monitored puma kills (n = 121)') +
  scale_fill_manual(values = c(
    "Large Carnivore" = "darkred", 
    "Mesocarnivore" = "lightcoral", 
    "Omnivore" = "orange", 
    "Bird" = "skyblue", 
    "Herbivore" = "lightgreen", 
    "Reptile" = "darkgreen", 
    "Human / Domestic" = "gray", 
    "Other" = "darkgray"
  )) +
  scale_y_continuous(labels = label_percent(scale = 100),
                     limits = c(0, 0.86),
                     expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.9),
        legend.justification = c("right", "top"),) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 1),
            hjust = -0.25,
            color = "black")

class_counts = scavenger_species_breakdown %>%
  group_by(Deployment.id, Class) %>%
  summarize(.groups = 'drop') %>%
  count(Class)


############################################
# 4 Combine all stats, including behaviour #
############################################

source("summarize_puma_feeding.R")
source("summarize_puma_non_feeding_behaviours.R")

concat <- function(x) {
  ifelse(identical(x, character(0)),
         "",
         do.call(paste, c(as.list(x), sep="; ")))
}

deployments_all_stats <- deployments %>%
  left_join(subjects_by_deployment,
            by = "Deployment.id") %>%
  left_join(puma_non_feeding_behaviours,
            by = "Deployment.id") %>%
  left_join(puma_feeding_times,
            by = "Deployment.id") %>%
  mutate(Pumas = sapply(Pumas, concat)) %>%
  mutate(Species = sapply(Species, concat)) %>%
  mutate(across(c("Species.richness",                       "N.kittens",                      "Total Alert Behaviour Counts", 
                  "Total Feeding Time (s) Adult Pumas",     "Total Feeding Time (s) Kittens", "Mean Feeding Bout Duration (s) Adult Pumas",   
                  "Mean Feeding Bout Duration (s) Kittens", "Total Feeding Time (m) Pumas"),
                function(x) ifelse(is.na(x), 0, x))) %>%
  mutate(across(c("Coyote.present", "Puma.present", "Uncollared.puma.present", "Bear.present", "Bobcat.present"),
                function(x) ifelse(is.na(x), FALSE, x)))

write.csv(deployments_all_stats, "deployments_all_stats.csv", row.names = FALSE)


########################################
# 5. Box Plots: Puma feeding behaviour #
########################################

ggplot(deployments_all_stats %>%
         filter(`Total Feeding Time (m) Pumas` > 0),
       aes(x=Bear.present, y=`Total Feeding Time (m) Pumas`)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  geom_point(aes(color = Bear.present), 
             position=position_jitterdodge(jitter.width = NULL,
                                           jitter.height = 0,
                                           dodge.width = 0.75,
                                           seed = NA)) +
  labs(x = 'Black bear presence', y = 'Total Puma Feeding Time (minutes)') +
  scale_x_discrete(labels = c("FALSE" = "Absent", "TRUE" = "Present")) +
  scale_color_manual(values = c("FALSE" = "#FF8C00", "TRUE" = "#2E8B57")) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(deployments_all_stats %>%
         filter(`Mean Feeding Bout Duration (s) Adult Pumas` > 0),
       aes(x=Bear.present, y=`Mean Feeding Bout Duration (s) Adult Pumas`)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  geom_point(aes(color = Bear.present), 
             position=position_jitterdodge(jitter.width = NULL,
                                           jitter.height = 0,
                                           dodge.width = 0.75,
                                           seed = NA)) +
  labs(x = 'Black bear presence', y = 'Mean Adult Puma Feeding Bout Duration (seconds)') +
  scale_x_discrete(labels = c("FALSE" = "Absent", "TRUE" = "Present")) +
  scale_color_manual(values = c("FALSE" = "#FF8C00", "TRUE" = "#2E8B57")) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(deployments_all_stats %>%
         filter(`Total Alert Behaviour Counts` > 0),
       aes(x=Bear.present, y=`Total Alert Behaviour Counts`)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  geom_point(aes(color = Bear.present), 
             position=position_jitterdodge(jitter.width = NULL,
                                           jitter.height = 0,
                                           dodge.width = 0.75,
                                           seed = NA)) +
  labs(x = 'Black bear presence', y = 'Total Puma Alert Behaviour Counts') +
  scale_x_discrete(labels = c("FALSE" = "Absent", "TRUE" = "Present")) +
  scale_color_manual(values = c("FALSE" = "#FF8C00", "TRUE" = "#2E8B57")) +
  theme_minimal() +
  theme(legend.position = "none")


##################################
# 5. Box Plots: Species richness #
##################################

puma_order = c("1F", "8F", "9F", "13F", "3M", "4M", "5M", "7M", "12M", "14M")
ggplot(deployments_all_stats %>%
         filter(Species.richness > 0) %>%
         mutate(Puma.name = factor(Puma.name, levels = puma_order)),
       aes(x=Puma.name, y=Species.richness)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  geom_point(aes(color = Puma.name), 
             position=position_jitterdodge(jitter.width = 3,
                                           jitter.height = 0,
                                           dodge.width = 0.75,
                                           seed = NA)) +
  labs(x = 'Puma', y = 'Species richness at carcass') +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(deployments_all_stats %>%
         filter(Species.richness > 0) %>%
         mutate(Species.richness.non.bear = ifelse(Bear.present, Species.richness - 1, Species.richness)),
       aes(x=Bear.present, y=Species.richness.non.bear)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  geom_point(aes(color = Bear.present), 
             position=position_jitterdodge(jitter.width = NULL,
                                           jitter.height = 0,
                                           dodge.width = 0.75,
                                           seed = NA)) +
  labs(x = 'Black bear presence', y = 'Species richness at carcass (excluding bears)') +
  scale_x_discrete(labels = c("FALSE" = "Absent", "TRUE" = "Present")) +
  scale_color_manual(values = c("FALSE" = "#FF8C00", "TRUE" = "#2E8B57")) +
  theme_minimal() +
  theme(legend.position = "none")


  

##########################
# 6. Export to shapefile #
##########################

# library(sf)
# shapefile <- deployments_summary_stats %>%
#   st_as_sf(coords = c("Long", "Lat"), crs = 4326)
# st_write(shapefile, "deployments.shp")


