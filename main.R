library(dplyr)

source("datetime_string_fns.R")

#source("combine_boris_csvs.R")
#source("extract_mp4_metadata.R")
#source("setup_boris_data.R")
#source("prune_boris_and_deployments.R")

#################
# Make Figures #
################

source("summarize_subjects.R")

# TODO: figure out when I should use this, and exclude deployments with no observations?
deployments_with_subjects <- deployments %>%
  right_join(subjects_by_deployment, by = "Deployment.id")

# TODO: and when I should use this, and include deployments with no observations?
subjects_by_deployment <- deployments %>%
 left_join(subjects_by_deployment, by = "Deployment.id")

########################
# 1. Puma Demographics #
########################

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

count_repro_status <- pumas %>%
  group_by(Reproductive.status) %>%
  summarize(Count = n(), .groups = 'drop')

ggplot(count_repro_status, aes(x = Reproductive.status, y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Pumas by reproductive status",
       x = "Reproductive status",
       y = "Number of puma sampled") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = Count), 
            position = position_stack(vjust = 1),
            vjust = -0.45,
            color = "black")


# TODO: make some graphs about presence of different species at kills



############################################
# Make Shapefile with all deployment stats #
############################################

# TODO: combine various summarize functions

# write deployment data to shapefile
# library(sf)
# deployments_for_printing <- deployment_data %>%
#   mutate(Pumas = paste(Pumas, collapse = "; ")) %>%
#   mutate(Species = paste(Species, collapse = "; "))
# shapefile <- deployments_for_printing %>%
#   st_as_sf(coords = c("Long", "Lat"), crs = 4326)
# st_write(shapefile, "deployments.shp")
