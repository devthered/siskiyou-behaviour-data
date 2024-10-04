library(dplyr)
library(ggpie)
library(ggplot2)

source("datetime_string_fns.R")

#source("combine_boris_csvs.R")
#source("extract_mp4_metadata.R")
#source("setup_boris_data.R")
#source("prune_boris_and_deployments.R")

#################
# Make Figures #
################

source("summarize_subjects.R")

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
