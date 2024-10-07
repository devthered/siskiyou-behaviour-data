library(dplyr)
library(ggpie)
library(ggplot2)

source("datetime_string_fns.R")

#source("combine_boris_csvs.R")
#source("extract_mp4_metadata.R")
#source("setup_boris_data.R")
#source("prune_boris_and_deployments.R")

############################################
# Make Shapefile with all deployment stats #
############################################

source("summarize_subjects.R")

library(sf)
shapefile <- deployments %>%
  right_join(subjects_by_deployment, by = "Deployment.id") %>%
  select(-Pumas, -Species) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)
st_write(shapefile, "deployments.shp")
