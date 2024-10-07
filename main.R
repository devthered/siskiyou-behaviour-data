library(dplyr)
library(ggpie)
library(ggplot2)

source("datetime_string_fns.R")

#source("combine_boris_csvs.R")
#source("extract_mp4_metadata.R")
#source("setup_boris_data.R")
#source("prune_boris_and_deployments.R")

#####################
# Combine all stats #
#####################

source("summarize_subjects.R")
source("summarize_puma_feeding.R")
source("summarize_puma_non_feeding_behaviours.R") 

deployments_with_all_stats <- deployments %>%
  left_join(subjects_by_deployment, by = "Deployment.id") %>%
  left_join(puma_feeding_times, by = "Deployment.id") %>%
  left_join(puma_non_feeding_behaviours, by = "Deployment.id") %>%
  mutate(across(c(
    "Total Feeding Time (s) Puma", "Total Feeding Time (m)", "Species.richness", "N.kittens",
    "Total Feeding Time (s) Kitten", "Mean Feeding Bout Duration (s) Puma", "Mean Feeding Bout Duration (s) Kitten",
    "Kitten alert"                , "Kitten bed/rest"                     , "Kitten cache",
    "Kitten defend"               , "Kitten groom"                        , "Kitten mark",
    "Kitten play"                 , "Puma alert"                          , "Puma bed/rest",
    "Puma cache"                  , "Puma defend"                         , "Puma groom",
    "Puma mark"                   , "Puma play"                           , "Total Alert Behaviour Counts"            
  ), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(across(c(
    "Uncollared.puma.present", "Bear.present", "Bobcat.present",                         
    "Coyote.present", "Puma.present" 
  ), ~ ifelse(is.na(.), FALSE, .))) %>%
  mutate(across(c(
    "Pumas", "Species"
  ), ~ ifelse(is.null(.), character(0), .)))

write.csv(deployments_with_all_stats %>%
            mutate(Pumas = sapply(Pumas, function(x) paste(x, collapse = "; "))) %>%
            mutate(Species = sapply(Species, function(x) paste(x, collapse = "; "))),
          "deployments_with_all_stats.csv",
          row.names = FALSE)
  

############################################
# Make Shapefile with all deployment stats #
############################################

library(sf)
shapefile <- deployments_with_all_stats %>%
  select(-Pumas, -Species) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)
st_write(shapefile, "deployments.shp")
