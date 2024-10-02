library(dplyr)

#source("combine_boris_csvs.R")
#source("extract_mp4_metadata.R")
#source("setup_boris_data.R")
boris_data <- read.csv("boris_tidy.csv")

############################################################
# Summarize Deployment Data: NOT INCORPORATING VIDEO TIMES #
############################################################

source("summarize_by_deployments.R")

# write deployment data to shapefile
# library(sf)
# deployments_for_printing <- deployment_data %>%
#   mutate(Pumas = paste(Pumas, collapse = "; ")) %>%
#   mutate(Species = paste(Species, collapse = "; "))
# shapefile <- deployments_for_printing %>%
#   st_as_sf(coords = c("Long", "Lat"), crs = 4326)
# st_write(shapefile, "deployments.shp")

# Summarize data by named puma
named_puma_feeding_data <- boris_data %>%
  filter(grepl("\\d+[MF]", Subject)) %>%
  filter(Behaviour == "feed") %>%
  group_by(Subject) %>%
  summarize(
    Total.Feeding.Time.s = sum(Duration..s.),
    Mean.Feeding.Bout.Duration.s = sum(Duration..s.),
    Std.Dev.Feeding.Bout.Duration.s = sum(Duration..s.),
  ) %>%
  ungroup()

# Other data by named puma, from deployment_data
named_puma_data <- deployment_data %>%
  group_by(Puma.id) %>%
  summarize(
    Deployments = n(),
    Deployments.With.Uncollared.Puma = sum(Uncollared.puma.present == TRUE),
    Deployments.With.Bear = sum(Bear.present == TRUE),
    First.Deployment.Setup = first(Setup.date),
    Last.Deployment.Pickup = last(na.omit(Pickup.date)),
    Kittens = max(Kittens),
    Kitten.alert.count = sum(`Kitten alert`),
    Kitten.cache.count = sum(`Kitten cache`),
    Puma.alert.count = sum(`Puma alert`),
    Puma.cache.count = sum(`Puma cache`)
  ) %>%
  ungroup()

# Join all named puma data together
named_puma_data <- full_join(named_puma_data,
                             named_puma_feeding_data,
                             by = c("Puma.id" = "Subject"))

# Replace NA values with 0
named_puma_data <- named_puma_data %>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))
