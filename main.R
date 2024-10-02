library(dplyr)
library(tidyr)
library(sf)
library(stringr)

# Read in all boris data and clean up
boris_data <- read.csv("combined_boris_data.csv")
boris_data <- boris_data %>%
  # extract Deployment.id (first two parts of Observation.id)
  mutate(Deployment.id =
           sapply(strsplit(Observation.id, "_"), function(x)
             paste(x[1:2], collapse = "_"))) %>%
  select(-Observation.id) %>%
  # fix some errors in the IDs
  mutate(Deployment.id = ifelse(Deployment.id == "1F_30Sept17", "1F_30Sep17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "1F_7Octo17", "1F_7Oct17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "1f_15Oct17", "1F_15Oct17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "1F_4Nov171", "1F_4Nov17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "1F_Apr19", "1F_9Apr19", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "5M_5Sept17", "5M_5Sep17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "5_22Nov17", "5M_22Nov17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "5M_22Nov17)B14", "5M_22Nov17", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "7M_5Feb18", "7M_15Feb18", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "8F_Mar18", "8F_28Mar18", Deployment.id)) %>%
  mutate(Deployment.id = ifelse(Deployment.id == "8F_14Dec19", "8F_14Dec18", Deployment.id)) %>%
  # remove unnecessary columns
  select(Deployment.id, everything(), -Behavioral.category, -FPS) %>%
  # NZ spelling
  rename(Behaviour = Behavior)

###########################
# Incorporate video times #
###########################

# 1. Get unique boris filenames and expected paths
##################################################

DEPLOYMENT_ID_REGEX <- "[0-9]{1,2}[MF]_[0-9]{1,2}(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)(17|18|19|20)"
FILENAME_REGEX <- "RCNX[0-9]{4}.MP4"

# Extract expected file path from filepath.
# Expected filepath is Deployment.id/Filename
# Many filepaths do not match this exactly, and these I need to adjust to correctly match up with video metadata
boris_data <- boris_data %>%
  mutate(Deployment.id.from.filepath = str_extract(Media.file, DEPLOYMENT_ID_REGEX)) %>%
  mutate(Filename = str_extract(Media.file, FILENAME_REGEX)) %>%
  mutate(Expected.path = paste(Deployment.id.from.filepath, Filename, sep = "/")) %>%
  mutate(Containing.folder = str_remove(Media.file, "/[^/]+$")) %>%
  arrange(Expected.path)

# Confirm that extraction works and deployment id from filepath matches one from Observation.id
boris_no_match <- boris_data %>% filter(Deployment.id != Deployment.id.from.filepath)
boris_no_id <- boris_data %>% filter(is.na(Deployment.id))
boris_no_id_from_filename <- boris_data %>% filter(is.na(Deployment.id.from.filepath))
boris_no_filename <- boris_data %>% filter(is.na(Filename))
if (nrow(boris_no_match) != 0
    || nrow(boris_no_id) != 0
    || nrow(boris_no_id_from_filename) != 0
    || nrow(boris_no_filename) != 0) {
  stop("Error: boris data should all have correct Deployment ID and Filename")
}
remove(boris_no_match)
remove(boris_no_id)
remove(boris_no_id_from_filename)
remove(boris_no_filename)
# --> confirmed that this is correct with our data, after Deployment.id corrections above

# Get unique set of abosolute paths to original boris media and matches to expected paths
boris_media_files <- boris_data %>%
  group_by(Media.file) %>%
  summarize(
    Expected.path = first(Expected.path),
    Num.expected.paths = sapply(unique(Expected.path), length)
  ) %>%
  ungroup()

# Ensure that there are no media files with no different expected paths
if (any(boris_media_files$Num.expected.paths > 1)) {
  stop("Error: there should only be one expected path per Media.file")
}
boris_media_files <- boris_media_files %>%
  select(-Num.expected.paths)
# --> confirmed this is correct for our data

# Get boris data with the same expected path but multiple actual source files
boris_multiple_expected_paths <- boris_data %>%
  group_by(Expected.path) %>%
  filter(length(unique(Media.file)) > 1) %>%
  ungroup()

# 2. Get video filenames and expected paths
###########################################

# Read in video metadata (CSV was produced from actual video files on hard disk)
# Also generate the same expected filepath from these filenames
video_metadata <- read.csv("video_metadata.csv") %>%
  mutate(Fixed.path = gsub("April", "Apr", SourceFile)) %>% # fix error in some names so regex matches
  mutate(Deployment.id = str_extract(Fixed.path, DEPLOYMENT_ID_REGEX)) %>%
  mutate(Filename = str_extract(Fixed.path, FILENAME_REGEX)) %>%
  mutate(Expected.path = paste(Deployment.id, Filename, sep = "/")) %>%
  mutate(Truncated.fixed.path = sub("/Volumes/One Touch/Siskiyou/CamTrap/XR6/", "", Fixed.path)) %>%
  mutate(Containing.folder = str_remove(Truncated.fixed.path, "/[^/]+$")) %>%
  arrange(Expected.path)

# Confirm extraction works and no entries are missing IDs or filenames
videos_no_id_from_filename <- video_metadata %>% filter(is.na(Deployment.id))
videos_no_filename <- video_metadata %>% filter(is.na(Filename))
if (nrow(videos_no_filename) != 0
    || nrow(videos_no_id_from_filename) != 0) {
  stop("Error: video data should all have Deployment ID and Filename")
}
remove(videos_no_id_from_filename)
remove(videos_no_filename)
# --> confirmed that this is correct with our data, after fixing "April" -> "Apr"

# Get videos where the actual path does not equal the expected path
videos_path_unexpected <- video_metadata %>%
  filter(Truncated.fixed.path != Expected.path) %>%
  group_by(Deployment.id, Containing.folder) %>%
  summarize(.groups = 'drop')

# 3. Investigate funny paths
############################

# get boris folders for videos with unexpected paths on disk
boris_unexpected_path_videos <- boris_data %>%
  filter(Deployment.id %in% videos_path_unexpected$Deployment.id)
  group_by(Deployment.id, Containing.folder) %>%
  summarize(.groups = 'drop') %>%

# get metadata for videos with duplicate paths in boris
video_metadata_boris_multiples <- video_metadata %>% 
  filter(Expected.path %in% boris_multiple_expected_paths$Expected.path) %>%
  select(Expected.path, SourceFile)

# The cleaning below was based on manual analysis of boris_multiple_expected_paths, videos_path_unexpected
# Using boris_unexpected_path_videos and video_metadata_boris_multiples
# After running this, every boris observation should have an Expected.path that matches a Truncated.fixed.path in video_metadata
boris_data_cleaned <- boris_data %>%
  # remove duplicates
  filter(!grepl("/Volumes/Seagate Backup Plus Drive/UCSC Puma/XR6/1F_13May18/1F_13May18_B7", Media.file)) %>%
  filter(!grepl("/Volumes/Seagate Backup Plus Drive/UCSC Puma/XR6/5M/5M_5Sep17/5M_5Sep17_B12/RCNX1100.MP4", Media.file)) %>%
  # apply simple custom expected paths
  mutate(Expected.path = ifelse(
    Deployment.id == "4M_3Apr17" | Deployment.id == "4M_12Apr17",
    paste(Deployment.id, "CAM1", Filename, sep = "/"),
    Expected.path
  )) %>%
  mutate(Expected.path = ifelse(
    Deployment.id == "7M_15Jan18",
    paste(Deployment.id, "7M_15Jan18_A", Filename, sep = "/"),
    Expected.path
  )) %>%
  mutate(Expected.path = ifelse(
    Deployment.id == "5M_24Nov18" 
    | Deployment.id == "7M_9Oct18" 
    | Deployment.id == "7M_23Nov18" 
    | Deployment.id == "9F_18Dec18"
    | Deployment.id == "3M_26Nov18",
    paste(Deployment.id, "100RECNX", Filename, sep = "/"),
    Expected.path
  )) %>%
  mutate(Expected.path = ifelse(
    Deployment.id == "9F_7Dec18"
    # for the following, some videos ended up in 100RECNX and others in 101RECNX
    | grepl("/Volumes/Seagate Backup Plus Drive/UCSC Puma/XR6/3M/3M_26Nov18/3M_26Nov18_B(16|17)", Media.file)
    | grepl("/Volumes/Seagate Backup Plus Drive/UCSC Puma/XR6/8F/8F_14Dec18/8F_14Dec18_B(8|9|10|11)", Media.file),
    paste(Deployment.id, "101RECNX", Filename, sep = "/"),
    Expected.path
  ))

boris_joined_to_videos <- left_join(boris_data_cleaned, video_metadata, by = c("Expected.path" = "Truncated.fixed.path"))

# TODO: clean up boris data
# TODO: timebox observation records when checking by deployment

##################################################
# Deployment Data: NOT INCORPORATING VIDEO TIMES #
##################################################

source("summarize_by_deployments.R")
deployment_data <- summarize_by_deployments(boris_data)

# write deployment data to shapefile
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
