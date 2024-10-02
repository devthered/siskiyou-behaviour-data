library(dplyr)
library(tidyr)
library(stringr)

# README
# This file reads BORIS data from combined_boris_data.csv (created by combine_boris_csvs.R)
# and MP4 metadata from video_metadata.csv (created by extract_mp4_metadata.R)
# and matches them up, to add the correct video start and stop times to the BORIS data
# The code here also creates intermediates used for error checking, to identify mismatches
# in Deployment IDs and filenames. These mismatches have been identified and corrected
# and matched, cleaned BORIS output with all errors resolves is written to boris_tidy.csv.

########################################################
# 1. Read in combined boris CSV and do initial cleanup #
########################################################

boris_data <- read.csv("combined_boris_data.csv")
boris_data <- boris_data %>%
  # extract Deployment.id (first two parts of Observation.id)
  mutate(Deployment.id =
           sapply(strsplit(Observation.id, "_"), function(x)
             paste(x[1:2], collapse = "_"))) %>%
  # fix some errors in the IDs. These were found through manual analysis.
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
  # clean up columns
  select(
    Deployment.id,
    Media.file,
    Subject,
    Behaviour = Behavior,
    Modifiers,
    Behaviour.start = Start..s.,
    Behaviour.end = Stop..s.,
    Behaviour.duration = Duration..s.
  )


#############################################################
# 2. Get video filenames and expected paths from BORIS data #
#############################################################

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
if (any(boris_data$Deployment.id != boris_data$Deployment.id.from.filepath,
        is.na(boris_data$Deployment.id),
        is.na(boris_data$Deployment.id.from.filepath),
        is.na(boris_data$Filename))) {
  stop("Error: boris data should all have correct Deployment ID and Filename")
}
# --> confirmed  this is correct for our data, after Deployment.id corrections above
# --> remove Deployment.id.from.filepath
boris_data <- boris_data %>% select(-Deployment.id.from.filepath)

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
# --> confirmed this is correct for our data
# --> remove Num.expected.paths
boris_media_files <- boris_media_files %>% select(-Num.expected.paths)

# FIND ERRORS: BORIS entries where a single expected path matches multiple possible source files
boris_multiple_expected_paths <- boris_data %>%
  group_by(Expected.path) %>%
  filter(length(unique(Media.file)) > 1) %>%
  ungroup()

#############################################
# 3. Get video filenames and expected paths #
#############################################

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
if (any(is.na(video_metadata$Deployment.id),
        is.na(video_metadata$Filename))) {
  stop("Error: video data should all have Deployment ID and Filename")
}
# --> confirmed that this is correct with our data, after fixing "April" -> "Apr"

# FIND ERRORS: Video files where the actual path does not equal the expected path
videos_path_unexpected <- video_metadata %>%
  filter(Truncated.fixed.path != Expected.path) %>%
  group_by(Deployment.id, Containing.folder) %>%
  summarize(.groups = 'drop')

###################################################
# 4. Investigate and resolve file matching errors #
###################################################

# FIND ERRORS: BORIS video folders with unexpected paths from video files
boris_unexpected_path_videos <- boris_data %>%
  filter(Deployment.id %in% videos_path_unexpected$Deployment.id) %>%
  group_by(Deployment.id, Containing.folder) %>%
  summarize(.groups = 'drop')
  
# FIND ERROR: Videos with duplicate paths in boris
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
    | Deployment.id == "3M_26Nov18"
    | Deployment.id == "8F_14Dec18",
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

# CHECK ERRORS: BORIS entries where a single expected path matches multiple possible source files
# this should be fixed now, so nothing should turn up
boris_FIXED_multiple_expected_paths <- boris_data_cleaned %>%
  group_by(Expected.path) %>%
  filter(length(unique(Media.file)) > 1) %>%
  ungroup()
if (nrow(boris_FIXED_multiple_expected_paths) != 0) {
  stop("Error: BORIS cleanup did not fix errors.")
}

# remove intermediate dfs used for error checking
remove(boris_FIXED_multiple_expected_paths)
remove(boris_multiple_expected_paths)
remove(boris_unexpected_path_videos)
remove(videos_path_unexpected)
remove(video_metadata_boris_multiples)


#################################################
# 5. Join BORIS data to video metadata and tidy #
#################################################

# Join BORIS data with video metadata
boris_joined <- left_join(boris_data_cleaned, video_metadata, by = c("Expected.path" = "Truncated.fixed.path"))

# Confirm Deployment ids all match as expected
if (any(!is.na(boris_joined$Deployment.id.y) & boris_joined$Deployment.id.x != boris_joined$Deployment.id.y)) {
  stop("Error: Deployment.ids all match")
}
# --> confirmed that this is correct for all

# 14M_10Feb20 videos are missing from my dataset
boris_no_matching_video <- boris_joined %>%
  filter(is.na(Start) | is.na(End) | is.na(Duration))

boris_tidy <- boris_joined %>%
  select(
    Deployment.id = Deployment.id.x,
    Video.relative.path = Expected.path,
    Video.start = Start,
    Video.end = End,
    Video.duration = Duration,
    Subject,
    Behaviour,
    Modifiers,
    Behaviour.start,
    Behaviour.end,
    Behaviour.duration
  ) %>%
  arrange(Deployment.id)

write.csv(boris_tidy, "boris_tidy.csv", row.names = FALSE)