library(exiftoolr)

# README
# This file extracts MP4 metadata, namely start and end datetimes and duration,
# from MP4 video files stored in the XR6 folder.
# This must be run to generate video_metadata.csv, prior to running setup_boris_data.R.
# PATH_TO_XR6 must be set prior to running.

source("datetime_string_fns.R")

PATH_TO_XR6 = "/Volumes/One Touch/Siskiyou/CamTrap/XR6"
files <- list.files(PATH_TO_XR6, pattern = "\\.MP4$", recursive = TRUE, full.names = TRUE)
filtered_files <- files[grepl("XR6/[0-9]+[M,F]_", files)]
filtered_files <- sort(filtered_files)

# doublecheck this: should just be videos stored in COOL_VIDEOS folder
filtered_out_files <- files[!grepl("XR6/[0-9]+[M,F]_", files)]
filtered_out_files <- sort(filtered_out_files)

# check: all files match this regex
check_filtered_files <- filtered_files[grepl("(RCNX[0-9]+).MP4", filtered_files)]
if (!identical(filtered_files, check_filtered_files)) {
  stop("Error: all found files must match RCNX####.MP4 format")
}

# warning: this line takes ages
raw_video_metadata <- exif_read(filtered_files)

# extract the valuable data (start time, duration, end time)
video_metadata <- raw_video_metadata %>%
  mutate(Start = convert_exiftool_datetime(CreateDate)) %>%
  mutate(End = add_seconds_to_exiftool_datetime(CreateDate, Duration)) %>%
  select(
    SourceFile,
    Start,
    End,
    Duration
  )

# Uncomment and start from here to fix a broken write
#video_metadata <- read.csv("video_metadata.csv")
if (any(is.na(video_metadata$Start), is.na(video_metadata$End), is.na(video_metadata$Duration))) {
  stop("Error: every video must have Start, End, and Duration.")

  # RETRY videos that are broken
  broken_videos <- video_metadata %>%
    filter(is.na(Start) | is.na(End) | is.na(Duration))
  broken_video_files <- broken_videos$SourceFile
  broken_video_metadata <- exif_read(broken_video_files)
  fixed_video_metadata <- broken_video_metadata %>%
    mutate(Start = convert_exiftool_datetime(CreateDate)) %>%
    mutate(End = add_seconds_to_exiftool_datetime(CreateDate, Duration)) %>%
    select(
      SourceFile,
      Start,
      End,
      Duration
    )
  # --> some were broken initially because of Daylight Savings Time. fixed here and replaced back below:
  video_metadata <- video_metadata %>%
    filter(!is.na(Start) & !is.na(End) & !is.na(Duration))
  video_metadata <- rbind(video_metadata, fixed_video_metadata)
  
  # remove error checking variables
  #remove(broken_videos)
  #remove(broken_video_files)
  #remove(broken_video_metadata)
  #remove(fixed_video_metadata)
}

write.csv(video_metadata, "video_metadata.csv", row.names = FALSE)
