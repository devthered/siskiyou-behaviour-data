library(exiftoolr)

source("datetime_string_fns.R")

files <- list.files("/Volumes/One Touch/Siskiyou/CamTrap/XR6", pattern = "\\.MP4$", recursive = TRUE, full.names = TRUE)
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
video_metadata <- exif_read(filtered_files)

# extract the valuable data (start time, duration, end time)
simplified_video_metadata <- video_metadata %>%
  mutate(Start = convert_exiftool_datetime(CreateDate)) %>%
  mutate(End = add_seconds_to_exiftool_datetime(CreateDate, Duration)) %>%
  select(
    SourceFile,
    Start,
    End,
    Duration
  )

write.csv(simplified_video_metadata, "video_metadata.csv", row.names = FALSE)
