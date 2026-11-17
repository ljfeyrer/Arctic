# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Recreate 2023 Leg 2 photo-ID sightings from the photo export. Photos are
# grouped into 30-minute encounter windows, then linked back to the nine later-November
# rows in the Tarajoq Cetaceans workbook. No photo coordinate is used to fill
# another source record.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(lubridate)
  library(stringr)
  library(purrr)
})

source("scripts/lib/helpers.R")

photo_file <- "input/orig_data/2023/photo_id/List View Leg2_2023.csv"
cetacean_file <- "input/orig_data/2023/sightings_original/Cetaceans_Arctic2023.xlsx"
out_dir <- "input/normalized/2023/sightings"
qa_dir <- "output/Sightings_QA"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

photos <- read_csv(photo_file, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
  mutate(
    camera_datetime_local = parse_date_time(`Date Time`,
      orders = c("mdy HM", "mdy HMS"),
      tz = "UTC", quiet = TRUE
    ),
    camera_datetime_utc = parse_date_time(`Date Created`,
      orders = c("ymd HMS z", "ymd HMS"),
      tz = "UTC", quiet = TRUE
    ),
    photo_source_offset = str_extract(`Date Created`, "[+-][0-9]{2}:?[0-9]{2}"),
    latitude_num = suppressWarnings(as.numeric(Latitude)),
    longitude_num = suppressWarnings(as.numeric(Longitude))
  ) %>%
  arrange(camera_datetime_local, `File name`) %>%
  mutate(
    gap_minutes = as.numeric(difftime(camera_datetime_local,
      lag(camera_datetime_local),
      units = "mins"
    )),
    new_encounter = row_number() == 1 | gap_minutes > 30,
    encounter_number = group_into_encounters(camera_datetime_local, gap_minutes = 30),
    photo_group_id = paste0("2023_leg2_photo_group_", sprintf("%02d", encounter_number))
  )

photo_groups <- photos %>%
  group_by(photo_group_id, encounter_number) %>%
  summarise(
    photo_start_local = min(camera_datetime_local, na.rm = TRUE),
    photo_end_local = max(camera_datetime_local, na.rm = TRUE),
    photo_start_utc = min(camera_datetime_utc, na.rm = TRUE),
    photo_end_utc = max(camera_datetime_utc, na.rm = TRUE),
    photo_source_offset = first(na.omit(photo_source_offset)),
    photo_count = n(),
    latitude = if (any(!is.na(latitude_num))) mean(latitude_num, na.rm = TRUE) else NA_real_,
    longitude = if (any(!is.na(longitude_num))) mean(longitude_num, na.rm = TRUE) else NA_real_,
    photos_without_coordinates = sum(is.na(latitude_num) | is.na(longitude_num)),
    first_photo = first(`File name`),
    last_photo = last(`File name`),
    photo_files = paste(`File name`, collapse = "; "),
    photo_date = as.Date(photo_start_local),
    .groups = "drop"
  )

cetaceans <- read_excel(cetacean_file, col_types = "text") %>%
  mutate(
    source_row = row_number() + 1L,
    source_datetime_local = as.POSIXct(as.numeric(Local_Time) * 86400,
      origin = "1899-12-30", tz = "UTC"
    ),
    source_date = as.Date(source_datetime_local),
    source_latitude = suppressWarnings(as.numeric(Latitude)),
    source_longitude = suppressWarnings(as.numeric(Longitude))
  ) %>%
  filter(source_date >= as.Date("2023-11-10"))

source_photo_review <- map_dfr(seq_len(nrow(cetaceans)), function(i) {
  candidates <- photo_groups %>%
    filter(photo_date == cetaceans$source_date[i]) %>%
    mutate(time_difference_minutes = abs(as.numeric(difftime(
      photo_start_local, cetaceans$source_datetime_local[i],
      units = "mins"
    )))) %>%
    arrange(time_difference_minutes)

  if (nrow(candidates) == 0 || candidates$time_difference_minutes[1] > 30) {
    return(tibble(
      source_row = cetaceans$source_row[i], source_date = cetaceans$source_date[i],
      source_datetime_local = cetaceans$source_datetime_local[i],
      source_latitude = cetaceans$source_latitude[i], source_longitude = cetaceans$source_longitude[i],
      photo_group_id = NA_character_, photo_count = NA_integer_,
      photo_start_local = as.POSIXct(NA), photo_end_local = as.POSIXct(NA),
      time_difference_minutes = NA_real_, photo_match_status = "no_photo_group_within_30_minutes"
    ))
  }

  best <- candidates[1, ]
  tibble(
    source_row = cetaceans$source_row[i], source_date = cetaceans$source_date[i],
    source_datetime_local = cetaceans$source_datetime_local[i],
    source_latitude = cetaceans$source_latitude[i], source_longitude = cetaceans$source_longitude[i],
    photo_group_id = best$photo_group_id, photo_count = best$photo_count,
    photo_start_local = best$photo_start_local, photo_end_local = best$photo_end_local,
    time_difference_minutes = best$time_difference_minutes,
    photo_match_status = "matched_photo_group"
  )
})

matched_rows <- source_photo_review %>%
  filter(photo_match_status == "matched_photo_group") %>%
  select(source_row, photo_group_id)

photo_sightings <- photo_groups %>%
  inner_join(matched_rows, by = "photo_group_id") %>%
  group_by(photo_group_id) %>%
  summarise(
    record_id = first(photo_group_id), source_program = "Tarajoq",
    source_method = "photo_id_encounter", source_year = 2023L,
    survey_leg = "Leg 2", survey_leg_basis = "2023 Cetaceans rows matched within 30 minutes",
    standard_datetime = format(first(photo_start_utc), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    standard_date = as.Date(first(photo_start_utc)),
    standard_time = format(first(photo_start_utc), "%H:%M:%S", tz = "UTC"),
    datetime_precision = "photo encounter group",
    photo_local_datetime = format(first(photo_start_local), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    photo_utc_datetime = format(first(photo_start_utc), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    photo_source_offset = first(photo_source_offset),
    photo_time_provenance = "Date Time used for encounter matching; Date Created ISO offset used for UTC standardization",
    latitude = first(latitude), longitude = first(longitude),
    count = 1, species = "Northern Bottlenose whale", sample_id = first(photo_files),
    source_file = basename(photo_file), source_sheet = NA_character_,
    source_row = paste(sort(unique(source_row)), collapse = "; "),
    coordinate_source = "photo-ID metadata", coordinate_use = "validation_only",
    comments = paste0(
      "Photos grouped within 30 minutes; photo count = ", first(photo_count),
      "; photo coordinates retained for track/station validation only."
    ),
    photo_count = first(photo_count), photo_files = first(photo_files),
    source_duplicate_count = 1L,
    .groups = "drop"
  ) %>%
  arrange(standard_datetime)

write_csv(photo_sightings,
  file.path(out_dir, "tarajoq_photo_id_sightings_2023_standardized.csv"),
  na = ""
)
write_csv(source_photo_review,
  file.path(qa_dir, "2023_leg2_photoid_reconstruction_review.csv"),
  na = ""
)

message(
  "Photo groups: ", nrow(photo_groups), "; matched Leg 2 sightings: ", nrow(photo_sightings),
  "; unmatched Cetaceans rows: ", sum(source_photo_review$photo_match_status != "matched_photo_group")
)
