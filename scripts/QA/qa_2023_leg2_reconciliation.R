# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Reconcile the 2023 Leg 2 Cetaceans records against photo-ID encounter
# groups and Annika's field-note sightings. This is an audit only: it does
# not merge or remove records.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

source("scripts/lib/helpers.R")

qa_dir <- "output/Sightings_QA"
source_review <- read_csv(file.path(qa_dir, "2023_leg2_photoid_reconstruction_review.csv"),
  show_col_types = FALSE
) %>%
  filter(source_row >= 41, source_row <= 49) %>%
  select(
    -photo_group_id, -photo_count, -photo_start_local, -photo_end_local,
    -time_difference_minutes
  ) %>%
  mutate(source_datetime = ymd_hms(source_datetime_local, tz = "UTC"))

photo <- read_csv(
  "input/normalized/2023/sightings/tarajoq_photo_id_sightings_2023_standardized.csv",
  show_col_types = FALSE
) %>%
  transmute(
    photo_group_id,
    photo_source_row = as.integer(source_row),
    photo_datetime = ymd_hms(standard_datetime, tz = "UTC"),
    photo_local_datetime = ymd_hms(photo_local_datetime, tz = "UTC"),
    photo_latitude = latitude,
    photo_longitude = longitude,
    photo_count = photo_count
  )

field_notes <- read_csv(
  "input/normalized/2023/sightings/field_note_sightings_2023_standardized.csv",
  show_col_types = FALSE
) %>%
  transmute(
    field_note_id = record_id,
    field_datetime = ymd_hms(standard_datetime, tz = "UTC"),
    field_latitude = latitude,
    field_longitude = longitude,
    field_count = count,
    field_station_set_number = station_set_number,
    field_set = station_fix_pos,
    field_note = comments
  )

field_matches <- bind_rows(lapply(seq_len(nrow(source_review)), function(i) {
  s <- source_review[i, ]
  candidates <- field_notes %>%
    mutate(
      coordinate_distance_m = haversine_m(
        s$source_latitude, s$source_longitude, field_latitude, field_longitude
      ),
      time_difference_hours = as.numeric(difftime(field_datetime, s$source_datetime,
        units = "hours"
      ))
    ) %>%
    filter(
      as.Date(field_datetime) == s$source_date,
      coordinate_distance_m <= 10
    ) %>%
    arrange(coordinate_distance_m, abs(time_difference_hours))

  if (nrow(candidates) == 0) {
    return(tibble(
      source_row = s$source_row,
      field_note_id = NA_character_, field_datetime = as.POSIXct(NA),
      field_count = NA_real_,
      field_set = NA_character_, field_note = NA_character_,
      field_coordinate_distance_m = NA_real_, field_time_difference_hours = NA_real_
    ))
  }

  candidates %>%
    slice(1) %>%
    transmute(
      source_row = s$source_row,
      field_note_id, field_datetime, field_count, field_station_set_number,
      field_set,
      field_note, field_coordinate_distance_m = coordinate_distance_m,
      field_time_difference_hours = time_difference_hours
    )
}))

# Build the comparison table explicitly so rows without a photo group remain.
review <- source_review %>%
  left_join(photo, by = c("source_row" = "photo_source_row")) %>%
  left_join(field_matches, by = "source_row") %>%
  mutate(
    photo_coordinate_distance_m = if_else(
      !is.na(photo_latitude) & !is.na(source_latitude),
      haversine_m(source_latitude, source_longitude, photo_latitude, photo_longitude),
      NA_real_
    ),
    photo_time_difference_minutes = as.numeric(
      difftime(photo_local_datetime, source_datetime, units = "mins")
    ),
    photo_coordinate_status = case_when(
      is.na(photo_coordinate_distance_m) ~ "no_photo_coordinates",
      photo_coordinate_distance_m <= 1000 ~ "encounter_match_within_1km",
      TRUE ~ "coordinate_review"
    ),
    source_time_status = case_when(
      !is.na(field_station_set_number) & field_station_set_number == 161 ~
        "station PM authoritative; Cetaceans workbook appears AM/PM error",
      !is.na(field_note_id) & abs(field_time_difference_hours) <= (1 / 60) ~ "field note and Cetaceans time agree",
      !is.na(field_note_id) ~ "field note and Cetaceans time differ; review",
      TRUE ~ "not field-note matched"
    ),
    reconciled_source_datetime = case_when(
      !is.na(field_station_set_number) & field_station_set_number == 161 ~ field_datetime,
      TRUE ~ source_datetime
    ),
    reconciliation = case_when(
      !is.na(field_note_id) & !is.na(photo_group_id) ~ "Cetaceans + field note + photo group",
      !is.na(field_note_id) & field_station_set_number == 161 ~ "Cetaceans + field note; station PM resolves AM/PM issue; no photo group",
      !is.na(field_note_id) ~ "Cetaceans + field note; no photo group",
      !is.na(photo_group_id) ~ "Cetaceans + photo group matched on local clock",
      TRUE ~ "Cetaceans only; review"
    )
  ) %>%
  select(
    source_row, source_date, source_datetime, source_latitude, source_longitude,
    photo_group_id, photo_datetime, photo_count, photo_coordinate_distance_m,
    photo_time_difference_minutes, photo_match_status, photo_coordinate_status,
    field_note_id, field_datetime, field_count, field_station_set_number,
    field_set,
    field_coordinate_distance_m, field_time_difference_hours, field_note,
    source_time_status, reconciled_source_datetime, reconciliation
  ) %>%
  arrange(source_row)

out_file <- file.path(qa_dir, "2023_leg2_reconciliation_review.csv")
write_csv(review, out_file, na = "")
message("Wrote ", out_file)
print(review, n = Inf)
