#!/usr/bin/env Rscript

# Normalize 2023 Leg 2 field-note observations and join their station dates
# and positions. Positive NBW observations become sightings; negative station
# notes are retained in a separate review table and are not sightings.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

note_file <- "input/normalized/2023/sightings/leg2_fieldnotes_2023_transcription.csv"
original_note_file <- "input/orig_data/2023/sightings_original/leg2_sightings_note.txt"
station_file <- "input/normalized/2023/stations/stations_2023_standardized.csv"
out_dir <- "input/normalized/2023/sightings"
qa_dir <- "output/Sightings_QA"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

notes <- read_csv(note_file, show_col_types = FALSE) %>%
  mutate(set_number = as.integer(set_number))
stations <- read_csv(station_file, show_col_types = FALSE) %>%
  filter(source_year == 2023) %>%
  select(set_number,
         station_trawl_id = trawl_id,
         station_fix_pos = fix_pos, station_datetime = standard_datetime,
         station_datetime_basis = datetime_basis,
         station_start_datetime, station_end_datetime,
         station_date = standard_date, station_time = standard_time,
         station_latitude = latitude, station_longitude = longitude)

review <- notes %>%
  left_join(stations, by = "set_number") %>%
  mutate(
      match_status = case_when(
      is.na(station_trawl_id) ~ "no_station_match",
      !is.na(fix_pos_raw) & !is.na(station_fix_pos) & fix_pos_raw != station_fix_pos ~ "station_fix_conflict",
      TRUE ~ "matched_station"
    ),
    source_file = basename(original_note_file),
    source_method = "field_note",
    survey_leg = "Leg 2",
    station_set_number = set_number,
    coordinate_source = if_else(!is.na(station_latitude), "station lookup", NA_character_)
  )

positive <- review %>%
  filter(nbw_present, match_status == "matched_station") %>%
  transmute(
    record_id = paste0("2023_field_note_leg2_", note_row),
    source_program = "Tarajoq", source_method, source_year = 2023L, survey_leg,
    standard_datetime = station_datetime, standard_date = as.Date(station_date),
    standard_time = station_time,
    datetime_precision = coalesce(station_datetime_basis, "station end/start time"),
    datetime_timezone = "UTC (station source; no offset conversion)",
    latitude = station_latitude, longitude = station_longitude,
    count = if_else(!is.na(count_min) & !is.na(count_max), (count_min + count_max) / 2,
                    coalesce(count_min, count_max)),
    count_min, count_max, species = "Northern Bottlenose whale",
    sample_id = NA_character_, source_file, source_sheet = NA_character_,
    source_row = note_row, coordinate_source, coordinate_use = "observation_location",
    station_set_number, station_trawl_id, station_fix_pos,
    station_start_datetime, station_end_datetime,
    comments = notes
  )

write_csv(positive, file.path(out_dir, "field_note_sightings_2023_standardized.csv"), na = "")
write_csv(review, file.path(qa_dir, "2023_field_note_station_match_review.csv"), na = "")
message("Positive NBW field-note sightings: ", nrow(positive))
message("All field-note station records reviewed: ", nrow(review))
