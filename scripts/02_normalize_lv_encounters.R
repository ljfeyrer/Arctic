#!/usr/bin/env Rscript

# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: Normalize each year's List View (LV) photo export into photo
# encounters. Every photo in the yearly LV export is kept -- including
# low-rated photos that never reach the multi-year catalogue -- and photos are
# grouped into encounters with the shared 30-minute rule. The result is the
# full list of times and places where whales were photographed that year,
# for comparison against the compiled Cetaceans sightings
# (09_compare_lv_encounters_to_cetaceans.R).
#
# This replaces the per-year collate_photo_id_encounters.R (archived in
# scripts/legacy/), which read from folders that no longer exist. It does not
# feed the sightings compiler: the photo-ID sightings that 03_ compiles still
# come from 02_normalize_2023_leg2_photoid.R and
# 02_normalize_tarajoq_nonform_sightings.R.
#
# Two clocks are kept for every photo and encounter, because the exports are
# not consistent about time zone: `Date Time` is the camera clock, and
# `Date Created` is an ISO timestamp that carries a UTC offset in some years
# (2023: -02:00) and a bare "Z" in others (2022), where it simply repeats
# the camera clock. Nothing here decides which is right; the comparison step
# tests both.
#
# Run with --year=YYYY, or --year=all (default) for every year that has an
# export. A year with an empty LV folder is skipped.
#
# Inputs (never modified):
#   input/orig_data/<year>/LV/*.csv
#
# Outputs:
#   input/normalized/<year>/LV_encounters/lv_encounters_<year>_standardized.csv
#   input/normalized/<year>/LV_encounters/lv_photos_<year>_standardized.csv
#   output/Sightings_QA/lv_encounter_normalization_audit_<year>.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
})

source("scripts/lib/helpers.R")

orig_root <- "input/orig_data"
out_root <- "input/normalized"
qa_root <- "output/Sightings_QA"
dir.create(qa_root, recursive = TRUE, showWarnings = FALSE)

encounter_gap_minutes <- 30
nbw_keyword_regex <- "\\bnbw\\b|bottlenose"

year_arg <- get_arg("year", "all")
all_years <- list.dirs(orig_root, recursive = FALSE, full.names = FALSE) %>%
  str_subset("^[0-9]{4}$") %>%
  sort()
years <- if (identical(year_arg, "all")) all_years else year_arg
if (!all(grepl("^[0-9]{4}$", years))) stop("Use --year=YYYY or --year=all")

iso_utc <- function(x) {
  if_else(is.na(x), NA_character_, format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}
clock_text <- function(x) {
  if_else(is.na(x), NA_character_, format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
}

# `Date Time` is month/day/year with hours and minutes ("11/4/2023 9:35").
# Formats are tried one at a time, minutes-only first: letting lubridate
# guess among several orders at once read "2023 9:35" as a two-digit year
# plus seconds for some rows.
parse_camera_clock <- function(x) {
  x <- na_text(x)
  out <- mdy_hm(x, tz = "UTC", quiet = TRUE)
  for (parser in list(mdy_hms, ymd_hms, ymd_hm)) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    out[todo] <- parser(x[todo], tz = "UTC", quiet = TRUE)
  }
  out
}

read_lv_export <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(source_file = basename(path), source_row = row_number() + 1L)
}

for (year in years) {
  lv_dir <- file.path(orig_root, year, "LV")
  files <- if (dir.exists(lv_dir)) {
    sort(list.files(lv_dir, pattern = "[.]csv$", full.names = TRUE, ignore.case = TRUE))
  } else {
    character(0)
  }
  if (!length(files)) {
    message(year, ": no LV export in ", lv_dir, "; skipped")
    next
  }

  raw <- map_dfr(files, read_lv_export)

  # The same export is sometimes saved twice under different names. A photo
  # is the same photo when its file name and creation timestamp both match.
  photos <- raw %>%
    group_by(`File name`, `Date Created`) %>%
    mutate(source_files = paste(unique(source_file), collapse = "; ")) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      file_name = na_text(`File name`),
      camera_datetime_utc = ymd_hms(na_text(`Date Created`), tz = "UTC", quiet = TRUE),
      camera_datetime_local = parse_camera_clock(`Date Time`),
      utc_offset_source = coalesce(
        str_extract(na_text(`Date Created`), "[+-][0-9]{2}:?[0-9]{2}$"),
        if_else(str_detect(coalesce(`Date Created`, ""), "Z$"), "Z (no offset recorded)", NA_character_)
      ),
      latitude = as_num(Latitude),
      longitude = as_num(Longitude),
      rating_stars = str_count(coalesce(Rating, ""), fixed("*")),
      keyword = na_text(`Keyword-export`),
      nbw_keyword = str_detect(str_to_lower(coalesce(keyword, "")), nbw_keyword_regex),
      source_file = source_files,
      source_row
    ) %>%
    # Seconds-resolution timestamp when present, camera clock otherwise.
    mutate(grouping_datetime = coalesce(camera_datetime_utc, camera_datetime_local))

  photos_without_time <- sum(is.na(photos$grouping_datetime))
  photos <- photos %>%
    filter(!is.na(grouping_datetime)) %>%
    arrange(grouping_datetime, file_name) %>%
    mutate(
      encounter_number = group_into_encounters(grouping_datetime, gap_minutes = encounter_gap_minutes),
      record_id = paste0(year, "_lv_encounter_", sprintf("%03d", encounter_number))
    )

  encounters <- photos %>%
    group_by(record_id, encounter_number) %>%
    summarise(
      source_year = as.integer(year),
      start_utc = min(camera_datetime_utc, na.rm = TRUE),
      end_utc = max(camera_datetime_utc, na.rm = TRUE),
      start_local = min(camera_datetime_local, na.rm = TRUE),
      end_local = max(camera_datetime_local, na.rm = TRUE),
      utc_offset_source = first(na.omit(utc_offset_source)),
      photo_count = n(),
      photos_3star_plus = sum(rating_stars >= 3),
      max_rating_stars = max(rating_stars),
      nbw_keyword_photos = sum(nbw_keyword),
      photos_without_coordinates = sum(is.na(latitude) | is.na(longitude)),
      mean_latitude = if (any(!is.na(latitude) & !is.na(longitude))) {
        mean(latitude[!is.na(latitude) & !is.na(longitude)])
      } else {
        NA_real_
      },
      mean_longitude = if (any(!is.na(latitude) & !is.na(longitude))) {
        mean(longitude[!is.na(latitude) & !is.na(longitude)])
      } else {
        NA_real_
      },
      position_spread_km = if (any(!is.na(latitude) & !is.na(longitude))) {
        max(haversine_m(latitude, longitude, mean_latitude, mean_longitude), na.rm = TRUE) / 1000
      } else {
        NA_real_
      },
      first_photo = first(file_name),
      last_photo = last(file_name),
      source_file = paste(unique(source_file), collapse = "; "),
      .groups = "drop"
    ) %>%
    transmute(
      record_id, source_year,
      standard_datetime = iso_utc(start_utc),
      standard_date = as.Date(start_utc),
      standard_time = format(start_utc, "%H:%M:%S", tz = "UTC"),
      encounter_end_datetime = iso_utc(end_utc),
      camera_clock_start = clock_text(start_local),
      camera_clock_end = clock_text(end_local),
      utc_offset_source,
      duration_minutes = round(as.numeric(difftime(end_utc, start_utc, units = "mins")), 1),
      latitude = mean_latitude, longitude = mean_longitude,
      position_spread_km = round(position_spread_km, 2),
      coordinate_source = if_else(is.na(latitude), NA_character_, "mean of photo GPS positions"),
      photo_count, photos_3star_plus, max_rating_stars, photos_without_coordinates,
      nbw_keyword_photos,
      species = "Northern Bottlenose Whale",
      species_basis = if_else(
        nbw_keyword_photos > 0, "NBW keyword on at least one photo",
        "no species keyword in export; assumed from survey context"
      ),
      first_photo, last_photo, source_file
    )

  photo_table <- photos %>%
    transmute(
      record_id, file_name,
      camera_datetime_utc = iso_utc(camera_datetime_utc),
      camera_clock = clock_text(camera_datetime_local),
      latitude, longitude, rating_stars, keyword, nbw_keyword, source_file, source_row
    )

  out_dir <- file.path(out_root, year, "LV_encounters")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  encounter_file <- file.path(out_dir, paste0("lv_encounters_", year, "_standardized.csv"))
  write_csv(encounters, encounter_file, na = "")
  write_csv(photo_table, file.path(out_dir, paste0("lv_photos_", year, "_standardized.csv")), na = "")

  audit <- tibble(
    year = as.integer(year),
    input_files = paste(basename(files), collapse = "; "),
    input_rows = nrow(raw),
    duplicate_photo_rows_removed = nrow(raw) - nrow(photos) - photos_without_time,
    photos_without_timestamp = photos_without_time,
    photos_kept = nrow(photos),
    encounter_count = nrow(encounters),
    encounters_without_coordinates = sum(is.na(encounters[["latitude"]])),
    encounter_gap_minutes = encounter_gap_minutes,
    output_file = normalizePath(encounter_file, winslash = "/", mustWork = FALSE)
  )
  write_csv(audit, file.path(qa_root, paste0("lv_encounter_normalization_audit_", year, ".csv")), na = "")

  message(
    year, ": ", nrow(raw), " LV rows -> ", nrow(photos), " photos in ",
    nrow(encounters), " encounters -> ", encounter_file
  )
}
