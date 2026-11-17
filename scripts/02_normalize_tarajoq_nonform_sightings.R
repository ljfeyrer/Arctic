# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Normalize Tarajoq Leg 2 sightings. Station-based sheets receive dates and
# coordinates from station/trawl lookup tables. Photo-ID-derived Leg 2 files
# remain a separate provenance stream.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(lubridate)
  library(purrr)
})

source("scripts/lib/helpers.R")

orig_root <- "input/orig_data"
out_root <- "input/normalized"
qa_root <- "output/Sightings_QA"
dir.create(qa_root, recursive = TRUE, showWarnings = FALSE)

parse_datetime <- function(date, time = NULL) {
  date <- na_text(date)
  if (!is.null(time)) date <- paste(date, na_text(time))
  serial <- suppressWarnings(as.numeric(date))
  out <- as.POSIXct(rep(NA_character_, length(date)), tz = "UTC")
  ok <- !is.na(serial) & serial > 20000 & serial < 60000
  out[ok] <- as.POSIXct(serial[ok] * 86400, origin = "1899-12-30", tz = "UTC")
  text_ok <- !ok & !is.na(date)
  out[text_ok] <- suppressWarnings(parse_date_time(
    str_replace_all(date[text_ok], "T", " "),
    orders = c(
      "ymd HMS", "ymd HM", "ymd I:M:S p", "ymd I:M p",
      "mdy HMS", "mdy HM", "mdy I:M:S p", "mdy I:M p",
      "dmy HMS", "dmy HM", "dmy I:M:S p", "dmy I:M p"
    ),
    tz = "UTC", quiet = TRUE
  ))
  out
}

read_station_lookup <- function(year) {
  standardized <- file.path(
    out_root, as.character(year), "stations",
    paste0("stations_", year, "_standardized.csv")
  )
  if (file.exists(standardized)) {
    x <- read_csv(standardized, col_types = cols(.default = col_character()), show_col_types = FALSE)
    x <- x %>% transmute(
      source_year = as.integer(source_year), set_number = as.integer(set_number),
      trawl_id = na_text(trawl_id), station_date = na_text(standard_date),
      station_time = na_text(standard_time), standard_datetime = parse_datetime(standard_datetime),
      datetime_basis = na_text(datetime_basis),
      datetime_timezone = "UTC (station source; no offset conversion)",
      latitude = as_num(latitude), longitude = as_num(longitude), station_minimum = NA_real_
    )
    if (any(!is.na(x$standard_datetime)) || year != 2025) {
      return(x)
    }
  }

  fallback <- file.path(orig_root, as.character(year), "station_ids", "Stns_2025_leg2.csv")
  if (!file.exists(fallback)) {
    return(tibble())
  }
  x <- read_csv(fallback, col_types = cols(.default = col_character()), show_col_types = FALSE)
  lat <- as_num(x$`Start Lat Degree`) + as_num(x$`Start Lat Minutes`) / 60
  lon <- -(as_num(x$`Start Long Degree`) + as_num(x$`Start Long Minutes`) / 60)
  start_dt <- parse_datetime(x$`Start Date`, x$`Start Time`)
  end_dt <- parse_datetime(x$`End Date`, x$`End Time`)
  dt <- coalesce(end_dt, start_dt)
  tibble(
    source_year = as.integer(year), set_number = as.integer(as_num(x$`Set Number`)),
    trawl_id = na_text(x$Trawl_ID), station_date = as.character(as.Date(dt)),
    station_time = format(dt, "%H:%M:%S", tz = "UTC"), standard_datetime = dt,
    datetime_basis = if_else(!is.na(end_dt), "station end time", "station start time (end unavailable)"),
    datetime_timezone = "UTC (station source; no offset conversion)",
    latitude = lat, longitude = lon, station_minimum = as_num(x$Whale_min)
  )
}

normalize_station_sheet <- function(year, path) {
  lookup <- read_station_lookup(year)
  if (year == 2024) {
    raw <- read_excel(path,
      sheet = "nbw_observation_TA_07_2024", skip = 1,
      col_types = "text", .name_repair = "unique"
    )
    names(raw)[1] <- "station_name"
    raw <- raw %>% mutate(
      source_year = year, source_row = row_number() + 2L,
      set_number = as.integer(str_extract(station_name, "[0-9]+")),
      nbw_flag = str_to_lower(na_text(raw[[2]])) == "yes"
    )
  } else {
    raw <- read_excel(path,
      sheet = "Leg 2 Sightings", col_types = "text",
      .name_repair = "unique"
    ) %>%
      mutate(
        source_year = year, source_row = row_number() + 1L, set_number = as.integer(as_num(Set)),
        station_name = paste0("Set ", set_number), nbw_flag = TRUE
      )
  }

  raw %>%
    left_join(lookup, by = c("set_number", "source_year" = "source_year")) %>%
    mutate(
      source_year = year,
      record_id = paste0(
        year, "_tarajoq_", if_else(year == 2024, "leg1", "leg2"),
        "_station_", row_number()
      ),
      standard_date = as.Date(standard_datetime),
      standard_time = format(standard_datetime, "%H:%M:%S", tz = "UTC"),
      count = if (year == 2025) coalesce(as_num(`Minimum whales`), as_num(station_minimum)) else NA_real_,
      species = "Northern bottlenose whale",
      source_family = if_else(year == 2024, "DFO_Tarajoq_Leg1_station", "DFO_Tarajoq_Leg2"),
      survey_leg = if_else(year == 2024, "Leg 1 (date-based classification)", "Leg 2"),
      survey_leg_basis = if_else(year == 2024, "station date range matches 2024 Leg 1", "source workbook / station date"),
      coordinate_source = if_else(!is.na(latitude) & !is.na(longitude), "station lookup", NA_character_),
      datetime_precision = if_else(!is.na(standard_datetime),
        coalesce(datetime_basis, "station end/start time"),
        "date/time unavailable"
      )
    ) %>%
    filter(nbw_flag) %>%
    transmute(record_id, source_family, source_year, survey_leg, survey_leg_basis,
      standard_datetime, standard_date, standard_time, datetime_precision,
      datetime_timezone,
      latitude, longitude, count, species,
      sample_id = NA_character_,
      source_file = basename(path), source_sheet = if_else(year == 2024,
        "nbw_observation_TA_07_2024", "Leg 2 Sightings"
      ), source_row,
      coordinate_source, comments = if (year == 2025) na_text(`Field notes`) else na_text(Comments)
    )
}

normalize_photo_id <- function(path, year) {
  raw <- read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
  dt <- parse_datetime(raw$DateT_UTC)
  tibble(
    record_id = paste0(year, "_tarajoq_leg2_photo_", seq_len(nrow(raw))),
    source_family = "DFO_Tarajoq_Leg2_PhotoID", source_year = year, survey_leg = "Leg 2",
    standard_datetime = dt, standard_date = as.Date(dt), standard_time = format(dt, "%H:%M:%S", tz = "UTC"),
    datetime_precision = "photo-ID encounter", latitude = as_num(raw$Latitude),
    longitude = as_num(raw$Longitude), count = as_num(raw$Count), species = na_text(raw$Species),
    sample_id = na_text(raw$Source_photo), source_file = basename(path), source_sheet = NA_character_,
    source_row = seq_len(nrow(raw)) + 1L, coordinate_source = "photo-ID metadata",
    coordinate_use = "validation_only", comments = na_text(raw$Comments)
  ) %>%
    group_by(standard_datetime, standard_date, latitude, longitude, sample_id) %>%
    mutate(source_duplicate_count = n()) %>%
    slice(1) %>%
    ungroup()
}

station_files <- c(
  `2024` = file.path(orig_root, "2024", "sightings_original", "Cetaceans_ArcticLeg2_2024.xlsx"),
  `2025` = file.path(orig_root, "2025", "sightings_original", "Cetaceans_ArcticLeg2_2025.xlsx")
)
for (year in names(station_files)) {
  path <- station_files[[year]]
  if (!file.exists(path)) next
  data <- normalize_station_sheet(as.integer(year), path)
  out_dir <- file.path(out_root, year, "sightings")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(data, file.path(
    out_dir,
    paste0("tarajoq_station_sightings_", year, "_standardized.csv")
  ), na = "")
  message(
    year, ": ", nrow(data), " station-based Tarajoq sightings (",
    if (as.integer(year) == 2024) "classified as Leg 1 by date" else "Leg 2",
    ")"
  )
}

photo_files <- list.files(orig_root,
  pattern = "^CetaceanSightings_leg2_[0-9]{4}[.]csv$",
  recursive = TRUE, full.names = TRUE
)
for (path in photo_files) {
  year <- as.integer(str_extract(basename(path), "[0-9]{4}"))
  data <- normalize_photo_id(path, year)
  out_dir <- file.path(out_root, as.character(year), "sightings")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(data, file.path(
    out_dir,
    paste0("tarajoq_photo_id_sightings_", year, "_standardized.csv")
  ), na = "")
  message(year, ": ", nrow(data), " photo-ID Leg 2 sightings")
}
