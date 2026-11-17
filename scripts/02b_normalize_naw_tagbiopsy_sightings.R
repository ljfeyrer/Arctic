#!/usr/bin/env Rscript

# Author: Laura Joan Feyrer
# Date created: 2026-11-17
# Date updated: 2026-11-17
# Description: Normalize the compiled NAW (New Arctic Whales) tag-deployment
# and biopsy-sample spreadsheets into per-year NBW sighting records. These
# were not logged as sightings at the time -- they are tag/biopsy metadata
# compiled after the fact -- but each tag deployment or biopsy sample is
# direct evidence of an individual NBW at a time and place, so they are
# normalized here as NAW sighting records (source_family = "NAW",
# coordinate_source distinguishes them from logged Arctic_<year>.xlsx
# sightings). Filtered to species = NBW and location = Baffin Bay only, per
# L. Storrie's 2026-11-17 email; other species/locations in these workbooks
# (other cetaceans, Newfoundland, Admiralty Inlet, Pond Inlet, Cumberland
# Sound) are out of scope and excluded.
#
# Two source files, each covering a different piece:
#   input/orig_data/NewArcticWhales2018_2025_LS_2025.09.11.xlsx (sheet
#     "all_tags"): one row per tagged individual, with its tag deployment
#     start position. The more current of the two files for tag data.
#   input/orig_data/New Arctic Whales 2018-2024 tags and biopsy
#     2025.05.20.xlsx (sheet "Biopsy samples"): one row per biopsy sample.
#     Only this file has biopsy data.
# A whale that was both tagged and biopsied in the same encounter appears as
# two separate records (one from each sheet) -- this is intentional, not
# deduplicated here, so that 06_merge_annual_sightings.R's existing
# cross-vessel/source duplicate audit can flag and review the overlap like
# it already does for Tarajoq vs. NAW sightings.
#
# Output: input/normalized/<year>/sightings/naw_tagbiopsy_<year>_standardized.csv
# (new year folders created as needed -- the source spreadsheets cover years
# with no existing Arctic_<year>.xlsx sightings workbook, e.g. 2017, 2019, 2021).
# Original source files are never modified.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(lubridate)
  library(tibble)
})

source("scripts/lib/helpers.R")

root <- "input/orig_data"
out_root <- "input/normalized"

tags_path <- file.path(root, "NewArcticWhales2018_2025_LS_2025.09.11.xlsx")
biopsy_path <- file.path(root, "New Arctic Whales 2018-2024 tags and biopsy 2025.05.20.xlsx")

# Mixed date/datetime formats across both sheets: Excel serials (numeric,
# including a bare day-fraction with no date for one 2017 row -- treated as
# unrecorded), "20/09/2018" and "04-10-2022 04:00" (day-month-year, confirmed
# against the same encounters' tag-deployment dates, which are unambiguous
# serials), and month-name forms ("July 5 2019", "15 Oct. 2021").
parse_naw_datetime <- function(x) {
  x <- na_text(x)
  out <- as.POSIXct(rep(NA_character_, length(x)), tz = "UTC")

  serial <- suppressWarnings(as.numeric(x))
  is_serial <- !is.na(serial) & serial >= 1
  out[is_serial] <- as.POSIXct(serial[is_serial] * 86400, origin = "1899-12-30", tz = "UTC")

  rest <- which(!is_serial & !is.na(x))
  cleaned <- str_replace_all(x[rest], fixed("."), "")

  month_name_first <- str_detect(cleaned, "^[A-Za-z]+ +[0-9]{1,2},? +[0-9]{4}$")
  out[rest[month_name_first]] <- suppressWarnings(mdy(cleaned[month_name_first], tz = "UTC", quiet = TRUE))

  day_month_name <- str_detect(cleaned, "^[0-9]{1,2} +[A-Za-z]+ +[0-9]{4}$")
  out[rest[day_month_name]] <- suppressWarnings(dmy(cleaned[day_month_name], tz = "UTC", quiet = TRUE))

  numeric_dmy <- str_detect(cleaned, "^[0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{4}")
  has_time <- numeric_dmy & str_detect(cleaned, "[0-9]{1,2}:[0-9]{2}")
  out[rest[has_time]] <- suppressWarnings(dmy_hm(cleaned[has_time], tz = "UTC", quiet = TRUE))
  date_only_dmy <- numeric_dmy & !has_time
  out[rest[date_only_dmy]] <- suppressWarnings(dmy(cleaned[date_only_dmy], tz = "UTC", quiet = TRUE))

  out
}

# This dataset is Baffin Bay / Davis Strait only (western hemisphere,
# northern hemisphere): forces a consistent sign rather than trusting the
# source, which is wrong for the 2023 Biopsy samples rows (longitude
# recorded positive -- compare e.g. WH-BIOP-23-05's 59.78 against the
# matching all_tags ptt 241770 fix at -59.78 the same day).
fix_lon <- function(x) -abs(as_num(x))
fix_lat <- function(x) abs(as_num(x))

first_num <- function(x) suppressWarnings(as.numeric(str_extract(x, "[0-9]+")))

collapse_comment <- function(...) {
  parts <- list(...)
  out <- do.call(paste, c(parts, sep = "; "))
  out <- str_replace_all(out, "(^|; )NA(?=;|$)", "")
  out <- str_replace_all(out, "^; +|; +$", "")
  out <- str_replace_all(out, "; +; +", "; ")
  na_text(out)
}

normalize_tags <- function(path) {
  raw <- read_excel(path, sheet = "all_tags", col_types = "text", .name_repair = "unique") %>%
    mutate(source_row = row_number() + 1L)
  raw <- raw %>%
    filter(str_to_lower(na_text(species)) == "nbw", str_to_lower(na_text(location)) == "baffin")
  if (!nrow(raw)) {
    return(tibble())
  }

  datetime <- parse_naw_datetime(raw$date_time_utc_start)
  tibble(
    record_id = paste0(raw$year, "_naw_tagbiopsy_tag_", coalesce(na_text(raw$ptt), as.character(raw$source_row))),
    source_family = "NAW",
    source_year = as.integer(raw$year),
    survey_leg = NA_character_,
    standard_datetime = datetime,
    standard_date = as.Date(datetime, tz = "UTC"),
    standard_time = if_else(!is.na(datetime), format(datetime, "%H:%M:%S", tz = "UTC"), NA_character_),
    datetime_precision = if_else(!is.na(datetime), "minute_or_better", "unknown"),
    latitude = fix_lat(raw$lat_N_dec_deg_start),
    longitude = fix_lon(raw$lon_E_dec_deg_start),
    count = 1,
    species = "Northern Bottlenose Whale",
    sample_id = na_text(raw$ptt),
    source_file = basename(path),
    source_sheet = "all_tags",
    source_row = raw$source_row,
    coordinate_source = "NAW tag deployment start position (all_tags)",
    comments = collapse_comment(
      if_else(na_text(raw$tag_hit_miss) == "miss", "tag deployment missed", NA_character_),
      if_else(na_text(raw$transmit) == "no", "tag did not transmit", NA_character_),
      if_else(na_text(raw$biopsy) == "yes", "biopsy also collected from this tagging", NA_character_),
      na_text(raw$comment), na_text(raw$comment_2)
    )
  )
}

normalize_biopsy <- function(path) {
  raw <- read_excel(path, sheet = "Biopsy samples", col_types = "text", .name_repair = "unique") %>%
    mutate(source_row = row_number() + 1L)
  raw <- raw %>%
    filter(str_to_upper(na_text(species)) == "NBW", str_to_upper(na_text(location)) == "BB") %>%
    # The single 2017 row (sample NBW01ARC17) is the same biopsy event already
    # normalized from the Pamiut paper forms -- see
    # input/normalized/2017/biopsy/biopsy_2017_standardized.csv
    # (record 2017_paper_biopsy_4, same coordinates and sample id). Excluded
    # here so it isn't counted twice between the two pipelines.
    filter(na_text(year) != "2017")
  if (!nrow(raw)) {
    return(tibble())
  }

  sample_id <- na_text(raw[["sample ID"]])
  datetime <- parse_naw_datetime(raw$date)
  date_unparsed <- is.na(datetime) & !is.na(na_text(raw$date))
  tibble(
    record_id = paste0(raw$year, "_naw_tagbiopsy_biopsy_", coalesce(sample_id, as.character(raw$source_row))),
    source_family = "NAW",
    source_year = as.integer(raw$year),
    survey_leg = NA_character_,
    standard_datetime = datetime,
    standard_date = as.Date(datetime, tz = "UTC"),
    standard_time = if_else(!is.na(datetime), format(datetime, "%H:%M:%S", tz = "UTC"), NA_character_),
    datetime_precision = case_when(
      !is.na(datetime) & str_detect(na_text(raw$date), "[0-9]{1,2}:[0-9]{2}") ~ "minute_or_better",
      !is.na(datetime) ~ "date_only",
      TRUE ~ "unknown"
    ),
    latitude = fix_lat(raw$lat),
    longitude = fix_lon(raw$long),
    count = 1,
    species = "Northern Bottlenose Whale",
    sample_id = sample_id,
    source_file = basename(path),
    source_sheet = "Biopsy samples",
    source_row = raw$source_row,
    coordinate_source = if_else(
      coalesce(as_num(raw$long) > 0, FALSE),
      "NAW biopsy sample position (Biopsy samples; longitude sign corrected from source, which recorded it positive)",
      "NAW biopsy sample position (Biopsy samples)"
    ),
    comments = collapse_comment(
      if_else(date_unparsed, paste0("date not recorded in source (raw value '", na_text(raw$date), "')"), NA_character_),
      na_text(raw$Length), na_text(raw$with_calf),
      if_else(!is.na(na_text(raw$group_size)), paste0("group size: ", na_text(raw$group_size)), NA_character_),
      if_else(!is.na(na_text(raw$collector)), paste0("collector: ", na_text(raw$collector)), NA_character_),
      na_text(raw$comments)
    )
  )
}

data <- bind_rows(normalize_tags(tags_path), normalize_biopsy(biopsy_path))
if (!nrow(data)) stop("No NAW NBW/Baffin Bay tag or biopsy records found")

for (yr in sort(unique(data$source_year))) {
  year_data <- filter(data, source_year == yr)
  out_dir <- file.path(out_root, yr, "sightings")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, paste0("naw_tagbiopsy_", yr, "_standardized.csv"))
  write_csv(year_data, out_file, na = "")
  message(yr, ": ", nrow(year_data), " NAW tag/biopsy rows -> ", out_file)
}
