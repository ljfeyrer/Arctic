# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-17
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Normalize heterogeneous NAW (New Arctic Whales) Arctic_<year>.xlsx sightings sheets.
# Outputs are separate from Tarajoq form sightings so the source programs are
# not silently mixed.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(lubridate)
  library(purrr)
})

source("scripts/lib/helpers.R")

root <- "input/orig_data"
out_root <- "input/normalized"
qa_root <- "output/Sightings_QA"
dir.create(qa_root, recursive = TRUE, showWarnings = FALSE)

parse_date <- function(x) {
  x <- na_text(x)
  serial <- suppressWarnings(as.numeric(x))
  out <- as.Date(rep(NA_character_, length(x)))
  ok <- !is.na(serial) & serial > 20000 & serial < 60000
  out[ok] <- as.Date(serial[ok], origin = "1899-12-30")
  text_ok <- !ok & !is.na(x)
  out[text_ok] <- suppressWarnings(parse_date_time(
    x[text_ok],
    orders = c("ymd", "mdy", "dmy"), tz = "UTC", quiet = TRUE
  )) %>% as.Date()
  out
}

parse_time <- function(x) {
  x <- na_text(x)
  serial <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok <- !is.na(serial) & serial >= 0 & serial < 1
  out[ok] <- format(as.POSIXct(serial[ok] * 86400, origin = "1899-12-30", tz = "UTC"), "%H:%M:%S")
  text_ok <- !ok & !is.na(x)
  if (any(text_ok)) {
    parsed <- suppressWarnings(parse_date_time(x[text_ok],
      orders = c("HMS", "HM", "H:M:S", "H:M"), tz = "UTC", quiet = TRUE
    ))
    out[text_ok] <- format(parsed, "%H:%M:%S")
  }
  out
}

combine_date_time <- function(date, time) {
  date <- parse_date(date)
  time <- parse_time(time)
  out <- as.POSIXct(rep(NA_character_, length(date)), tz = "UTC")
  ok <- !is.na(date) & !is.na(time)
  out[ok] <- as.POSIXct(paste(date[ok], time[ok]), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  out
}

parse_count_pair <- function(x) {
  x <- na_text(x)
  parts <- str_split(x, ":", simplify = TRUE)
  first <- as_num(parts[, 1])
  second <- if (ncol(parts) > 1) as_num(parts[, 2]) else rep(NA_real_, length(first))
  ifelse(!is.na(second), first + second, first)
}

normalize_file <- function(path) {
  year <- as.integer(str_extract(basename(path), "[0-9]{4}"))
  raw <- read_excel(path, sheet = 1, col_types = "text", .name_repair = "unique")
  date <- get_col(raw, c("Date (UTC)", "Date", "date", "Date_LOCALTIME"))
  time <- get_col(raw, c("Time (UTC)", "time"))
  datetime <- combine_date_time(date, time)
  date_only <- parse_date(date)

  count <- get_col(raw, c("group_size", "Group size", "Number of individuals (adults:calf)"))
  count_num <- if (all(is.na(count))) {
    rep(NA_real_, nrow(raw))
  } else {
    if (any(str_detect(count, fixed(":")), na.rm = TRUE)) parse_count_pair(count) else as_num(count)
  }
  adult <- as_num(get_col(raw, "adult"))
  juvenile <- as_num(get_col(raw, "juvenile"))
  calf <- as_num(get_col(raw, "calf"))
  count_num <- coalesce(count_num, rowSums(cbind(adult, juvenile, calf), na.rm = TRUE))
  count_num[rowSums(!is.na(cbind(adult, juvenile, calf))) == 0 & is.na(as_num(count))] <- NA_real_

  tibble(
    record_id = paste0(year, "_naw_", seq_len(nrow(raw))),
    source_family = "NAW",
    source_year = year,
    survey_leg = NA_character_,
    standard_datetime = datetime,
    standard_date = if_else(!is.na(datetime), as.Date(datetime, tz = "UTC"), date_only),
    standard_time = format(datetime, "%H:%M:%S", tz = "UTC"),
    datetime_precision = if_else(!is.na(datetime), "minute_or_better", "date_only"),
    latitude = as_num(get_col(raw, c("latitude", "Latitude", "Lat", "Lat (°N)"))),
    longitude = as_num(get_col(raw, c("longitude", "Longitude", "Long", "Long (°E)"))),
    count = count_num,
    species = get_col(raw, c("species", "Species")),
    sample_id = get_col(raw, c("sample_id", "Sample_ID")),
    source_file = basename(path),
    source_sheet = "Sheet1",
    source_row = seq_len(nrow(raw)) + 1L,
    coordinate_source = "NAW source sheet",
    comments = get_col(raw, c("comments", "Comments", "Notes"))
  )
}

# Hand-transcribed raw field notes (e.g. a text message or note received
# before any spreadsheet export exists for that year) live alongside the
# normalized sightings at
# input/normalized/<year>/sightings/naw_fieldnotes_<year>_transcription.csv
# with columns note_row, date_raw, standard_date, latitude_raw,
# longitude_raw, latitude, longitude, count, notes, source_file. They are
# combined into the same per-year output as the Arctic_<year>.xlsx sheets,
# with coordinate_source flagged as approximate (hand-estimated positions,
# not a logged GPS fix).
read_fieldnote_transcription <- function(year) {
  path <- file.path(out_root, year, "sightings", paste0("naw_fieldnotes_", year, "_transcription.csv"))
  if (!file.exists(path)) {
    return(tibble())
  }
  raw <- read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
  tibble(
    record_id = paste0(year, "_naw_fieldnote_", raw$note_row),
    source_family = "NAW",
    source_year = as.integer(year),
    survey_leg = NA_character_,
    standard_datetime = as.POSIXct(NA_real_, tz = "UTC"),
    standard_date = as.Date(raw$standard_date),
    standard_time = NA_character_,
    datetime_precision = "date_only",
    latitude = as_num(raw$latitude),
    longitude = as_num(raw$longitude),
    count = as_num(raw$count),
    species = "Northern Bottlenose Whale",
    sample_id = NA_character_,
    source_file = raw$source_file,
    source_sheet = "field_note",
    source_row = raw$note_row,
    coordinate_source = paste0(
      "field note (approximate position: ", raw$latitude_raw, ", ", raw$longitude_raw, ")"
    ),
    comments = raw$notes
  )
}

xlsx_files <- list.files(root,
  pattern = "^Arctic_[0-9]{4}[.]xlsx$", recursive = TRUE,
  full.names = TRUE, ignore.case = TRUE
)
# Most years file this under naw/cetacean_sightings/, but 2023's workbook
# sits directly under naw/ instead -- match on the unambiguous
# Arctic_<year>.xlsx filename and just require a /naw/ ancestor folder, so
# that inconsistency doesn't silently drop a year.
xlsx_files <- xlsx_files[str_detect(xlsx_files, "/naw/")]
xlsx_years <- str_extract(basename(xlsx_files), "[0-9]{4}")

fieldnote_years <- list.dirs(root, recursive = FALSE, full.names = FALSE) %>%
  str_subset("^[0-9]{4}$") %>%
  keep(~ file.exists(file.path(out_root, .x, "sightings", paste0("naw_fieldnotes_", .x, "_transcription.csv"))))

years <- sort(union(xlsx_years, fieldnote_years))
if (length(years) == 0) stop("No NAW Arctic_<year>.xlsx files or field-note transcriptions found")

for (year in years) {
  xlsx_path <- xlsx_files[xlsx_years == year]
  data <- bind_rows(
    if (length(xlsx_path)) normalize_file(xlsx_path[1]) else tibble(),
    read_fieldnote_transcription(year)
  )
  out_dir <- file.path(out_root, year, "sightings")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, paste0("naw_sightings_", year, "_standardized.csv"))
  write_csv(data, out_file, na = "")
  message(year, ": ", nrow(data), " NAW rows -> ", out_file)
}
