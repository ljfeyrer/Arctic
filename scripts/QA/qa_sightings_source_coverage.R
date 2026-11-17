# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Create a source coverage table showing row counts, dated rows, coordinate
# rows, and date ranges for original, derived, and normalized sightings files.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(lubridate)
  library(purrr)
})

qa_dir <- "output/Sightings_QA"
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

source("scripts/lib/helpers.R")
get_col <- get_col_best

parse_dt <- function(x) {
  x <- na_text(x)
  serial <- suppressWarnings(as.numeric(x))
  out <- as.POSIXct(rep(NA_character_, length(x)), tz = "UTC")
  ok <- !is.na(serial) & serial > 20000 & serial < 60000
  out[ok] <- as.POSIXct(serial[ok] * 86400, origin = "1899-12-30", tz = "UTC")
  text_ok <- !ok & !is.na(x)
  out[text_ok] <- suppressWarnings(parse_date_time(
    str_replace_all(x[text_ok], "T", " "),
    orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "dmy HMS", "dmy HM"),
    tz = "UTC", quiet = TRUE
  ))
  out
}
combine_date_time <- function(date, time) {
  date_text <- na_text(date)
  time_text <- na_text(time)
  date_serial <- suppressWarnings(as.numeric(date_text))
  date_value <- rep(NA_character_, length(date_text))
  date_ok <- !is.na(date_serial) & date_serial > 20000 & date_serial < 60000
  date_value[date_ok] <- as.character(as.Date(date_serial[date_ok], origin = "1899-12-30"))
  date_value[!date_ok] <- date_text[!date_ok]
  time_serial <- suppressWarnings(as.numeric(time_text))
  time_value <- time_text
  time_ok <- !is.na(time_serial) & time_serial >= 0 & time_serial < 1
  time_value[time_ok] <- format(as.POSIXct(time_serial[time_ok] * 86400,
    origin = "1899-12-30", tz = "UTC"
  ), "%H:%M:%S")
  parse_dt(paste(date_value, time_value))
}
summarize <- function(path, year, source_group, source_role, data, datetime, latitude, longitude, date_value = NULL) {
  date_value <- if (is.null(date_value)) as.Date(datetime, tz = "UTC") else as.Date(date_value)
  tibble(
    source_year = as.integer(year), source_group, source_role,
    source_file = basename(path), source_path = normalizePath(path, winslash = "/", mustWork = FALSE),
    records = nrow(data), dated_records = sum(!is.na(date_value)),
    coordinate_records = sum(!is.na(as_num(latitude)) & !is.na(as_num(longitude))),
    first_date = if (any(!is.na(date_value))) min(date_value, na.rm = TRUE) else as.Date(NA),
    last_date = if (any(!is.na(date_value))) max(date_value, na.rm = TRUE) else as.Date(NA)
  )
}

rows <- list()
add_row <- function(x) rows[[length(rows) + 1L]] <<- x

# Original Kiviuq workbooks.
kiviuq_files <- list.files("input/orig_data",
  pattern = "^Arctic_[0-9]{4}[.]xlsx$",
  recursive = TRUE, full.names = TRUE, ignore.case = TRUE
)
kiviuq_files <- kiviuq_files[str_detect(kiviuq_files, "/kiviuq/cetacean_sightings/")]
for (path in kiviuq_files) {
  year <- as.integer(str_extract(basename(path), "[0-9]{4}(?=[.][^.]+$)"))
  d <- read_excel(path, sheet = 1, col_types = "text", .name_repair = "unique")
  date <- get_col(d, c("Date (UTC)", "Date", "date", "Date_LOCALTIME"))
  time <- get_col(d, c("Time (UTC)", "time"))
  datetime <- if (all(is.na(time))) parse_dt(date) else combine_date_time(date, time)
  add_row(summarize(
    path, year, "Kiviuq", "original_sightings", d, datetime,
    get_col(d, c("latitude", "Latitude", "Lat", "Lat (°N)")),
    get_col(d, c("longitude", "Longitude", "Long", "Long (°E)"))
  ))
}

# Original Tarajoq Cetaceans workbooks.
tarajoq_files <- list.files("input/orig_data",
  pattern = "^Cetaceans_.*[.](xlsx|xls)$",
  recursive = TRUE, full.names = TRUE, ignore.case = TRUE
)
tarajoq_files <- tarajoq_files[str_detect(tarajoq_files, "/sightings_original/")]
for (path in tarajoq_files) {
  year <- as.integer(str_extract(basename(path), "[0-9]{4}(?=[.][^.]+$)"))
  sheets <- excel_sheets(path)
  d <- read_excel(path, sheet = sheets[[1]], col_types = "text", .name_repair = "unique")
  candidates <- c("timestamp", "Enc_start", "DateT_UTC", "DateT", "Local_Time")
  datetime <- parse_dt(get_col(d, candidates))
  if (all(is.na(datetime)) && "Start Date" %in% names(d)) datetime <- parse_dt(paste(d[["Start Date"]], d[["Start Time"]]))
  role <- if (year == 2024 && str_detect(basename(path), regex("Leg2", ignore_case = TRUE))) {
    "original_station_sheet_date_based_leg1"
  } else if (str_detect(basename(path), regex("Leg2", ignore_case = TRUE))) {
    "original_leg2"
  } else {
    "original_cetaceans"
  }
  add_row(summarize(
    path, year, "Tarajoq", role, d, datetime,
    get_col(d, c("Latitude", "latitude")), get_col(d, c("Longitude", "longitude"))
  ))
}

# Existing photo-ID-derived Leg 2 files.
photo_files <- list.files("input/orig_data",
  pattern = "^CetaceanSightings_leg2_[0-9]{4}[.]csv$",
  recursive = TRUE, full.names = TRUE
)
for (path in photo_files) {
  year <- as.integer(str_extract(basename(path), "[0-9]{4}(?=[.][^.]+$)"))
  d <- read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
  datetime <- parse_dt(get_col(d, "DateT_UTC"))
  add_row(summarize(
    path, year, "Tarajoq", "derived_leg2_photo_id", d, datetime,
    get_col(d, "Latitude"), get_col(d, "Longitude")
  ))
}

# Normalized tables from forms, Kiviuq, and Leg 2.
norm_files <- list.files("input/normalized", pattern = "[.]csv$", recursive = TRUE, full.names = TRUE)
norm_files <- norm_files[str_detect(norm_files, "/sightings/")]
norm_files <- norm_files[!str_detect(norm_files, "/archive/")]
for (path in norm_files) {
  d <- read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
  year <- as.integer(str_extract(path, "[0-9]{4}(?=/)"))
  role <- case_when(
    str_detect(path, "kiviuq_sightings_") ~ "normalized_kiviuq",
    str_detect(path, "tarajoq_station_sightings_") ~ "normalized_station_sightings",
    str_detect(path, "tarajoq_photo_id_sightings_") ~ "normalized_photo_id_sightings",
    TRUE ~ "normalized_form_sightings"
  )
  add_row(summarize(
    path, year, if_else(role == "normalized_kiviuq", "Kiviuq", "Tarajoq"), role,
    d, parse_dt(get_col(d, c("standard_datetime", "DateT_UTC"))),
    get_col(d, c("latitude", "Latitude")), get_col(d, c("longitude", "Longitude")),
    get_col(d, "standard_date")
  ))
}

coverage <- bind_rows(rows) %>% arrange(source_year, source_group, source_role, source_file)
write_csv(coverage, file.path(qa_dir, "sightings_source_coverage_audit.csv"), na = "")
coverage_by_year <- coverage %>%
  group_by(source_year, source_group, source_role) %>%
  summarise(
    source_files = n(), records = sum(records), dated_records = sum(dated_records),
    coordinate_records = sum(coordinate_records),
    first_date = if (all(is.na(first_date))) as.Date(NA) else min(first_date, na.rm = TRUE),
    last_date = if (all(is.na(last_date))) as.Date(NA) else max(last_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(source_year, source_group, source_role)
write_csv(coverage_by_year, file.path(qa_dir, "sightings_source_coverage_by_year.csv"), na = "")
message("Wrote ", file.path(qa_dir, "sightings_source_coverage_audit.csv"))
message("Wrote ", file.path(qa_dir, "sightings_source_coverage_by_year.csv"))
print(coverage, n = Inf)
