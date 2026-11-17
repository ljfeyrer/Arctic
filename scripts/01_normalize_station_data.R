# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Normalize yearly Tarajoq station-position sources into one point table.
# Original station CSVs/workbooks are not modified. Shapefiles and maps belong
# to the later output stage.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(lubridate)
  library(here)
})

source(here::here("scripts", "lib", "helpers.R"))

year_arg <- get_arg("year", "all")
year_dirs <- list.dirs(here::here("input", "orig_data"), recursive = FALSE, full.names = FALSE)
years <- if (identical(year_arg, "all")) {
  sort(year_dirs[grepl("^[0-9]{4}$", year_dirs)])
} else {
  if (!grepl("^[0-9]{4}$", year_arg)) stop("year must be a four-digit year or all")
  year_arg
}

first_value <- function(x) {
  x <- na_text(x)
  hit <- x[!is.na(x)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

parse_dms_pair <- function(degrees, minutes, longitude = FALSE) {
  deg <- as_num(degrees)
  mins <- as_num(minutes)
  value <- deg + mins / 60
  if (longitude) -value else value
}

parse_datetime <- function(date, time) {
  date <- na_text(date)
  time <- na_text(time)
  out <- suppressWarnings(parse_date_time(
    paste(date, time),
    orders = c(
      "dmy I:M:S p", "mdy I:M:S p", "ymd I:M:S p",
      "dmy I:M p", "mdy I:M p", "ymd I:M p",
      "dmy HMS", "mdy HMS", "ymd HMS", "dmy HM", "mdy HM", "ymd HM"
    ),
    tz = "UTC",
    quiet = TRUE
  ))
  as.POSIXct(out, tz = "UTC")
}

read_station_file <- function(path, fallback_year) {
  ext <- tolower(tools::file_ext(path))
  data <- if (ext %in% c("xls", "xlsx")) {
    read_excel(path, sheet = 1, col_types = "text", .name_repair = "unique")
  } else {
    read_csv(path, col_types = cols(.default = col_character()), name_repair = "unique")
  }
  names(data) <- str_replace(names(data), "^\ufeff", "")

  lat_direct <- as_num(get_col(data, c("Latitude", "Start Latitude")))
  lon_direct <- as_num(get_col(data, c("Longitude", "Start Longitude")))
  lat_start <- coalesce(
    lat_direct,
    parse_dms_pair(
      get_col(data, c("Start Lat Degree", "Start Latitude Degree")),
      get_col(data, c("Start Lat Minutes", "Start Latitude Minutes"))
    )
  )
  lon_start <- coalesce(
    lon_direct,
    parse_dms_pair(get_col(data, c("Start Long Degree", "Start Longitude Degree")),
      get_col(data, c("Start Long Minutes", "Start Longitude Minutes")),
      longitude = TRUE
    )
  )

  tibble(
    source_file = basename(path),
    source_year = as.integer(coalesce(as_num(get_col(data, "Year")), as_num(fallback_year))),
    trawl_id = get_col(data, c("Trawl_ID", "Trawl ID")),
    set_number = as_num(get_col(data, c("Set Number", "Set"))),
    fix_pos = get_col(data, c("Fix Pos", "FixPos")),
    station_start_date = na_text(get_col(data, c("Start Date", "Date"))),
    station_start_time = na_text(get_col(data, c("Start Time", "Time"))),
    station_end_date = na_text(get_col(data, c("End Date"))),
    station_end_time = na_text(get_col(data, c("End Time"))),
    station_start_datetime = parse_datetime(
      get_col(data, c("Start Date", "Date")),
      get_col(data, c("Start Time", "Time"))
    ),
    station_end_datetime = parse_datetime(
      get_col(data, c("End Date")),
      get_col(data, c("End Time"))
    ),
    standard_datetime = coalesce(station_end_datetime, station_start_datetime),
    latitude = lat_start,
    longitude = lon_start,
    end_latitude = parse_dms_pair(
      get_col(data, "End Lat Degree"),
      get_col(data, "End Lat Minutes")
    ),
    end_longitude = parse_dms_pair(get_col(data, "End Long Degree"),
      get_col(data, "End Long Minutes"),
      longitude = TRUE
    ),
    start_depth_m = as_num(get_col(data, c("Start Depth", "Start depth"))),
    end_depth_m = as_num(get_col(data, c("End Depth", "End depth"))),
    stratum = get_col(data, "Stratum"),
    study_area = get_col(data, c("Study Area", "Study_area")),
    ship_id = get_col(data, "Ship ID"),
    trip = get_col(data, "Trip")
  )
}

for (year in years) {
  station_dir <- here::here("input", "orig_data", year, "station_ids")
  if (!dir.exists(station_dir)) next

  files <- list.files(station_dir, full.names = TRUE, recursive = FALSE)
  files <- files[!dir.exists(files) & grepl("[.](csv|xls|xlsx)$", files, ignore.case = TRUE)]
  if (length(files) == 0) next

  records <- bind_rows(lapply(files, read_station_file, fallback_year = year)) %>%
    mutate(
      across(
        c(
          source_year, set_number, latitude, longitude,
          end_latitude, end_longitude, start_depth_m, end_depth_m
        ),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) %>%
    filter(
      !is.na(latitude), !is.na(longitude),
      between(latitude, 0, 90), between(longitude, -180, 0)
    )

  if (nrow(records) == 0) {
    message(year, ": station files found, but no usable coordinate records")
    next
  }

  points <- records %>%
    mutate(station_key = paste(source_year, trawl_id, set_number, fix_pos, sep = "|")) %>%
    group_by(station_key) %>%
    summarise(
      source_year = first_value(source_year),
      trawl_id = first_value(trawl_id),
      set_number = first_value(set_number),
      fix_pos = first_value(fix_pos),
      coordinate_versions = n_distinct(round(latitude, 6), round(longitude, 6)),
      coordinate_conflict = coordinate_versions > 1,
      station_start_date = first_value(station_start_date),
      station_start_time = first_value(station_start_time),
      station_end_date = first_value(station_end_date),
      station_end_time = first_value(station_end_time),
      station_start_datetime = station_start_datetime[!is.na(station_start_datetime)][1],
      station_end_datetime = station_end_datetime[!is.na(station_end_datetime)][1],
      standard_datetime = standard_datetime[!is.na(standard_datetime)][1],
      latitude = first_value(latitude),
      longitude = first_value(longitude),
      end_latitude = first_value(end_latitude),
      end_longitude = first_value(end_longitude),
      start_depth_m = first_value(start_depth_m),
      end_depth_m = first_value(end_depth_m),
      stratum = first_value(stratum),
      study_area = first_value(study_area),
      ship_id = first_value(ship_id),
      trip = first_value(trip),
      source_files = paste(unique(source_file), collapse = "; "),
      source_record_count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      source_year = as.integer(source_year),
      set_number = as.integer(set_number),
      record_id = paste0(source_year, "_station_", row_number()),
      datetime_basis = if_else(!is.na(station_end_datetime),
        "station end time", "station start time (end unavailable)"
      ),
      datetime_timezone = "UTC (station source; no offset conversion)",
      station_date = as.Date(standard_datetime),
      station_time = format(standard_datetime, "%H:%M:%S", tz = "UTC"),
      standard_date = as.Date(standard_datetime),
      standard_time = format(standard_datetime, "%H:%M:%S", tz = "UTC")
    ) %>%
    select(
      record_id, source_year, trawl_id, set_number, fix_pos,
      station_date, station_time, station_start_date, station_start_time,
      station_end_date, station_end_time, station_start_datetime,
      station_end_datetime, standard_datetime, datetime_basis,
      datetime_timezone,
      standard_date, standard_time,
      latitude, longitude, end_latitude, end_longitude,
      start_depth_m, end_depth_m, stratum, study_area, ship_id, trip,
      source_files, source_record_count, coordinate_versions, coordinate_conflict
    ) %>%
    arrange(source_year, set_number, trawl_id)

  out_dir <- here::here("input", "normalized", year, "stations")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, paste0("stations_", year, "_standardized.csv"))
  write_csv(points, out_file, na = "")

  audit <- tibble(
    year = as.integer(year),
    source_files = length(files),
    source_records_with_coordinates = nrow(records),
    normalized_station_points = nrow(points),
    coordinate_conflicts = sum(points$coordinate_conflict),
    output_file = normalizePath(out_file, winslash = "/", mustWork = FALSE)
  )
  audit_file <- here::here("output", "Sightings_QA", paste0("station_normalization_audit_", year, ".csv"))
  dir.create(dirname(audit_file), recursive = TRUE, showWarnings = FALSE)
  write_csv(audit, audit_file, na = "")
  message(year, ": ", nrow(records), " coordinate records -> ", nrow(points), " station points")
}
