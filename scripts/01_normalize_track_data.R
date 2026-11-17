# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Normalize and deduplicate yearly ship tracks.
#
# Inputs:
#   input/orig_data/<year>/gps/TimeZero/*.csv
#   input/orig_data/<year>/gps/GPX/*.gpx
#
# Output:
#   input/normalized/<year>/track/track_standardized.csv
#
# The rule follows the existing time0_2025.r workflow: all usable CSV/GPX
# points are combined first, then one record per UTC minute is retained.
# Shapefiles and map products are intentionally deferred to output/Track.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(sf)
  library(readr)
  library(lubridate)
  library(tidyr)
  library(ggplot2)
  library(here)
})

source(here::here("scripts", "lib", "helpers.R"))

year_arg <- get_arg("year", "all")
min_latitude <- as.numeric(get_arg("min-latitude", "60"))
if (is.na(min_latitude)) stop("min-latitude must be numeric")
years <- if (identical(year_arg, "all")) {
  dirs <- list.dirs(here::here("input", "orig_data"), recursive = FALSE, full.names = FALSE)
  sort(dirs[grepl("^[0-9]{4}$", dirs)])
} else {
  if (!grepl("^[0-9]{4}$", year_arg)) stop("year must be a four-digit year or all")
  year_arg
}

parse_dms <- function(x) {
  x <- na_text(x)
  m <- str_match(toupper(x), "^(\\d{1,3})[-D]([0-9.]+)[-D]?([NSEW])$")
  deg <- suppressWarnings(as.numeric(m[, 2]))
  mins <- suppressWarnings(as.numeric(m[, 3])) / 60
  value <- deg + mins
  ifelse(m[, 4] %in% c("S", "W"), -value, value)
}

read_time0_csv <- function(path) {
  message("Reading TimeZero: ", basename(path))
  lines <- readLines(path, warn = FALSE)
  header_line <- which(str_detect(lines, regex("latitude", ignore_case = TRUE)))[1]
  data_start <- which(str_detect(lines, "^\\s*\\d"))[1]
  if (is.na(data_start)) {
    message("  no data rows; skipped")
    return(tibble())
  }

  n_data_cols <- length(str_split(lines[data_start], ",")[[1]])
  if (is.na(header_line)) {
    header_names <- c(
      "Record", "Status", "Latitude", "Longitude", "X1", "Depth", "X2", "X3",
      "Year", "Month", "Day", "Hour", "Minute", "Second", "X4", "X5", "X6"
    )
    if (n_data_cols > length(header_names)) {
      header_names <- c(header_names, paste0("X", seq_len(n_data_cols - length(header_names))))
    }
    header_names <- header_names[seq_len(n_data_cols)]
  } else {
    header_names <- str_split(lines[header_line], ",")[[1]] %>%
      str_trim() %>%
      make.names(unique = TRUE)
    if (n_data_cols > length(header_names)) {
      header_names <- c(header_names, paste0("X", seq_len(n_data_cols - length(header_names))))
    }
  }

  raw <- read.csv(
    path,
    skip = data_start - 1,
    header = FALSE,
    col.names = header_names,
    fill = TRUE,
    stringsAsFactors = FALSE
  )

  if (!all(c("Latitude", "Longitude") %in% names(raw))) {
    return(tibble())
  }

  dt <- as.POSIXct(
    sprintf(
      "%04d-%02d-%02d %02d:%02d:%02d",
      as_num(get_col(raw, "Year")),
      as_num(get_col(raw, "Month")),
      as_num(get_col(raw, "Day")),
      as_num(get_col(raw, "Hour")),
      as_num(get_col(raw, "Minute")),
      as_num(get_col(raw, "Second"))
    ),
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )

  tibble(
    source_file = basename(path),
    source_format = "TimeZero CSV",
    standard_datetime = dt,
    latitude = parse_dms(raw$Latitude),
    longitude = parse_dms(raw$Longitude),
    depth_m = as_num(get_col(raw, "Depth")),
    course_deg = as_num(get_col(raw, c("Course.over.ground", "COG"))),
    speed_over_ground = as_num(get_col(raw, c("Speed.over.ground", "SOG"))),
    heading_deg = as_num(get_col(raw, "Heading")),
    speed_through_water = as_num(get_col(raw, "Speed.through.water")),
    true_wind_speed = as_num(get_col(raw, "True.wind.speed")),
    apparent_wind_speed = as_num(get_col(raw, "Apparent.wind.speed")),
    atmospheric_pressure = as_num(get_col(raw, "Atmospheric.pressure")),
    air_temperature_c = as_num(get_col(raw, "Air.temperature")),
    sea_surface_temperature_c = as_num(get_col(raw, "SST"))
  ) %>%
    filter(!is.na(standard_datetime), !is.na(latitude), !is.na(longitude))
}

read_gpx <- function(path) {
  message("Reading GPX: ", basename(path))
  out <- tryCatch(
    {
      gpx <- st_read(path, layer = "track_points", quiet = TRUE)
      if (nrow(gpx) == 0) {
        return(tibble())
      }
      coords <- st_coordinates(gpx)
      dt <- as.POSIXct(gpx$time, tz = "UTC")
      tibble(
        source_file = basename(path),
        source_format = "GPX",
        standard_datetime = dt,
        latitude = coords[, "Y"],
        longitude = coords[, "X"],
        depth_m = NA_real_, course_deg = NA_real_, speed_over_ground = NA_real_,
        heading_deg = NA_real_, speed_through_water = NA_real_,
        true_wind_speed = NA_real_, apparent_wind_speed = NA_real_,
        atmospheric_pressure = NA_real_, air_temperature_c = NA_real_,
        sea_surface_temperature_c = NA_real_
      ) %>%
        filter(!is.na(standard_datetime), !is.na(latitude), !is.na(longitude))
    },
    error = function(e) {
      warning("Failed to read ", basename(path), ": ", e$message)
      tibble()
    }
  )
  out
}

for (year in years) {
  timezero_dir <- here::here("input", "orig_data", year, "gps", "TimeZero")
  gpx_dir <- here::here("input", "orig_data", year, "gps", "GPX")

  csv_files <- if (dir.exists(timezero_dir)) {
    list.files(timezero_dir,
      pattern = "[.]csv$", full.names = TRUE,
      recursive = TRUE, ignore.case = TRUE
    )
  } else {
    character(0)
  }

  # Only use own-ship tracks: headered Latitude exports and headerless TK
  # track exports. Exclude waypoint, route, group, and example files.
  csv_files <- csv_files[
    !grepl("example|_grp|_rte|_twp|_wpt|survey_grp|survey_rte|survey_twp|survey_wpt",
      basename(csv_files),
      ignore.case = TRUE
    )
  ]
  csv_files <- csv_files[
    !grepl("/(review|archive)(/|$)", csv_files, ignore.case = TRUE)
  ]
  csv_files <- csv_files[vapply(csv_files, function(f) {
    lines <- readLines(f, n = 3, warn = FALSE)
    any(str_detect(lines, regex("latitude", ignore_case = TRUE))) ||
      any(str_detect(lines, "^TK,"))
  }, logical(1))]

  gpx_files <- if (dir.exists(gpx_dir)) {
    list.files(gpx_dir,
      pattern = "[.]gpx$", full.names = TRUE,
      recursive = TRUE, ignore.case = TRUE
    )
  } else {
    character(0)
  }
  gpx_files <- gpx_files[
    !grepl("/(review|archive)(/|$)", gpx_files, ignore.case = TRUE)
  ]

  if (length(csv_files) == 0 && length(gpx_files) == 0) {
    message(year, ": no usable track CSV or GPX files found")
    next
  }

  points <- bind_rows(
    lapply(csv_files, read_time0_csv),
    lapply(gpx_files, read_gpx)
  )

  if (nrow(points) == 0) {
    warning(year, ": inputs contained no usable track points")
    next
  }

  points <- points %>%
    filter(!is.na(standard_datetime), !is.na(latitude), !is.na(longitude)) %>%
    arrange(standard_datetime)

  clipped_below_min_latitude <- sum(points$latitude < min_latitude, na.rm = TRUE)
  points <- points %>% filter(latitude >= min_latitude)

  if (nrow(points) == 0) {
    warning(year, ": all usable track points were below the minimum latitude")
    next
  }

  points <- points %>%
    mutate(
      standard_date = as.Date(standard_datetime, tz = "UTC"),
      standard_time = format(standard_datetime, "%H:%M:%S", tz = "UTC"),
      dedup_minute = floor_date(standard_datetime, unit = "minute"),
      source_year = as.integer(year)
    )

  duplicate_points <- nrow(points) - n_distinct(points$dedup_minute)
  track <- points %>%
    distinct(dedup_minute, .keep_all = TRUE) %>%
    mutate(record_id = paste0(year, "_track_", seq_len(n()))) %>%
    select(
      record_id, standard_datetime, standard_date, standard_time,
      latitude, longitude, depth_m, course_deg, speed_over_ground,
      heading_deg, speed_through_water, true_wind_speed,
      apparent_wind_speed, atmospheric_pressure, air_temperature_c,
      sea_surface_temperature_c, source_year, source_format, source_file,
      dedup_minute
    )

  gaps <- track %>%
    arrange(standard_datetime) %>%
    mutate(gap_hours = as.numeric(difftime(standard_datetime,
      lag(standard_datetime),
      units = "hours"
    ))) %>%
    filter(!is.na(gap_hours), gap_hours > 12)

  out_dir <- here::here("input", "normalized", year, "track")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, paste0("track_", year, "_standardized.csv"))
  write_csv(track, out_file, na = "")

  # Lightweight visual QA stays in output/Track and is not used as an input.
  # The CSV remains WGS84 lon/lat; the map uses North Polar Stereographic
  # (EPSG:3413) so Arctic distances and orientation are not visually distorted.
  track_points_sf <- st_as_sf(
    track,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(3413)
  track_line_sf <- track_points_sf %>%
    st_combine() %>%
    st_cast("LINESTRING")

  qa_dir <- here::here("output", "Track", "QA")
  dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
  qa_map <- ggplot() +
    geom_sf(data = track_line_sf, linewidth = 0.35, colour = "grey35") +
    geom_sf(data = track_points_sf, aes(colour = source_format), size = 0.45, alpha = 0.75) +
    coord_sf(crs = 3413, expand = FALSE) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.margin = margin(8, 8, 8, 8)
    ) +
    labs(
      title = paste("Ship track QA", year),
      subtitle = paste(
        format(min(track$standard_datetime), "%Y-%m-%d %H:%M UTC"),
        "to",
        format(max(track$standard_datetime), "%Y-%m-%d %H:%M UTC"),
        "|", nrow(track), "deduplicated fixes"
      ),
      caption = "North Polar Stereographic projection (EPSG:3413)",
      colour = "Source"
    )
  qa_map_file <- file.path(qa_dir, paste0("ship_track_QA_", year, ".png"))
  ggsave(qa_map_file, qa_map, width = 7, height = 10, units = "in", dpi = 220)

  audit <- tibble(
    year = as.integer(year),
    input_csv_files = length(csv_files),
    input_gpx_files = length(gpx_files),
    raw_points = nrow(points),
    points_clipped_below_min_latitude = clipped_below_min_latitude,
    minimum_latitude_filter = min_latitude,
    deduplicated_points = nrow(track),
    duplicate_points_removed = duplicate_points,
    gap_count_over_12h = nrow(gaps),
    track_start_utc = min(track$standard_datetime),
    track_end_utc = max(track$standard_datetime),
    output_file = normalizePath(out_file, winslash = "/", mustWork = FALSE),
    qa_map_file = normalizePath(qa_map_file, winslash = "/", mustWork = FALSE)
  )
  audit_file <- here::here("output", "Sightings_QA", paste0("track_normalization_audit_", year, ".csv"))
  dir.create(dirname(audit_file), recursive = TRUE, showWarnings = FALSE)
  write_csv(audit, audit_file, na = "")

  if (nrow(gaps) > 0) {
    write_csv(
      gaps,
      here::here("output", "Sightings_QA", paste0("track_gaps_over_12h_", year, ".csv")),
      na = ""
    )
  }
  message(
    year, ": ", nrow(points), " raw points -> ", nrow(track),
    " deduplicated points; gaps >12h: ", nrow(gaps)
  )
}
