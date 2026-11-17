# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Validate photo-ID sighting coordinates against ship tracks. This script
# never fills missing coordinates and never overwrites the normalized sightings.
# It tests a small set of possible camera-clock UTC offsets so a systematic
# offset can be reviewed before any duplicate decision is made.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(stringr)
  library(purrr)
})

source("scripts/lib/helpers.R")

qa_dir <- "output/Sightings_QA"
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

# Year-specific provenance and QA assumptions. These notes travel with the
# row-level output so a future year cannot silently inherit another year's
# timestamp interpretation.
year_rules <- tibble(
  source_year = 2022:2026,
  qa_rule_set = paste0("photo_station_qa_", 2022:2026),
  preferred_camera_offset_hours = c(NA_real_, -1, 1, NA_real_, NA_real_),
  station_candidate_priority = "At the preferred camera offset, minimize station-time difference first; use distance as supporting evidence.",
  station_time_rule = "Station timestamps are UTC; use end time when available, otherwise start time.",
  station_match_window = "Up to 12 hours for station candidate review; report time-only matches when photo coordinates are missing.",
  camera_offset_rule = "Test camera/photo offsets from -4 to +4 hours; offsets are diagnostic only and do not alter normalized data.",
  year_specific_note = c(
    "2022: station/photo comparison uses normalized UTC fields; verify source photo timestamp provenance.",
    "2023: station files have start times but no end times; Date Time is used for photo encounter grouping and Date Created ISO offset for UTC photo timestamps. Use a consistent -1 hour camera offset for review; station positions may lag whale sightings during deep-station operations. The St. 161 AM/PM issue is resolved in the reconciliation QA using station time.",
    "2024: Leg 2 photo dates overlap station dates but not the available ship track; station comparison is the primary validation.",
    "2025: use station/trawl end times when present; photo-ID station QA applies only if a normalized photo source is present.",
    "2026: use the same UTC station rule; any photo source requires explicit timestamp provenance review."
  )
)

parse_datetime <- function(x) {
  suppressWarnings(parse_date_time(
    str_replace_all(as.character(x), "T", " "),
    orders = c("ymd HMS", "ymd HM", "ymd"), tz = "UTC", quiet = TRUE
  ))
}

best_reference_match <- function(reference, target_time, target_lat, target_lon,
                                 offsets, window_seconds, prioritize_time = FALSE) {
  if (prioritize_time) {
    candidates <- map_dfr(offsets, function(offset) {
      adjusted <- target_time + hours(offset)
      time_diff <- abs(as.numeric(difftime(reference$reference_datetime, adjusted, units = "secs")))
      in_window <- which(time_diff <= window_seconds)
      if (length(in_window) == 0) {
        return(tibble(
          offset_hours_added = offset,
          nearest_distance_m = NA_real_,
          nearest_time_difference_seconds = NA_real_,
          nearest_reference_id = NA_character_
        ))
      }
      distance <- if (is.na(target_lat) || is.na(target_lon)) {
        rep(NA_real_, length(in_window))
      } else {
        haversine_m(
          target_lat, target_lon,
          reference$reference_latitude[in_window],
          reference$reference_longitude[in_window]
        )
      }
      tibble(
        offset_hours_added = offset,
        nearest_distance_m = distance,
        nearest_time_difference_seconds = time_diff[in_window],
        nearest_reference_id = reference$reference_id[in_window]
      )
    })
    return(candidates %>%
      arrange(
        is.na(nearest_time_difference_seconds), nearest_time_difference_seconds,
        is.na(nearest_distance_m), nearest_distance_m
      ) %>%
      slice(1))
  }

  candidates <- map_dfr(offsets, function(offset) {
    adjusted <- target_time + hours(offset)
    time_diff <- abs(as.numeric(difftime(reference$reference_datetime, adjusted, units = "secs")))
    in_window <- which(time_diff <= window_seconds)
    if (length(in_window) == 0) {
      return(tibble(
        offset_hours_added = offset, nearest_distance_m = NA_real_,
        nearest_time_difference_seconds = NA_real_,
        nearest_reference_id = NA_character_
      ))
    }
    if (is.na(target_lat) || is.na(target_lon)) {
      return(tibble(
        offset_hours_added = offset,
        nearest_distance_m = NA_real_,
        nearest_time_difference_seconds = min(time_diff[in_window]),
        nearest_reference_id = reference$reference_id[in_window[which.min(time_diff[in_window])]]
      ))
    }
    distance <- haversine_m(
      target_lat, target_lon,
      reference$reference_latitude[in_window],
      reference$reference_longitude[in_window]
    )
    best <- in_window[which.min(distance)]
    tibble(
      offset_hours_added = offset,
      nearest_distance_m = haversine_m(
        target_lat, target_lon,
        reference$reference_latitude[best],
        reference$reference_longitude[best]
      ),
      nearest_time_difference_seconds = time_diff[best],
      nearest_reference_id = reference$reference_id[best]
    )
  })
  if (prioritize_time) {
    candidates %>%
      arrange(
        is.na(nearest_time_difference_seconds), nearest_time_difference_seconds,
        is.na(nearest_distance_m), nearest_distance_m
      ) %>%
      slice(1)
  } else {
    candidates %>%
      arrange(is.na(nearest_distance_m), nearest_distance_m, nearest_time_difference_seconds) %>%
      slice(1)
  }
}

nuuk_local_minus_utc_hours <- function(utc_time) {
  # Convert the IANA-zone local clock label back to UTC clock time. This
  # captures the historical Nuuk offset for the date instead of hardcoding a
  # single offset across survey years.
  local_label <- format(utc_time, tz = "America/Nuuk", usetz = FALSE)
  local_as_utc <- as.POSIXct(local_label, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  as.numeric(difftime(local_as_utc, utc_time, units = "hours"))
}

years <- list.dirs("input/normalized", recursive = FALSE, full.names = FALSE)
years <- years[grepl("^[0-9]{4}$", years)]
results <- list()
coverage_results <- list()

for (year in years) {
  sighting_file <- file.path(
    "input", "normalized", year, "sightings",
    paste0("tarajoq_photo_id_sightings_", year, "_standardized.csv")
  )
  track_file <- file.path(
    "input", "normalized", year, "track",
    paste0("track_", year, "_standardized.csv")
  )
  if (!file.exists(sighting_file) || !file.exists(track_file)) next

  sightings <- read_csv(sighting_file, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(
      photo_datetime = parse_datetime(standard_datetime),
      latitude = as.numeric(latitude), longitude = as.numeric(longitude)
    ) %>%
    filter(!is.na(photo_datetime))
  track <- read_csv(track_file, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(
      track_datetime = parse_datetime(standard_datetime),
      track_latitude = as.numeric(latitude), track_longitude = as.numeric(longitude)
    ) %>%
    filter(!is.na(track_datetime), !is.na(track_latitude), !is.na(track_longitude))
  if (nrow(sightings) == 0 || nrow(track) == 0) next

  station_file <- file.path(
    "input", "normalized", year, "stations",
    paste0("stations_", year, "_standardized.csv")
  )
  stations <- if (file.exists(station_file)) {
    read_csv(station_file, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
      filter(as.integer(source_year) == as.integer(year)) %>%
      transmute(
        reference_id = coalesce(
          record_id,
          paste0("station_", set_number, "_", trawl_id)
        ),
        reference_datetime = parse_datetime(standard_datetime),
        reference_latitude = as.numeric(latitude),
        reference_longitude = as.numeric(longitude)
      ) %>%
      filter(!is.na(reference_datetime), !is.na(reference_latitude), !is.na(reference_longitude))
  } else {
    tibble(
      reference_datetime = as.POSIXct(character()),
      reference_latitude = numeric(), reference_longitude = numeric(),
      reference_id = character()
    )
  }

  photo_dates <- as.Date(sightings$photo_datetime)
  track_dates <- as.Date(track$track_datetime)
  station_dates <- as.Date(stations$reference_datetime)
  track_overlap <- min(photo_dates) <= max(track_dates) && max(photo_dates) >= min(track_dates)
  station_overlap <- nrow(stations) > 0 &&
    min(photo_dates) <= max(station_dates) && max(photo_dates) >= min(station_dates)
  coverage_results[[length(coverage_results) + 1L]] <- tibble(
    source_year = as.integer(year),
    photo_id_first_date = min(photo_dates), photo_id_last_date = max(photo_dates),
    track_first_date = min(track_dates), track_last_date = max(track_dates),
    track_date_overlap = track_overlap,
    station_first_date = if (nrow(stations) > 0) min(station_dates) else as.Date(NA),
    station_last_date = if (nrow(stations) > 0) max(station_dates) else as.Date(NA),
    station_date_overlap = station_overlap
  )

  offsets <- seq(-4, 4, by = 1)
  for (i in seq_len(nrow(sightings))) {
    track_reference <- track %>%
      mutate(reference_id = paste0("track_", row_number())) %>%
      transmute(reference_id,
        reference_datetime = track_datetime,
        reference_latitude = track_latitude,
        reference_longitude = track_longitude
      )
    best <- if (!is.na(sightings$latitude[i]) && !is.na(sightings$longitude[i])) {
      best_reference_match(track_reference, sightings$photo_datetime[i],
        sightings$latitude[i], sightings$longitude[i],
        offsets,
        window_seconds = 600
      )
    } else {
      tibble(
        offset_hours_added = NA_real_, nearest_distance_m = NA_real_,
        nearest_time_difference_seconds = NA_real_,
        nearest_reference_id = NA_character_
      )
    }
    station_best <- if (nrow(stations) > 0) {
      best_reference_match(stations, sightings$photo_datetime[i], sightings$latitude[i],
        sightings$longitude[i], offsets,
        window_seconds = 43200,
        prioritize_time = FALSE
      )
    } else {
      tibble(
        offset_hours_added = NA_real_, nearest_distance_m = NA_real_,
        nearest_time_difference_seconds = NA_real_,
        nearest_reference_id = NA_character_
      )
    }
    station_time_best <- if (nrow(stations) > 0) {
      best_reference_match(stations, sightings$photo_datetime[i], sightings$latitude[i],
        sightings$longitude[i], offsets,
        window_seconds = 43200,
        prioritize_time = TRUE
      )
    } else {
      tibble(
        offset_hours_added = NA_real_, nearest_distance_m = NA_real_,
        nearest_time_difference_seconds = NA_real_,
        nearest_reference_id = NA_character_
      )
    }
    rule_row <- year_rules[match(as.integer(year), year_rules$source_year), ]
    preferred_offset <- rule_row$preferred_camera_offset_hours[[1]]
    preferred_station <- if (nrow(stations) > 0 && !is.na(preferred_offset)) {
      best_reference_match(stations, sightings$photo_datetime[i], sightings$latitude[i],
        sightings$longitude[i],
        offsets = preferred_offset,
        window_seconds = 43200, prioritize_time = TRUE
      )
    } else {
      tibble(
        offset_hours_added = NA_real_, nearest_distance_m = NA_real_,
        nearest_time_difference_seconds = NA_real_,
        nearest_reference_id = NA_character_
      )
    }

    results[[length(results) + 1L]] <- tibble(
      source_year = as.integer(year), record_id = sightings$record_id[i],
      qa_rule_set = rule_row$qa_rule_set[[1]],
      preferred_camera_offset_hours = preferred_offset,
      station_candidate_priority = rule_row$station_candidate_priority[[1]],
      station_time_rule = rule_row$station_time_rule[[1]],
      station_match_window = rule_row$station_match_window[[1]],
      camera_offset_rule = rule_row$camera_offset_rule[[1]],
      year_specific_note = rule_row$year_specific_note[[1]],
      photo_datetime_utc = sightings$photo_datetime[i], latitude = sightings$latitude[i],
      longitude = sightings$longitude[i], coordinate_use = "validation_only",
      nuuk_local_minus_utc_hours = nuuk_local_minus_utc_hours(sightings$photo_datetime[i]),
      best_offset_hours_added = best$offset_hours_added,
      closest_track_reference_id = best$nearest_reference_id,
      best_track_distance_m = best$nearest_distance_m,
      best_track_time_difference_seconds = best$nearest_time_difference_seconds,
      track_date_overlap = track_overlap,
      track_within_3km = !is.na(best$nearest_distance_m) & best$nearest_distance_m <= 3000,
      best_station_offset_hours_added = station_best$offset_hours_added,
      closest_station_reference_id = station_best$nearest_reference_id,
      best_station_distance_m = station_best$nearest_distance_m,
      best_station_spatial_time_difference_seconds = station_best$nearest_time_difference_seconds,
      station_date_overlap = station_overlap,
      station_within_5km = !is.na(station_best$nearest_distance_m) & station_best$nearest_distance_m <= 5000,
      best_station_time_offset_hours_added = station_time_best$offset_hours_added,
      closest_station_time_reference_id = station_time_best$nearest_reference_id,
      best_station_time_distance_m = station_time_best$nearest_distance_m,
      best_station_time_difference_seconds = station_time_best$nearest_time_difference_seconds,
      station_time_within_2hr = !is.na(station_time_best$nearest_time_difference_seconds) &
        station_time_best$nearest_time_difference_seconds <= 7200,
      preferred_station_reference_id = preferred_station$nearest_reference_id,
      preferred_station_distance_m = preferred_station$nearest_distance_m,
      preferred_station_time_difference_seconds = preferred_station$nearest_time_difference_seconds,
      station_match_type = case_when(
        is.na(station_time_best$nearest_time_difference_seconds) ~ "no_station_time_match",
        is.na(station_best$nearest_distance_m) ~ "time_only_station_match",
        station_time_best$nearest_time_difference_seconds <= 7200 &
          station_time_best$nearest_distance_m <= 5000 ~ "station_time_and_location_match",
        station_time_best$nearest_time_difference_seconds <= 7200 ~ "station_time_match_location_over_5km",
        TRUE ~ "station_time_review"
      )
    )
  }
}

out <- bind_rows(results)
out_file <- file.path(qa_dir, "photo_id_against_track_offset_review.csv")
write_csv(out, out_file, na = "")
write_csv(year_rules, file.path(qa_dir, "photo_id_qa_rules_by_year.csv"), na = "")
coverage <- bind_rows(coverage_results)
coverage_file <- file.path(qa_dir, "photo_id_reference_date_coverage.csv")
write_csv(coverage, coverage_file, na = "")
message("Wrote ", out_file)
message("Wrote ", coverage_file)
if (nrow(coverage) > 0) print(coverage, n = Inf)
if (nrow(out) > 0) {
  print(out %>% count(source_year, best_offset_hours_added, track_within_3km), n = Inf)
  print(out %>% count(source_year, best_station_offset_hours_added, station_within_5km), n = Inf)
}
