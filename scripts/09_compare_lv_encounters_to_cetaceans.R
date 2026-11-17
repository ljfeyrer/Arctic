#!/usr/bin/env Rscript

# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: Compare each year's LV photo encounters with that year's
# compiled Tarajoq Cetaceans sightings, and list the photo encounters that
# have no sighting nearby in time and space -- candidates to add to the
# sightings record. This is a review product only: nothing here is added to
# the compiled sightings automatically.
#
# An encounter counts as already in the sightings when a compiled sighting
# falls within --max-minutes (default 60) of the encounter's time span and
# within --max-km (default 5) of its mean photo position.
#
# Clocks: the compiled sightings mix time bases (form times follow the
# tablet's clock, photo-ID sightings are true UTC), and the LV exports only
# record a UTC offset in some years. Each encounter is therefore compared on
# both its UTC time and its camera-clock time, and the closer of the two is
# kept, with the basis recorded. This can only make a match more likely, so
# an encounter listed as a candidate has no sighting within the window on
# either clock.
#
# Must run after 02_normalize_lv_encounters.R and
# 03_compile_normalized_tarajoq_sightings.R. Run with --year=YYYY or
# --year=all (default). The all-years candidate file is always rebuilt from
# every per-year comparison file on disk.
#
# Inputs (never modified):
#   input/normalized/<year>/LV_encounters/lv_encounters_<year>_standardized.csv
#   output/Cetaceans/tarajoq_nbw_sightings_<year>_{compiled_deduplicated,deduplicated}.csv
#
# Outputs:
#   output/LV_encounters/lv_encounters_vs_cetaceans_<year>.csv
#   output/LV_encounters/lv_encounter_candidate_additions_compiled.csv
#   output/LV_encounters/lv_encounters_vs_cetaceans_summary.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
})

source("scripts/lib/helpers.R")

normalized_root <- "input/normalized"
cetaceans_root <- "output/Cetaceans"
out_dir <- "output/LV_encounters"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

year_arg <- get_arg("year", "all")
max_minutes <- as.numeric(get_arg("max-minutes", "60"))
max_km <- as.numeric(get_arg("max-km", "5"))
if (is.na(max_minutes) || is.na(max_km)) stop("--max-minutes and --max-km must be numeric")

encounter_path <- function(year) {
  file.path(normalized_root, year, "LV_encounters", paste0("lv_encounters_", year, "_standardized.csv"))
}
available_years <- list.dirs(normalized_root, recursive = FALSE, full.names = FALSE) %>%
  str_subset("^[0-9]{4}$") %>%
  keep(~ file.exists(encounter_path(.x))) %>%
  sort()
years <- if (identical(year_arg, "all")) available_years else intersect(year_arg, available_years)
if (!length(years)) stop("No normalized LV encounters found. Run 02_normalize_lv_encounters.R first.")

# Minutes between a sighting time and an encounter's time span: zero when the
# sighting falls inside the span, otherwise the gap to the nearer end.
minutes_from_span <- function(sighting_time, span_start, span_end) {
  gap <- pmax(
    as.numeric(difftime(span_start, sighting_time, units = "mins")),
    as.numeric(difftime(sighting_time, span_end, units = "mins")),
    0
  )
  gap
}

compare_year <- function(year) {
  encounters <- read_csv(encounter_path(year), col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(
      start_utc = ymd_hms(standard_datetime, tz = "UTC", quiet = TRUE),
      end_utc = ymd_hms(encounter_end_datetime, tz = "UTC", quiet = TRUE),
      start_clock = ymd_hms(camera_clock_start, tz = "UTC", quiet = TRUE),
      end_clock = ymd_hms(camera_clock_end, tz = "UTC", quiet = TRUE),
      latitude = as_num(latitude), longitude = as_num(longitude)
    )

  sightings_file <- resolve_dedup_file(cetaceans_root, "tarajoq_nbw_sightings", year)
  sightings <- if (is.na(sightings_file)) {
    tibble()
  } else {
    read_csv(sightings_file, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
      transmute(
        sighting_id = record_id, sighting_source_type = source_type,
        sighting_datetime = as_datetime(standard_datetime, tz = "UTC"),
        sighting_date = as.Date(standard_date),
        sighting_latitude = as_num(latitude), sighting_longitude = as_num(longitude)
      )
  }

  result <- map_dfr(seq_len(nrow(encounters)), function(i) {
    e <- encounters[i, ]
    base <- tibble(
      record_id = e$record_id, source_year = as.integer(year),
      match_status = NA_character_, candidate_addition = NA,
      matched_sighting_id = NA_character_, matched_sighting_source_type = NA_character_,
      matched_sighting_datetime = NA_character_, time_difference_minutes = NA_real_,
      distance_km = NA_real_, clock_basis = NA_character_,
      sightings_within_time_window = 0L
    )
    if (!nrow(sightings)) {
      return(mutate(base,
        match_status = "no_compiled_sightings_for_year", candidate_addition = TRUE
      ))
    }

    gap_utc <- minutes_from_span(sightings$sighting_datetime, e$start_utc, e$end_utc)
    gap_clock <- minutes_from_span(sightings$sighting_datetime, e$start_clock, e$end_clock)
    pairs <- sightings %>%
      mutate(
        time_difference_minutes = pmin(gap_utc, gap_clock, na.rm = TRUE),
        clock_basis = case_when(
          is.na(gap_utc) & is.na(gap_clock) ~ NA_character_,
          # The camera clock is recorded to the minute, so sub-minute
          # differences between the two do not indicate different clocks.
          !is.na(gap_utc) & !is.na(gap_clock) & abs(gap_utc - gap_clock) < 1.5 ~ "utc and camera clock agree",
          is.na(gap_clock) | (!is.na(gap_utc) & gap_utc < gap_clock) ~ "utc",
          TRUE ~ "camera clock"
        ),
        distance_km = haversine_m(e$latitude, e$longitude, sighting_latitude, sighting_longitude) / 1000,
        in_time = !is.na(time_difference_minutes) & time_difference_minutes <= max_minutes,
        in_distance = !is.na(distance_km) & distance_km <= max_km,
        # A sighting with a date but no time can only be placed by position.
        same_date_no_time = is.na(sighting_datetime) & !is.na(sighting_date) &
          (sighting_date == as.Date(e$start_utc) | sighting_date == as.Date(e$start_clock))
      )

    pick <- function(rows, status, candidate) {
      best <- rows %>%
        arrange(time_difference_minutes, distance_km) %>%
        slice(1)
      mutate(base,
        match_status = status, candidate_addition = candidate,
        matched_sighting_id = best$sighting_id,
        matched_sighting_source_type = best$sighting_source_type,
        matched_sighting_datetime = format(best$sighting_datetime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        time_difference_minutes = round(best$time_difference_minutes, 1),
        distance_km = round(best$distance_km, 2),
        clock_basis = best$clock_basis,
        sightings_within_time_window = sum(pairs$in_time)
      )
    }

    if (any(pairs$in_time & pairs$in_distance)) {
      pick(filter(pairs, in_time, in_distance), "matched_within_time_and_distance", FALSE)
    } else if (any(pairs$in_time & is.na(pairs$distance_km))) {
      pick(filter(pairs, in_time, is.na(distance_km)), "matched_in_time_position_unavailable", FALSE)
    } else if (any(pairs$same_date_no_time & pairs$in_distance)) {
      pick(filter(pairs, same_date_no_time, in_distance), "matched_same_date_and_distance", FALSE)
    } else if (any(pairs$in_time)) {
      pick(filter(pairs, in_time), "sighting_in_time_but_beyond_distance", TRUE)
    } else {
      # Nothing in the window: report the nearest sighting in time for context.
      nearest <- pairs %>% filter(!is.na(time_difference_minutes))
      if (nrow(nearest)) {
        mutate(pick(nearest, "no_sighting_within_time_window", TRUE), sightings_within_time_window = 0L)
      } else {
        mutate(base, match_status = "no_sighting_within_time_window", candidate_addition = TRUE)
      }
    }
  })

  encounters %>%
    select(
      record_id, standard_datetime, standard_date, encounter_end_datetime,
      camera_clock_start, camera_clock_end, utc_offset_source, duration_minutes,
      latitude, longitude, position_spread_km, photo_count, photos_3star_plus,
      max_rating_stars, nbw_keyword_photos, species_basis, first_photo, last_photo, source_file
    ) %>%
    left_join(result, by = "record_id") %>%
    relocate(source_year, match_status, candidate_addition, .after = record_id) %>%
    mutate(max_minutes_rule = max_minutes, max_km_rule = max_km)
}

for (year in years) {
  compared <- compare_year(year)
  write_csv(compared, file.path(out_dir, paste0("lv_encounters_vs_cetaceans_", year, ".csv")), na = "")
  message(
    year, ": ", nrow(compared), " LV encounters; ",
    sum(!compared$candidate_addition), " already in sightings, ",
    sum(compared$candidate_addition), " candidate(s) to review"
  )
}

# ---- ALL-YEARS CANDIDATES AND SUMMARY ---------------------------------------

per_year_files <- list.files(out_dir, pattern = "^lv_encounters_vs_cetaceans_[0-9]{4}[.]csv$", full.names = TRUE)
all_compared <- map_dfr(per_year_files, read_csv, col_types = cols(.default = col_character()), show_col_types = FALSE)

candidates <- all_compared %>%
  filter(candidate_addition == "TRUE") %>%
  mutate(
    review_note = case_when(
      match_status == "sighting_in_time_but_beyond_distance" ~
        "A sighting was logged at this time but farther away than the distance rule; check whether it is the same group or a position error.",
      match_status == "no_compiled_sightings_for_year" ~
        "No compiled Cetaceans sightings exist for this year.",
      is.na(latitude) ~ "No sighting in the time window; photos have no GPS position.",
      TRUE ~ "No sighting in the time window."
    )
  ) %>%
  arrange(source_year, standard_datetime)
write_csv(candidates, file.path(out_dir, "lv_encounter_candidate_additions_compiled.csv"), na = "")

summary_table <- all_compared %>%
  count(source_year, match_status, candidate_addition, name = "encounters") %>%
  arrange(source_year, candidate_addition, match_status)
write_csv(summary_table, file.path(out_dir, "lv_encounters_vs_cetaceans_summary.csv"), na = "")

message(
  "Candidate additions across ", n_distinct(all_compared$source_year), " year(s): ",
  nrow(candidates), " of ", nrow(all_compared), " LV encounters -> ",
  file.path(out_dir, "lv_encounter_candidate_additions_compiled.csv")
)
