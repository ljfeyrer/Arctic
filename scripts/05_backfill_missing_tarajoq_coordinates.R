# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Backfill missing coordinates in normalized Tarajoq form sightings,
# environment, and biopsy tables from the normalized ship track.
#
# This is the "later, traceable enrichment step" described in
# input/normalized/README.md: timestamped records with no recorded position
# get the nearest ship-track fix, within a maximum time gap, rather than
# being left blank. It ports the ship-track backfill that
# datasheet_importer_2025.R applied inline (via a floor-to-the-minute join)
# into a reusable, audited pipeline step.
#
# Inputs:
#   input/normalized/<year>/track/track_<year>_standardized.csv
#   input/normalized/<year>/sightings/tarajoq_form_sightings_<year>_cor.csv
#     (or _standardized.csv when no _cor file exists)
#   input/normalized/<year>/environment/environment_<year>_standardized.csv
#   input/normalized/<year>/biopsy/biopsy_<year>_standardized.csv
#
# Outputs (never overwrite the files above):
#   input/normalized/<year>/<table>/<table>_<year>_filled.csv
#   output/Sightings_QA/coordinate_backfill_audit_<year>.csv
#
# This step is review-only: it is not wired into
# 03_compile_normalized_tarajoq_sightings.R. Check the audit (gap sizes,
# how many rows were filled vs. left missing) before deciding whether the
# compiler should prefer the _filled files.
#
# Example:
#   Rscript scripts/05_backfill_missing_tarajoq_coordinates.R --year=2024
#   Rscript scripts/05_backfill_missing_tarajoq_coordinates.R --year=all --max-gap-minutes=15

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
})

source(here::here("scripts", "lib", "helpers.R"))

year_arg <- get_arg("year", "all")
max_gap_minutes <- as.numeric(get_arg("max-gap-minutes", "30"))
if (is.na(max_gap_minutes)) stop("max-gap-minutes must be numeric")

years <- if (identical(year_arg, "all")) {
  dirs <- list.dirs(here::here("input", "normalized"), recursive = FALSE, full.names = FALSE)
  sort(dirs[grepl("^[0-9]{4}$", dirs)])
} else {
  if (!grepl("^[0-9]{4}$", year_arg)) stop("year must be a four-digit year or all")
  year_arg
}

# Nearest ship-track fix in time for each missing-coordinate row. Returns the
# fix's lat/lon and the gap in minutes, or NA when no track is available.
nearest_track_fix <- function(target_datetime, track) {
  if (nrow(track) == 0 || length(target_datetime) == 0) {
    return(tibble(
      fill_latitude = rep(NA_real_, length(target_datetime)),
      fill_longitude = rep(NA_real_, length(target_datetime)),
      fill_gap_minutes = rep(NA_real_, length(target_datetime))
    ))
  }
  idx <- vapply(target_datetime, function(t) {
    if (is.na(t)) {
      return(NA_integer_)
    }
    which.min(abs(as.numeric(difftime(track$standard_datetime, t, units = "mins"))))
  }, integer(1))
  gap <- vapply(seq_along(target_datetime), function(i) {
    if (is.na(idx[i])) {
      return(NA_real_)
    }
    abs(as.numeric(difftime(track$standard_datetime[idx[i]], target_datetime[i], units = "mins")))
  }, numeric(1))
  tibble(
    fill_latitude = ifelse(is.na(idx), NA_real_, track$latitude[idx]),
    fill_longitude = ifelse(is.na(idx), NA_real_, track$longitude[idx]),
    fill_gap_minutes = gap
  )
}

backfill_table <- function(data, track, max_gap_minutes) {
  if (!"coordinate_source" %in% names(data)) data$coordinate_source <- NA_character_
  if (!"coordinate_fill_method" %in% names(data)) data$coordinate_fill_method <- NA_character_
  data$coordinate_fill_gap_minutes <- NA_real_
  data$coordinate_fill_status <- NA_character_

  needs_fill <- is.na(data$latitude) | is.na(data$longitude)
  data$coordinate_fill_status[!needs_fill] <- "coordinate_present"

  if (!any(needs_fill)) {
    return(data)
  }

  if (nrow(track) == 0) {
    data$coordinate_fill_status[needs_fill] <- "no_track_available"
    return(data)
  }

  fix <- nearest_track_fix(data$standard_datetime[needs_fill], track)
  within_gap <- !is.na(fix$fill_gap_minutes) & fix$fill_gap_minutes <= max_gap_minutes

  fill_idx <- which(needs_fill)
  data$latitude[fill_idx[within_gap]] <- fix$fill_latitude[within_gap]
  data$longitude[fill_idx[within_gap]] <- fix$fill_longitude[within_gap]
  data$coordinate_source[fill_idx[within_gap]] <- "ship track (nearest fix)"
  data$coordinate_fill_method[fill_idx[within_gap]] <- paste0(
    "Nearest ship-track fix within ", round(fix$fill_gap_minutes[within_gap], 1), " min"
  )
  data$coordinate_fill_gap_minutes[fill_idx[within_gap]] <- round(fix$fill_gap_minutes[within_gap], 1)
  data$coordinate_fill_status[fill_idx[within_gap]] <- "filled_from_track"

  no_time <- is.na(data$standard_datetime[fill_idx])
  data$coordinate_fill_status[fill_idx[no_time]] <- "no_datetime_to_match"
  too_far <- !within_gap & !no_time
  data$coordinate_fill_status[fill_idx[too_far]] <- paste0(
    "nearest_track_fix_exceeds_", max_gap_minutes, "_min_gap"
  )

  data
}

qa_dir <- here::here("output", "Sightings_QA")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

for (year in years) {
  track_file <- here::here("input", "normalized", year, "track", paste0("track_", year, "_standardized.csv"))
  track <- if (file.exists(track_file)) {
    read_csv(track_file, show_col_types = FALSE) %>%
      select(standard_datetime, latitude, longitude) %>%
      filter(!is.na(standard_datetime), !is.na(latitude), !is.na(longitude)) %>%
      arrange(standard_datetime)
  } else {
    tibble(standard_datetime = as.POSIXct(character()), latitude = numeric(), longitude = numeric())
  }
  if (nrow(track) == 0) {
    message(year, ": no normalized ship track available; coordinates cannot be backfilled")
  }

  sightings_cor <- here::here(
    "input", "normalized", year, "sightings",
    paste0("tarajoq_form_sightings_", year, "_cor.csv")
  )
  sightings_std <- here::here(
    "input", "normalized", year, "sightings",
    paste0("tarajoq_form_sightings_", year, "_standardized.csv")
  )
  sightings_in <- if (file.exists(sightings_cor)) sightings_cor else sightings_std

  specs <- list(
    list(
      type = "sightings", folder = "sightings", in_file = sightings_in,
      out_file = here::here(
        "input", "normalized", year, "sightings",
        paste0("tarajoq_form_sightings_", year, "_filled.csv")
      )
    ),
    list(
      type = "environment", folder = "environment",
      in_file = here::here(
        "input", "normalized", year, "environment",
        paste0("environment_", year, "_standardized.csv")
      ),
      out_file = here::here(
        "input", "normalized", year, "environment",
        paste0("environment_", year, "_filled.csv")
      )
    ),
    list(
      type = "biopsy", folder = "biopsy",
      in_file = here::here(
        "input", "normalized", year, "biopsy",
        paste0("biopsy_", year, "_standardized.csv")
      ),
      out_file = here::here(
        "input", "normalized", year, "biopsy",
        paste0("biopsy_", year, "_filled.csv")
      )
    )
  )

  audit_rows <- list()
  for (spec in specs) {
    if (!file.exists(spec$in_file)) next
    data <- read_csv(spec$in_file, show_col_types = FALSE)
    if (!all(c("standard_datetime", "latitude", "longitude") %in% names(data))) next

    missing_before <- sum(is.na(data$latitude) | is.na(data$longitude))
    filled <- backfill_table(data, track, max_gap_minutes)
    dir.create(dirname(spec$out_file), recursive = TRUE, showWarnings = FALSE)
    write_csv(filled, spec$out_file, na = "")

    filled_count <- sum(filled$coordinate_fill_status == "filled_from_track", na.rm = TRUE)
    audit_rows[[length(audit_rows) + 1L]] <- tibble(
      year = as.integer(year),
      data_type = spec$type,
      input_rows = nrow(data),
      missing_coordinates_before = missing_before,
      filled_from_track = filled_count,
      still_missing_after = missing_before - filled_count,
      median_fill_gap_minutes = if (filled_count > 0) {
        median(filled$coordinate_fill_gap_minutes, na.rm = TRUE)
      } else {
        NA_real_
      },
      max_fill_gap_minutes = if (filled_count > 0) {
        max(filled$coordinate_fill_gap_minutes, na.rm = TRUE)
      } else {
        NA_real_
      },
      max_gap_minutes_allowed = max_gap_minutes,
      output_file = normalizePath(spec$out_file, winslash = "/", mustWork = FALSE)
    )
    message(
      year, " ", spec$type, ": ", missing_before, " missing coordinate(s), ",
      filled_count, " filled from ship track (<= ", max_gap_minutes, " min)"
    )
  }

  if (length(audit_rows) > 0) {
    audit_file <- file.path(qa_dir, paste0("coordinate_backfill_audit_", year, ".csv"))
    write_csv(bind_rows(audit_rows), audit_file, na = "")
  }
}
