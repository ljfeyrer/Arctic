# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Apply documented coordinate corrections to normalized Tarajoq form tables.
# The standardized form table is never overwritten. Corrected output uses the
# same columns and is named tarajoq_form_sightings_<year>_cor.csv.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(here)
})

source(here::here("scripts", "lib", "helpers.R"))

year_arg <- get_arg("year", "all")
years <- if (year_arg == "all") {
  dirs <- list.dirs(here::here("input", "normalized"), recursive = FALSE, full.names = FALSE)
  sort(dirs[grepl("^[0-9]{4}$", dirs)])
} else {
  year_arg
}

parse_unsigned_dmm <- function(x) {
  x <- na_text(x)
  m <- str_match(toupper(x), "^(\\d{1,3})D?([0-9.]+)$")
  suppressWarnings(as.numeric(m[, 2]) + as.numeric(m[, 3]) / 60)
}

parse_arctic_position <- function(x) {
  parts <- str_split(na_text(x), "\\s+", simplify = TRUE)
  tibble(
    latitude = parse_unsigned_dmm(parts[, 1]),
    longitude = -parse_unsigned_dmm(parts[, 2])
  )
}

for (year in years) {
  form_file <- here::here(
    "input", "normalized", year, "sightings",
    paste0("tarajoq_form_sightings_", year, "_standardized.csv")
  )
  if (!file.exists(form_file)) next

  form <- read_csv(form_file, col_types = cols(.default = col_character()), name_repair = "unique")
  coordinate_note_pattern <- regex(
    "coord|position|station|environment|gps|lat|lon|location|fix|same place|same position|next coord",
    ignore_case = TRUE
  )
  note_flag <- !is.na(form$comments) & str_detect(form$comments, coordinate_note_pattern)
  form$coordinate_correction_note_flag <- note_flag
  form$coordinate_correction_status <- ifelse(note_flag, "review_needed", "no_note_flag")
  form$coordinate_correction_documentation <- NA_character_

  if (year == "2022") {
    target <- form$record_id == "2022_sighting_21" &
      (is.na(form$latitude) | is.na(form$longitude))
    next_i <- which(target) + 1L
    valid_next <- next_i <= nrow(form) &
      !is.na(form$latitude[next_i]) & !is.na(form$longitude[next_i])
    if (any(target) && any(valid_next)) {
      i <- which(target)[which(valid_next)[1]]
      j <- i + 1L
      form$latitude[i] <- form$latitude[j]
      form$longitude[i] <- form$longitude[j]
      form$coordinate_source[i] <- "next form record"
      form$coordinate_fill_method[i] <-
        "Approximate carry-forward from immediately following trawling record"
      form$coordinate_correction_status[i] <- "corrected"
      form$coordinate_correction_documentation[i] <-
        "2022 sighting 21: use immediately following trawling record position; documented source review decision"
    }
  }

  if (year == "2025") {
    env_file <- here::here(
      "input", "orig_data", year, "forms_exports", "Environment",
      paste0("environment_", year, "_compiled_deduplicated.csv")
    )
    if (file.exists(env_file)) {
      env <- read_csv(env_file, col_types = cols(.default = col_character()), name_repair = "unique")
      target <- !is.na(form$comments) &
        str_detect(str_to_lower(form$comments), "same position as the first environment") &
        (is.na(form$latitude) | is.na(form$longitude))

      if (any(target) && nrow(env) > 0) {
        env_source <- na_text(env$source_workbook)
        for (i in which(target)) {
          source_token <- str_trim(str_extract(form$source_workbook[i], "^[^;]+"))
          same_source <- !is.na(env_source) & str_detect(env_source, fixed(source_token))
          env_row <- if (any(same_source)) which(same_source)[1] else NA_integer_
          if (!is.na(env_row)) {
            pos <- parse_arctic_position(env$StartPos[env_row])
            if (!is.na(pos$latitude) && !is.na(pos$longitude)) {
              form$latitude[i] <- as.character(pos$latitude)
              form$longitude[i] <- as.character(pos$longitude)
              form$coordinate_source[i] <- "first environment record"
              form$coordinate_fill_method[i] <-
                "Explicit form note: same position as the first environment record"
              form$coordinate_correction_status[i] <- "corrected"
              form$coordinate_correction_documentation[i] <-
                "2025 sighting 7: explicit note says same position as the first environment record from the same source workbook"
            }
          }
        }
      }
    }
  }

  out_file <- here::here(
    "input", "normalized", year, "sightings",
    paste0("tarajoq_form_sightings_", year, "_cor.csv")
  )
  write_csv(form, out_file, na = "")

  corrections <- form %>%
    filter(coordinate_correction_note_flag | coordinate_correction_status == "corrected") %>%
    transmute(
      year,
      record_id,
      standard_datetime,
      latitude,
      longitude,
      comments,
      coordinate_source,
      coordinate_fill_method,
      correction_status = coordinate_correction_status,
      correction_documentation = coordinate_correction_documentation
    )
  qa_file <- here::here(
    "output", "Sightings_QA",
    paste0("tarajoq_form_coordinate_corrections_", year, ".csv")
  )
  dir.create(dirname(qa_file), recursive = TRUE, showWarnings = FALSE)
  write_csv(corrections, qa_file, na = "")
  message(year, ": wrote corrected form table and ", nrow(corrections), " flagged form note(s)")
}
