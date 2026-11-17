# Author: Laura Joan Feyrer
# Date created: 2026-11-17
# Date updated: 2026-11-17
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Extract note text that may require coordinate review before compilation.
# This is an audit only: it never changes normalized source tables.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(here)
})

files <- list.files(
  here::here("input", "normalized"),
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
files <- files[
  grepl("/sightings/", files) &
    grepl("tarajoq_form_sightings_.*_standardized\\.csv$", files) &
    !grepl("naw", files, ignore.case = TRUE)
]

note_columns <- c(
  "comments", "notes", "source_comments", "coordinate_notes",
  "review_notes", "reconciliation_notes", "photo_status"
)
coordinate_terms <- regex(
  "coord|position|station|environment|gps|lat|lon|location|fix|same place|same position|next coord",
  ignore_case = TRUE
)

extract_file <- function(path) {
  data <- read_csv(path, col_types = cols(.default = col_character()), name_repair = "unique")
  available <- intersect(note_columns, names(data))
  if (!length(available)) {
    return(tibble())
  }

  note_table <- map_dfr(available, function(col) {
    value <- str_squish(data[[col]])
    keep <- !is.na(value) & value != "" & str_detect(value, coordinate_terms)
    if (!any(keep)) {
      return(tibble())
    }
    tibble(
      source_file = normalizePath(path, winslash = "/", mustWork = FALSE),
      source_table = basename(path),
      note_column = col,
      source_row = which(keep),
      record_id = if ("record_id" %in% names(data)) data$record_id[keep] else NA_character_,
      standard_datetime = if ("standard_datetime" %in% names(data)) data$standard_datetime[keep] else NA_character_,
      standard_date = if ("standard_date" %in% names(data)) data$standard_date[keep] else NA_character_,
      latitude = if ("latitude" %in% names(data)) data$latitude[keep] else NA_character_,
      longitude = if ("longitude" %in% names(data)) data$longitude[keep] else NA_character_,
      note_text = value[keep],
      coordinate_source = if ("coordinate_source" %in% names(data)) data$coordinate_source[keep] else NA_character_,
      coordinate_fill_method = if ("coordinate_fill_method" %in% names(data)) data$coordinate_fill_method[keep] else NA_character_
    )
  })
  note_table
}

review <- map_dfr(files, extract_file)
review <- review %>%
  mutate(review_status = case_when(
    is.na(latitude) | is.na(longitude) ~ "note_mentions_coordinates_missing",
    !is.na(coordinate_fill_method) ~ "coordinate_already_filled_documented",
    TRUE ~ "coordinate_present_review_note"
  ))
out_dir <- here::here("output", "Sightings_QA")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "coordinate_note_review.csv")
write_csv(review, out_file, na = "")

summary <- review %>% count(source_table, note_column, review_status, name = "records")
write_csv(summary, file.path(out_dir, "coordinate_note_review_summary.csv"), na = "")

message("Coordinate-relevant notes flagged: ", nrow(review))
message("Review table: ", out_file)
