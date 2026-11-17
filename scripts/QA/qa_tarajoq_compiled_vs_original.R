# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Compare a compiled/deduplicated Tarajoq year against its original Cetaceans
# workbook. This is an audit only; neither source nor compiled files change.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(lubridate)
  library(stringr)
  library(purrr)
})

args <- commandArgs(trailingOnly = TRUE)
year_arg <- if (length(args)) sub("^--year=", "", args[[1]]) else "2023"
year <- as.integer(year_arg)
if (is.na(year)) stop("Use --year=YYYY")

source("scripts/lib/helpers.R")

parse_datetime <- function(x) {
  x <- na_text(x)
  serial <- suppressWarnings(as.numeric(x))
  out <- as.POSIXct(rep(NA_character_, length(x)), tz = "UTC")
  serial_ok <- !is.na(serial) & serial > 20000 & serial < 60000
  out[serial_ok] <- as.POSIXct(serial[serial_ok] * 86400,
    origin = "1899-12-30", tz = "UTC"
  )
  text_ok <- !serial_ok & !is.na(x)
  out[text_ok] <- suppressWarnings(parse_date_time(
    str_replace_all(x[text_ok], "T", " "),
    orders = c("ymd HMS z", "ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy"),
    tz = "UTC", quiet = TRUE
  ))
  out
}

source_candidates <- file.path(
  "input", "orig_data", as.character(year), "sightings_original",
  c(
    paste0("Cetaceans_Arctic", year, ".xlsx"),
    paste0("Cetaceans_ArcticLeg1_", year, ".xlsx"),
    paste0("Cetaceans_ArcticLeg1", year, ".xlsx"),
    paste0("Cetaceans_ArcticLeg1_", year, ".xls"),
    paste0("Cetaceans_ArcticLeg1", year, ".xls")
  )
)
source_file <- source_candidates[file.exists(source_candidates)][1]
compiled_file <- file.path(
  "output", "Sightings",
  paste0("tarajoq_nbw_sightings_", year, "_deduplicated.csv")
)
if (!file.exists(source_file) || !file.exists(compiled_file)) stop("Missing source or compiled file")

raw <- read_excel(source_file, sheet = 1, col_types = "text")
datetime_fields <- c("timestamp", "Enc_start", "DateT_UTC", "DateT", "Local_Time", "DateT_local")
datetime_fields <- datetime_fields[datetime_fields %in% names(raw)]
field_counts <- vapply(datetime_fields, function(f) sum(!is.na(na_text(raw[[f]]))), integer(1))
datetime_field <- if (length(field_counts) && max(field_counts) > 0) {
  datetime_fields[which.max(field_counts)]
} else {
  NA_character_
}
datetime_raw <- if (!is.na(datetime_field)) {
  na_text(raw[[datetime_field]])
} else {
  rep(NA_character_, nrow(raw))
}
species_field <- if ("Species" %in% names(raw)) {
  na_text(raw$Species)
} else {
  rep("Northern Bottlenose Whale", nrow(raw))
}
source <- raw %>%
  mutate(
    source_row = row_number() + 1L,
    source_datetime_field = datetime_field,
    source_datetime_raw = datetime_raw,
    source_datetime = parse_datetime(source_datetime_raw),
    source_date = as.Date(source_datetime),
    source_latitude = suppressWarnings(as.numeric(Latitude)),
    source_longitude = suppressWarnings(as.numeric(Longitude)),
    source_species = species_field
  ) %>%
  filter(!is.na(source_datetime))

compiled <- read_csv(compiled_file, show_col_types = FALSE) %>%
  mutate(
    compiled_datetime = ymd_hms(standard_datetime, tz = "UTC"),
    compiled_date = as.Date(standard_date),
    compiled_latitude = suppressWarnings(as.numeric(latitude)),
    compiled_longitude = suppressWarnings(as.numeric(longitude)),
    compiled_source_row = suppressWarnings(as.integer(source_row))
  )

review <- map_dfr(seq_len(nrow(source)), function(i) {
  s <- source[i, ]
  dtime <- abs(as.numeric(difftime(compiled$compiled_datetime, s$source_datetime, units = "secs")))
  dist <- rep(NA_real_, nrow(compiled))
  ok <- !is.na(s$source_latitude) & !is.na(s$source_longitude) &
    !is.na(compiled$compiled_latitude) & !is.na(compiled$compiled_longitude)
  dist[ok] <- haversine_m(
    s$source_latitude, s$source_longitude,
    compiled$compiled_latitude[ok], compiled$compiled_longitude[ok]
  )
  same_date_coord <- compiled$compiled_date == s$source_date & !is.na(dist) & dist <= 10
  exact <- !is.na(dtime) & dtime <= 1 & !is.na(dist) & dist <= 10
  exact_time_missing_coord <- !is.na(dtime) & dtime <= 1 & is.na(dist)
  near <- !is.na(dtime) & dtime <= 60 & !is.na(dist) & dist <= 100
  provenance <- year == 2023 & compiled$source_type == "photo_id" &
    compiled$compiled_source_row == s$source_row
  eligible <- which(exact | exact_time_missing_coord | near | same_date_coord | provenance)
  if (!length(eligible)) {
    return(tibble(
      source_row = s$source_row, source_datetime_field = s$source_datetime_field,
      source_datetime = s$source_datetime,
      source_latitude = s$source_latitude, source_longitude = s$source_longitude,
      source_species = s$source_species, match_status = "no_compiled_match",
      compiled_record_id = NA_character_, compiled_source_type = NA_character_,
      compiled_datetime = as.POSIXct(NA), compiled_date = as.Date(NA),
      time_difference_seconds = NA_real_, distance_m = NA_real_
    ))
  }
  rank <- rep(4L, nrow(compiled))
  rank[near] <- 3L
  rank[same_date_coord] <- 2L
  rank[exact_time_missing_coord] <- 1L
  rank[provenance] <- 0L
  rank[exact] <- 0L
  best <- eligible[order(
    rank[eligible],
    ifelse(is.na(dtime[eligible]), Inf, dtime[eligible]),
    ifelse(is.na(dist[eligible]), Inf, dist[eligible])
  )[1]]
  tibble(
    source_row = s$source_row, source_datetime_field = s$source_datetime_field,
    source_datetime = s$source_datetime,
    source_latitude = s$source_latitude, source_longitude = s$source_longitude,
    source_species = s$source_species,
    match_status = case_when(
      provenance[best] ~ "photo_source_row_link",
      exact[best] ~ "exact_datetime_coordinate",
      exact_time_missing_coord[best] ~ "exact_datetime_missing_coordinate",
      same_date_coord[best] ~ "same_date_coordinate",
      near[best] ~ "very_close",
      TRUE ~ "candidate"
    ),
    compiled_record_id = compiled$record_id[best],
    compiled_source_type = compiled$source_type[best],
    compiled_datetime = compiled$compiled_datetime[best],
    compiled_date = compiled$compiled_date[best],
    time_difference_seconds = dtime[best], distance_m = dist[best]
  )
})

qa_dir <- "output/Sightings_QA"
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(review, file.path(qa_dir, paste0("tarajoq_compiled_vs_original_", year, ".csv")), na = "")
write_csv(review %>% count(match_status, name = "records"),
  file.path(qa_dir, paste0("tarajoq_compiled_vs_original_", year, "_summary.csv")),
  na = ""
)
matched_compiled_ids <- unique(na.omit(review$compiled_record_id))
compiled_extras <- compiled %>%
  filter(!record_id %in% matched_compiled_ids) %>%
  mutate(compare_status = "compiled_additional_source")
write_csv(compiled_extras,
  file.path(qa_dir, paste0("tarajoq_compiled_vs_original_", year, "_compiled_extras.csv")),
  na = ""
)
write_csv(
  tibble(
    source_workbook_records = nrow(review),
    source_records_matched = sum(review$match_status != "no_compiled_match"),
    compiled_deduplicated_records = nrow(compiled),
    compiled_records_matched_to_source = length(matched_compiled_ids),
    compiled_additional_records = nrow(compiled_extras)
  ), file.path(qa_dir, paste0("tarajoq_compiled_vs_original_", year, "_bidirectional_summary.csv")),
  na = ""
)
print(review %>% count(match_status, name = "records"))
