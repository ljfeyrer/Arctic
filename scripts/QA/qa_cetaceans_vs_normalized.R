# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Compare legacy/curated Cetaceans workbooks with the canonical yearly
# normalized Tarajoq sightings tables.  This is a review audit only: it does
# not merge, delete, or overwrite any source or normalized data.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tidyr)
})

source_dir <- file.path("input", "orig_data")
normalized_dir <- file.path("input", "normalized")
qa_dir <- file.path("output", "Sightings_QA")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

source("scripts/lib/helpers.R")
first_col <- get_col

parse_datetime <- function(x) {
  x <- na_text(x)
  out <- as.POSIXct(rep(NA_character_, length(x)), tz = "UTC")

  serial <- suppressWarnings(as.numeric(x))
  serial_ok <- !is.na(serial) & serial > 20000 & serial < 60000
  out[serial_ok] <- as.POSIXct(serial[serial_ok] * 86400,
    origin = "1899-12-30", tz = "UTC"
  )

  text <- str_replace_all(x, "T", " ")
  text <- str_replace(text, "Z$", "")
  text_ok <- !serial_ok & !is.na(text)
  if (any(text_ok)) {
    out[text_ok] <- suppressWarnings(parse_date_time(
      text[text_ok],
      orders = c(
        "ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM",
        "dmy", "mdy HMS", "mdy HM", "mdy"
      ),
      tz = "UTC", quiet = TRUE
    ))
  }
  out
}

source_year <- function(path) {
  file <- basename(path)
  if (str_detect(file, "2022")) {
    return(2022L)
  }
  if (str_detect(file, "2023")) {
    return(2023L)
  }
  if (str_detect(file, "2024")) {
    return(2024L)
  }
  if (str_detect(file, "2025")) {
    return(2025L)
  }
  NA_integer_
}

normalize_cetaceans <- function(path) {
  year <- source_year(path)
  sheets <- excel_sheets(path)
  map_dfr(sheets, function(sheet) {
    raw <- read_excel(path, sheet = sheet, col_types = "text", .name_repair = "unique")
    if (nrow(raw) == 0) {
      return(tibble())
    }

    datetime_candidates <- list(
      timestamp = first_col(raw, "timestamp"),
      Enc_start = first_col(raw, "Enc_start"),
      DateT_UTC = first_col(raw, "DateT_UTC"),
      DateT = first_col(raw, "DateT"),
      Local_Time = first_col(raw, "Local_Time")
    )
    datetime <- as.POSIXct(rep(NA_character_, nrow(raw)), tz = "UTC")
    datetime_field <- rep(NA_character_, nrow(raw))
    for (field in names(datetime_candidates)) {
      candidate <- datetime_candidates[[field]]
      parsed <- parse_datetime(candidate)
      # Local_Time is on the form tablet's clock; shift it to UTC for years
      # where that clock was local (form_clock_offset_hours, helpers.R).
      if (field == "Local_Time") parsed <- form_time_to_utc(parsed, year)
      use <- is.na(datetime) & !is.na(parsed)
      datetime[use] <- parsed[use]
      datetime_field[use] <- field
    }

    tibble(
      source_year = year,
      source_file = basename(path),
      source_sheet = sheet,
      source_row = seq_len(nrow(raw)) + 1L,
      source_datetime_field = datetime_field,
      source_datetime = datetime,
      source_date = as.Date(datetime, tz = "UTC"),
      source_latitude = as_num(first_col(raw, c("Latitude", "Lat"))),
      source_longitude = as_num(first_col(raw, c("Longitude", "Long", "Lon"))),
      source_species = first_col(raw, c("Species")),
      source_count = as_num(first_col(raw, c("Best", "Minimum whales", "Min", "Count")))
    )
  }) %>%
    mutate(source_record_id = paste(source_file, source_sheet, source_row, sep = " | "))
}

source_files <- list.files(
  source_dir,
  pattern = "^Cetaceans_.*[.](xlsx|xls)$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)
source_files <- source_files[!is.na(vapply(source_files, source_year, integer(1)))]
if (length(source_files) == 0) stop("No Cetaceans workbooks found in ", source_dir)

source_data <- map_dfr(source_files, normalize_cetaceans)

source_inventory <- tibble(
  source_year = vapply(source_files, source_year, integer(1)),
  source_file = basename(source_files),
  source_path = normalizePath(source_files, winslash = "/", mustWork = FALSE),
  file_size_bytes = file.info(source_files)$size,
  md5 = unname(tools::md5sum(source_files))
) %>%
  group_by(md5) %>%
  mutate(
    duplicate_file_count = n(),
    duplicate_group = if_else(n() > 1, paste(sort(source_file), collapse = " | "), NA_character_)
  ) %>%
  ungroup()
write_csv(source_inventory, file.path(qa_dir, "cetaceans_source_inventory.csv"), na = "")

normalized_files <- file.path(
  normalized_dir,
  unique(na.omit(source_data$source_year)),
  "sightings",
  paste0("tarajoq_form_sightings_", unique(source_data$source_year), "_standardized.csv")
)
normalized_files <- normalized_files[file.exists(normalized_files)]
normalized <- map_dfr(normalized_files, function(path) {
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(
      normalized_file = path,
      source_year = as.integer(source_year),
      normalized_datetime = parse_datetime(standard_datetime),
      normalized_date = as.Date(standard_date),
      normalized_latitude = as_num(latitude),
      normalized_longitude = as_num(longitude),
      normalized_count = as_num(count_best),
      normalized_species = na_text(species)
    )
})

match_one <- function(source_row, normalized_data) {
  if (nrow(normalized_data) == 0) {
    return(tibble(match_class = "no_normalized_table", candidate_count = 0L))
  }

  time_diff_sec <- abs(as.numeric(difftime(
    normalized_data$normalized_datetime, source_row$source_datetime,
    units = "secs"
  )))
  distance_m <- rep(NA_real_, nrow(normalized_data))
  both_coords <- !is.na(source_row$source_latitude) &
    !is.na(source_row$source_longitude) &
    !is.na(normalized_data$normalized_latitude) &
    !is.na(normalized_data$normalized_longitude)
  distance_m[both_coords] <- haversine_m(
    source_row$source_latitude, source_row$source_longitude,
    normalized_data$normalized_latitude[both_coords],
    normalized_data$normalized_longitude[both_coords]
  )

  exact_coord <- !is.na(time_diff_sec) & time_diff_sec <= 1 &
    !is.na(distance_m) & distance_m <= 1
  exact_time_missing_coord <- !is.na(time_diff_sec) & time_diff_sec <= 1 &
    is.na(distance_m)
  same_date_coord <- !is.na(source_row$source_date) &
    normalized_data$normalized_date == source_row$source_date &
    !is.na(distance_m) & distance_m <= 10
  near_time_coord <- !is.na(time_diff_sec) & time_diff_sec <= 60 &
    !is.na(distance_m) & distance_m <= 100

  eligible <- which(exact_coord | exact_time_missing_coord | same_date_coord | near_time_coord)
  if (length(eligible) == 0) {
    return(tibble(
      match_class = "no_match",
      candidate_count = 0L,
      normalized_record_id = NA_character_,
      normalized_datetime = as.POSIXct(NA_character_, tz = "UTC"),
      normalized_date = as.Date(NA_character_),
      normalized_latitude = NA_real_,
      normalized_longitude = NA_real_,
      normalized_species = NA_character_,
      normalized_count = NA_real_,
      time_difference_seconds = NA_real_,
      distance_m = NA_real_
    ))
  }

  rank <- rep(4L, nrow(normalized_data))
  rank[near_time_coord] <- 3L
  rank[same_date_coord] <- 2L
  rank[exact_time_missing_coord] <- 1L
  rank[exact_coord] <- 0L
  best <- eligible[order(
    rank[eligible],
    ifelse(is.na(time_diff_sec[eligible]), Inf, time_diff_sec[eligible]),
    ifelse(is.na(distance_m[eligible]), Inf, distance_m[eligible])
  )[1]]

  tibble(
    match_class = c(
      "exact_datetime_coordinate", "exact_datetime_missing_coordinate",
      "same_date_coordinate", "very_close"
    )[rank[best] + 1L],
    candidate_count = length(eligible),
    normalized_record_id = normalized_data$record_id[best],
    normalized_datetime = normalized_data$normalized_datetime[best],
    normalized_date = normalized_data$normalized_date[best],
    normalized_latitude = normalized_data$normalized_latitude[best],
    normalized_longitude = normalized_data$normalized_longitude[best],
    normalized_species = normalized_data$normalized_species[best],
    normalized_count = normalized_data$normalized_count[best],
    time_difference_seconds = time_diff_sec[best],
    distance_m = distance_m[best]
  )
}

compare_year <- function(year, sources, normalized_data) {
  source_rows <- sources %>% filter(source_year == year)
  norm_rows <- normalized_data %>% filter(source_year == year)
  if (nrow(norm_rows) == 0) {
    norm_rows <- normalized_data %>%
      filter(as.integer(format(normalized_datetime, "%Y", tz = "UTC")) == year)
  }
  matches <- map_dfr(seq_len(nrow(source_rows)), function(i) {
    match_one(source_rows[i, ], norm_rows)
  })
  bind_cols(source_rows, matches)
}

years <- sort(unique(source_data$source_year))
review <- map_dfr(years, compare_year, sources = source_data, normalized_data = normalized)

normalized_matched_ids <- review %>%
  filter(!is.na(normalized_record_id)) %>%
  distinct(source_year, normalized_record_id)

normalized_review <- normalized %>%
  mutate(source_year_num = as.integer(source_year)) %>%
  anti_join(normalized_matched_ids,
    by = c("source_year_num" = "source_year", "record_id" = "normalized_record_id")
  ) %>%
  transmute(
    source_year = source_year_num,
    normalized_record_id = record_id,
    normalized_datetime,
    normalized_date,
    normalized_latitude,
    normalized_longitude,
    normalized_species,
    normalized_count,
    normalized_file
  )

summary <- review %>%
  group_by(source_year, source_file) %>%
  summarise(
    source_rows = n(),
    exact_datetime_coordinate = sum(match_class == "exact_datetime_coordinate"),
    exact_datetime_missing_coordinate = sum(match_class == "exact_datetime_missing_coordinate"),
    same_date_coordinate = sum(match_class == "same_date_coordinate"),
    very_close = sum(match_class == "very_close"),
    no_match = sum(match_class == "no_match"),
    source_rows_with_multiple_candidates = sum(candidate_count > 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    normalized %>% count(source_year, name = "normalized_rows"),
    by = c("source_year" = "source_year")
  ) %>%
  left_join(
    normalized_review %>% count(source_year, name = "normalized_rows_without_source_match"),
    by = c("source_year" = "source_year")
  ) %>%
  arrange(source_year, source_file)

write_csv(summary, file.path(qa_dir, "cetaceans_vs_normalized_summary.csv"), na = "")
write_csv(review, file.path(qa_dir, "cetaceans_vs_normalized_review.csv"), na = "")
write_csv(normalized_review, file.path(qa_dir, "normalized_without_cetaceans_source_match.csv"), na = "")

message("Wrote: ", file.path(qa_dir, "cetaceans_vs_normalized_summary.csv"))
message("Wrote: ", file.path(qa_dir, "cetaceans_vs_normalized_review.csv"))
message("Wrote: ", file.path(qa_dir, "normalized_without_cetaceans_source_match.csv"))
message("Wrote: ", file.path(qa_dir, "cetaceans_source_inventory.csv"))
print(summary, n = Inf)
