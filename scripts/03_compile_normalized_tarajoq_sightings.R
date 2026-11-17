# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Compile and deduplicate normalized Tarajoq NBW sightings.
# Kiviuq and original workbooks are excluded. Normalized Tarajoq station
# sightings are included when present; files removed from normalized are not
# discovered or compiled.
# Original and normalized source files are never modified.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
})

source("scripts/lib/helpers.R")

year_arg <- get_arg("year", "all")
years <- if (year_arg == "all") {
  list.dirs("input/normalized", recursive = FALSE, full.names = FALSE) %>%
    str_subset("^[0-9]{4}$") %>%
    as.integer() %>%
    sort()
} else {
  as.integer(year_arg)
}
if (any(is.na(years))) stop("Use --year=YYYY or --year=all")

parse_datetime <- function(x) {
  x <- na_text(x)
  out <- suppressWarnings(parse_date_time(
    str_replace_all(x, "T", " "),
    orders = c(
      "ymd HMS z", "ymd HMS", "ymd HM", "ymd",
      "dmy HMS", "dmy HM", "dmy", "mdy HMS", "mdy HM", "mdy"
    ),
    tz = "UTC", quiet = TRUE
  ))
  as.POSIXct(out, tz = "UTC")
}

source_type <- function(file) {
  case_when(
    str_detect(file, "^tarajoq_form_sightings_") ~ "form",
    str_detect(file, "^field_note_sightings_") ~ "field_note",
    str_detect(file, "^tarajoq_photo_id_sightings_") ~ "photo_id",
    str_detect(file, "^tarajoq_station_sightings_") ~ "station",
    TRUE ~ "other"
  )
}

read_tarajoq_file <- function(path, year) {
  raw <- read_csv(path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  file <- basename(path)
  species <- get_col(raw, c("species", "species_raw"))
  count <- coalesce(
    as_num(get_col(raw, c("count", "count_best", "count_min"))),
    as_num(get_col(raw, c("count_max", "Minimum whales")))
  )
  datetime <- parse_datetime(get_col(raw, c("standard_datetime", "standard_date")))
  latitude <- as_num(get_col(raw, c("latitude", "source_latitude")))
  longitude <- as_num(get_col(raw, c("longitude", "source_longitude")))
  tibble(
    record_id = coalesce(get_col(raw, "record_id"), paste0(year, "_", source_type(file), "_", seq_len(nrow(raw)))),
    source_year = year,
    source_type = source_type(file),
    standard_datetime = datetime,
    standard_date = as.Date(datetime, tz = "UTC"),
    standard_time = format(datetime, "%H:%M:%S", tz = "UTC"),
    latitude, longitude, count,
    species, survey_leg = get_col(raw, c("survey_leg", "source_leg")),
    coordinate_source = get_col(raw, "coordinate_source"),
    coordinate_use = get_col(raw, "coordinate_use"),
    source_file = file,
    source_sheet = get_col(raw, "source_sheet"),
    source_row = get_col(raw, c("source_row", "source_rows")),
    source_comments = get_col(raw, c("comments", "comments_raw")),
    source_path = normalizePath(path, winslash = "/", mustWork = FALSE)
  ) %>%
    filter(str_detect(
      str_to_lower(coalesce(species, "")),
      "northern bottlenose|northen bottlenose|bottlenose whale|\\bnbw\\b"
    )) %>%
    mutate(species = "Northern Bottlenose Whale")
}

find_candidates <- function(data) {
  if (nrow(data) < 2) {
    return(tibble())
  }
  pairs <- map_dfr(seq_len(nrow(data) - 1), function(i) {
    map_dfr((i + 1):nrow(data), function(j) {
      dt_diff <- abs(as.numeric(difftime(data$standard_datetime[i],
        data$standard_datetime[j],
        units = "secs"
      )))
      distance <- if (all(!is.na(c(
        data$latitude[i], data$longitude[i],
        data$latitude[j], data$longitude[j]
      )))) {
        haversine_m(data$latitude[i], data$longitude[i], data$latitude[j], data$longitude[j])
      } else {
        NA_real_
      }
      same_date <- !is.na(data$standard_date[i]) && !is.na(data$standard_date[j]) &&
        data$standard_date[i] == data$standard_date[j]
      exact <- !is.na(dt_diff) && dt_diff <= 1 && !is.na(distance) && distance <= 10
      date_coord <- same_date && !is.na(distance) && distance <= 10 &&
        (is.na(data$standard_datetime[i]) || is.na(data$standard_datetime[j]))
      very_close <- !is.na(dt_diff) && dt_diff <= 60 && !is.na(distance) && distance <= 100
      if (!(exact || date_coord || very_close)) {
        return(tibble())
      }
      tibble(
        record_id_a = data$record_id[i], record_id_b = data$record_id[j],
        source_type_a = data$source_type[i], source_type_b = data$source_type[j],
        datetime_difference_seconds = dt_diff, distance_m = distance,
        match_class = case_when(
          exact ~ "exact_datetime_coordinate",
          date_coord ~ "same_date_coordinate_missing_time",
          TRUE ~ "very_close"
        ),
        auto_deduplicate = exact || date_coord
      )
    })
  })
  pairs
}

priority <- c(form = 1L, station = 2L, field_note = 3L, photo_id = 4L)
out_root <- "output/Sightings"
qa_root <- "output/Sightings_QA"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_root, recursive = TRUE, showWarnings = FALSE)

for (year in years) {
  sighting_dir <- file.path("input", "normalized", as.character(year), "sightings")
  files <- list.files(sighting_dir, full.names = TRUE, pattern = "[.]csv$")
  files <- files[str_detect(basename(files), paste0(
    "^(tarajoq_form_sightings_|field_note_sightings_|tarajoq_photo_id_sightings_|tarajoq_station_sightings_)", year
  ))]
  # A _cor form file is a corrected derivative of the standardized form file.
  # Prefer it when present so corrections are applied exactly once.
  corrected_forms <- files[str_detect(basename(files), "^tarajoq_form_sightings_.*_cor[.]csv$")]
  if (length(corrected_forms)) {
    standardized_forms <- sub("_cor[.]csv$", "_standardized.csv", corrected_forms)
    files <- files[!files %in% standardized_forms]
  }
  if (!length(files)) {
    message(year, ": no normalized Tarajoq form/field-note/photo-ID files")
    next
  }

  compiled <- map_dfr(files, read_tarajoq_file, year = year) %>%
    arrange(standard_datetime, source_type, source_file, source_row)
  candidates <- find_candidates(compiled)

  # Union exact/date-coordinate pairs into duplicate groups.
  parent <- seq_len(nrow(compiled))
  find_root <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]
      x <- parent[x]
    }
    x
  }
  if (nrow(candidates)) {
    for (k in which(candidates$auto_deduplicate)) {
      a <- match(candidates$record_id_a[k], compiled$record_id)
      b <- match(candidates$record_id_b[k], compiled$record_id)
      ra <- find_root(a)
      rb <- find_root(b)
      if (ra != rb) parent[rb] <- ra
    }
  }
  groups <- vapply(seq_len(nrow(compiled)), find_root, integer(1))
  group_id <- ifelse(duplicated(groups) | duplicated(groups, fromLast = TRUE),
    paste0(year, "_dedup_", groups), NA_character_
  )
  compiled <- compiled %>%
    mutate(
      dedup_group_id = group_id,
      source_priority = unname(priority[source_type]),
      retained = TRUE,
      dedup_status = if_else(is.na(dedup_group_id), "unique", "duplicate_group_representative_pending")
    )
  for (g in unique(na.omit(compiled$dedup_group_id))) {
    ix <- which(compiled$dedup_group_id == g)
    keep <- ix[order(
      compiled$source_priority[ix],
      is.na(compiled$standard_datetime[ix]),
      compiled$source_file[ix]
    )[1]]
    compiled$retained[ix] <- FALSE
    compiled$retained[keep] <- TRUE
    compiled$dedup_status[ix] <- "duplicate_removed"
    compiled$dedup_status[keep] <- "retained_duplicate_representative"
  }

  compiled_file <- file.path(out_root, paste0("tarajoq_nbw_sightings_", year, "_compiled.csv"))
  dedup_file <- file.path(out_root, paste0("tarajoq_nbw_sightings_", year, "_deduplicated.csv"))
  candidate_file <- file.path(qa_root, paste0("tarajoq_nbw_sightings_", year, "_duplicate_candidates.csv"))
  audit_file <- file.path(qa_root, paste0("tarajoq_nbw_sightings_", year, "_deduplication_audit.csv"))
  write_csv(compiled, compiled_file, na = "")
  write_csv(compiled %>% filter(retained), dedup_file, na = "")
  write_csv(candidates, candidate_file, na = "")
  write_csv(compiled %>% count(source_type, retained, dedup_status, name = "records"), audit_file, na = "")
  message(
    year, ": ", nrow(compiled), " normalized Tarajoq NBW records; ",
    sum(compiled$retained), " retained after conservative deduplication"
  )
}
