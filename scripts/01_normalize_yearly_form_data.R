# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Standardize yearly deduplicated form tables.
#
# Inputs:
#   input/orig_data/<year>/forms_exports/<sheet>/*compiled_deduplicated.csv
# Outputs:
#   input/normalized/<year>/sightings/tarajoq_form_sightings_<year>_standardized.csv
#   input/normalized/<year>/environment/environment_<year>_standardized.csv
#   input/normalized/<year>/biopsy/biopsy_<year>_standardized.csv
#
# The original workbooks and deduplicated source CSVs are never modified.
# Missing values remain missing. Coordinate corrections are applied separately
# by scripts/04_correct_tarajoq_form_coordinates.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(here)
})

source(here::here("scripts", "lib", "helpers.R"))

year_arg <- get_arg("year", "all")
years <- if (identical(year_arg, "all")) {
  dirs <- list.dirs(here::here("input", "orig_data"), recursive = FALSE, full.names = FALSE)
  sort(dirs[grepl("^[0-9]{4}$", dirs)])
} else {
  if (!grepl("^[0-9]{4}$", year_arg)) stop("year must be a four-digit year or all")
  year_arg
}

parse_datetime <- function(x) {
  x <- na_text(x)
  out <- as.POSIXct(rep(NA_character_, length(x)), tz = "UTC")

  iso <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  out[iso] <- suppressWarnings(ymd_hms(x[iso], tz = "UTC", quiet = TRUE))

  dmy <- !is.na(x) & grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}", x)
  out[dmy] <- suppressWarnings(dmy_hms(x[dmy], tz = "UTC", quiet = TRUE, truncated = 2))

  # Excel serial dates. Values below one are time-only cells and are not
  # promoted to a calendar date without an independent date source.
  numeric_date <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
  numeric_date <- numeric_date & as_num(x) >= 1000
  out[numeric_date] <- as.POSIXct(
    as_num(x[numeric_date]) * 86400,
    origin = "1899-12-30",
    tz = "UTC"
  )

  # Field-form biopsy times may begin with H/M/E and use DD/MM/YY.
  prefixed <- !is.na(x) & grepl("^[A-Za-z]\\d{1,2}/\\d{1,2}/", x)
  if (any(prefixed)) {
    clean <- sub("^[A-Za-z]", "", x[prefixed])
    out[prefixed] <- suppressWarnings(dmy_hms(clean, tz = "UTC", quiet = TRUE, truncated = 1))
  }
  out
}

parse_dms_component <- function(x) {
  x <- na_text(x)
  m <- str_match(toupper(x), "^(\\d{1,3})D\\s*([0-9.]+)\\s*([NSEW])$")
  deg <- suppressWarnings(as.numeric(m[, 2]))
  mins <- suppressWarnings(as.numeric(m[, 3]))
  value <- deg + mins / 60
  ifelse(m[, 4] %in% c("S", "W"), -value, value)
}

parse_position <- function(x) {
  x <- na_text(x)
  parts <- str_split(x, "\\s+", simplify = TRUE)
  if (ncol(parts) < 2) {
    return(tibble(latitude = NA_real_, longitude = NA_real_))
  }
  tibble(
    latitude = parse_dms_component(parts[, 1]),
    longitude = parse_dms_component(parts[, 2])
  )
}

canonical_species <- function(x) {
  x <- na_text(x)
  is_nbw <- str_detect(
    str_to_lower(coalesce(x, "")),
    "northern bottlenose|northen bottlenose|bottlenose whale|\\bnbw\\b"
  )
  ifelse(is_nbw, "Northern Bottlenose Whale", x)
}

source_columns <- function(data, year, data_type) {
  tibble(
    source = "Tarajoq",
    source_year = as.integer(year),
    data_type = data_type,
    source_workbook = get_col(data, "source_workbook"),
    source_rows = get_col(data, "source_rows")
  )
}

normalize_sightings <- function(data, year) {
  start_raw <- get_col(data, c("DateT", "Date_Time", "Local_Time"))
  end_raw <- get_col(data, c("TimeEnd", "END encounter", "Local_Time_End"))
  pos <- parse_position(get_col(data, c("StartPos", "Pos")))
  end_pos <- parse_position(get_col(data, "EndPos"))
  dt <- parse_datetime(start_raw)

  normalized <- bind_cols(
    tibble(
      record_id = paste0(year, "_sighting_", seq_len(nrow(data))),
      standard_datetime = dt,
      standard_date = as.Date(dt),
      standard_time = format(dt, "%H:%M:%S", tz = "UTC"),
      datetime_raw = start_raw,
      latitude = pos$latitude,
      longitude = pos$longitude,
      latitude_end = end_pos$latitude,
      longitude_end = end_pos$longitude,
      time_end_raw = end_raw,
      species = canonical_species(get_col(data, "Species")),
      count_min = as_num(get_col(data, "Min")),
      count_best = as_num(get_col(data, "Best")),
      count_max = as_num(get_col(data, "Max")),
      distance_m = as_num(get_col(data, "Dist")),
      bearing_deg = as_num(get_col(data, "Bearing")),
      port_starboard = get_col(data, c("PS", "Port_Star")),
      behaviour = get_col(data, "Behaviour"),
      photo_id = get_col(data, "Pic_no"),
      fishing_state = get_col(data, c("fishing", "Fishing_state")),
      comments = get_col(data, "Comments")
    ),
    source_columns(data, year, "sightings")
  )

  normalized %>%
    mutate(
      coordinate_source = if_else(!is.na(latitude) & !is.na(longitude),
        "form source", NA_character_
      ),
      coordinate_fill_method = NA_character_
    ) %>%
    identity()
}

normalize_environment <- function(data, year) {
  start_raw <- get_col(data, c("DateT", "Date_Time"))
  end_raw <- get_col(data, c("TimeEnd", "END encounter"))
  pos <- parse_position(get_col(data, c("StartPos", "Pos")))
  dt <- parse_datetime(start_raw)

  bind_cols(
    tibble(
      record_id = paste0(year, "_environment_", seq_len(nrow(data))),
      standard_datetime = dt,
      standard_date = as.Date(dt),
      standard_time = format(dt, "%H:%M:%S", tz = "UTC"),
      datetime_raw = start_raw,
      latitude = pos$latitude,
      longitude = pos$longitude,
      time_end_raw = end_raw,
      wind_direction = get_col(data, "Wind_dir"),
      wind_speed_ms = as_num(get_col(data, c("Windspeed_ms", "Windspeed_kt"))),
      wave_height_m = as_num(get_col(data, "Wave_ht_m")),
      swell_height_m = as_num(get_col(data, "Swell_ht_m")),
      air_temperature_c = as_num(get_col(data, "Air_temp_C")),
      water_temperature_c = as_num(get_col(data, c("water_temp", "Sea_temp_C"))),
      visibility = as_num(get_col(data, "Vis")),
      cloud_cover_pct = as_num(get_col(data, "Cloud_cover_pct")),
      vessel_speed_ms = as_num(get_col(data, "Vessel_SOG")),
      station_name = get_col(data, c("stn Name", "stn.Name")),
      fishing_state = get_col(data, c("fishing", "Fishing_state")),
      comments = get_col(data, "Comments")
    ),
    source_columns(data, year, "environment")
  )
}

normalize_biopsy <- function(data, year) {
  start_raw <- get_col(data, c("Biopsy_start", "Biopsy_Time", "DateT"))
  pos <- parse_position(get_col(data, c("StartPos", "Pos")))
  dt <- parse_datetime(start_raw)

  # Older biopsy exports (2024 and earlier) record the subsample id under
  # Sample_no and the outcome as free text under Sample_success rather than
  # the newer Subsample_B_no/Biopsy_outcome columns. Both id schemes are kept
  # so no subsample id is silently dropped; the raw outcome text is retained
  # alongside a canonicalized Hit/Miss value using the same hit/tissue
  # keyword rule the pre-2025 field script used.
  outcome_raw <- get_col(data, c("Biopsy_outcome", "Sample_success"))
  outcome_is_native <- "Biopsy_outcome" %in% names(data)
  outcome_canonical <- if (outcome_is_native) {
    outcome_raw
  } else {
    ifelse(
      is.na(outcome_raw), NA_character_,
      ifelse(str_detect(str_to_lower(outcome_raw), "hit|tissue"), "Hit", "Miss")
    )
  }

  normalized <- bind_cols(
    tibble(
      record_id = paste0(year, "_biopsy_", seq_len(nrow(data))),
      standard_datetime = dt,
      standard_date = as.Date(dt),
      standard_time = format(dt, "%H:%M:%S", tz = "UTC"),
      datetime_raw = start_raw,
      latitude = as_num(get_col(data, "Latitude")),
      longitude = as_num(get_col(data, "Longitude")),
      latitude_from_position = pos$latitude,
      longitude_from_position = pos$longitude,
      sighting_no = get_col(data, "Sighting_no"),
      outcome = outcome_canonical,
      outcome_raw = outcome_raw,
      reaction = get_col(data, "Reaction"),
      species = get_col(data, "Species"),
      side = get_col(data, "Side"),
      sample_a_id = get_col(data, "Subsample_A_no"),
      sample_b_id = coalesce(get_col(data, "Subsample_B_no"), get_col(data, "Sample_no")),
      photo_id = get_col(data, "Pic_no"),
      reliability_notes = get_col(data, "Reliable"),
      comments = get_col(data, "Comments")
    ),
    source_columns(data, year, "biopsy")
  ) %>%
    mutate(
      latitude = coalesce(latitude, latitude_from_position),
      longitude = coalesce(longitude, longitude_from_position)
    ) %>%
    select(-latitude_from_position, -longitude_from_position)

  normalized %>%
    group_by(sample_b_id) %>%
    mutate(duplicate_sample_b_id_count = if_else(
      is.na(sample_b_id), 1L, dplyr::n()
    )) %>%
    ungroup()
}

read_compiled <- function(year, sheet) {
  path <- file.path(
    here::here("input", "orig_data", year, "forms_exports", sheet),
    paste0(tolower(sheet), "_", year, "_compiled_deduplicated.csv")
  )
  if (!file.exists(path)) {
    return(NULL)
  }
  read_csv(path, col_types = cols(.default = col_character()), name_repair = "unique")
}

for (year in years) {
  output_root <- here::here("input", "normalized", year)
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  audit <- list()

  for (spec in list(
    list(
      sheet = "Cetaceans", type = "sightings", folder = "sightings",
      fn = normalize_sightings
    ),
    list(sheet = "Environment", type = "environment", folder = "environment", fn = normalize_environment),
    list(sheet = "Biopsy", type = "biopsy", folder = "biopsy", fn = normalize_biopsy)
  )) {
    raw <- read_compiled(year, spec$sheet)
    if (is.null(raw)) next
    normalized <- spec$fn(raw, year)
    out_dir <- file.path(output_root, spec$folder)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_name <- if (spec$type == "sightings") {
      paste0("tarajoq_form_sightings_", year, "_standardized.csv")
    } else {
      paste0(spec$type, "_", year, "_standardized.csv")
    }
    out_file <- file.path(out_dir, out_name)
    write_csv(normalized, out_file, na = "")

    duplicate_sample_ids <- 0L
    if (spec$type == "biopsy" && nrow(normalized) > 0) {
      dup_rows <- normalized %>% filter(!is.na(sample_b_id), duplicate_sample_b_id_count > 1)
      duplicate_sample_ids <- n_distinct(dup_rows$sample_b_id)
      if (nrow(dup_rows) > 0) {
        dup_file <- here::here(
          "output", "Sightings_QA",
          paste0("biopsy_duplicate_sample_ids_", year, ".csv")
        )
        dir.create(dirname(dup_file), recursive = TRUE, showWarnings = FALSE)
        write_csv(
          dup_rows %>% arrange(sample_b_id, record_id),
          dup_file,
          na = ""
        )
        message(
          year, " biopsy: ", duplicate_sample_ids,
          " Subsample_B_no/Sample_no value(s) shared by more than one record; see ", dup_file
        )
      }
    }

    audit[[length(audit) + 1L]] <- tibble(
      year = as.integer(year),
      data_type = spec$type,
      input_rows = nrow(raw),
      output_rows = nrow(normalized),
      missing_datetime = sum(is.na(normalized$standard_datetime)),
      missing_coordinates = sum(is.na(normalized$latitude) | is.na(normalized$longitude)),
      duplicate_sample_ids = duplicate_sample_ids,
      output_file = normalizePath(out_file, winslash = "/", mustWork = FALSE)
    )
    message(year, " ", spec$type, ": ", nrow(raw), " rows -> ", out_file)
  }

  audit_file <- here::here("output", "Sightings_QA", paste0("normalization_audit_", year, ".csv"))
  dir.create(dirname(audit_file), recursive = TRUE, showWarnings = FALSE)
  if (length(audit) > 0) write_csv(bind_rows(audit), audit_file, na = "")
}
