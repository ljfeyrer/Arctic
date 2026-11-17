#!/usr/bin/env Rscript

# Author: Laura Joan Feyrer
# Date created: 2025-11-14
# Date updated: 2026-11-16
# Description: Cross-reference the merged, within-vessel-deduplicated annual
# NBW sightings (Tarajoq + Kiviuq, from 06_merge_annual_sightings.R) against
# external reference databases -- the DFO national sightings database, the
# Greenland NBW compilation, and the Paamiut all-NBW-observations table -- to
# find sightings already present in those databases. Flags likely duplicates
# with a tiered time/distance match and produces a combined,
# cross-database-deduplicated sightings table.
#
# This step does not depend on which field-data years have been compiled.
# Run with --year=all (default) to pick up whatever
# output/Sightings/nbw_annual_sightings_<year>_{compiled_deduplicated,deduplicated}.csv
# files exist at the time (see resolve_dedup_file() in scripts/lib/helpers.R),
# or --year=YYYY for one year. Re-run any time a new year is merged or a
# reference database is updated. Must run after 06_merge_annual_sightings.R
# (which must itself run after 03_compile_normalized_tarajoq_sightings.R and
# 02_normalize_kiviuq_sightings.R).
#
# Inputs (never modified):
#   output/Sightings/nbw_annual_sightings_<year>_{compiled_deduplicated,deduplicated}.csv
#   input/reference/database_collated/DFO_NBW_Oct2026.xlsx
#   input/reference/database_collated/Greenland_NBW_Oct2026.xlsx
#   input/reference/database_collated/allnbwobs_paamiut_260522.csv
#
# Outputs:
#   output/Sightings_QA/nbw_compiled_reference_validation.csv
#   output/Sightings_QA/nbw_compiled_reference_candidate_pairs.csv
#   output/Sightings_QA/nbw_compiled_reference_best_matches.csv
#   output/Sightings_QA/nbw_compiled_reference_exact_matches.csv
#   output/Sightings_QA/nbw_compiled_reference_match_summary.csv
#   output/Sightings_QA/nbw_compiled_reference_deduplication_audit.csv
#   output/Sightings_QA/nbw_compiled_reference_crossref_summary.txt
#   output/Sightings/nbw_sightings_compiled_vs_reference_deduplicated.csv
#   output/Sightings/nbw_sightings_compiled_vs_reference_map.png
#
# To add another reference database later: write a reader function that
# returns the common ref_* schema used by read_dfo_reference() /
# read_greenland_reference() / read_paamiut_reference() below, then add one
# entry to `reference_sources`.

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
})

source("scripts/lib/helpers.R")

year_arg <- get_arg("year", "all")

compiled_root <- "output/Sightings"
reference_root <- "input/reference/database_collated"
out_dir <- "output/Sightings"
qa_dir <- "output/Sightings_QA"
eez_path <- "input/basemaps/eez_greenland.json"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

nbw_species_regex <- "northern bottlenose|northen bottlenose|bottlenose whale|\\bnbw\\b"

# ---- DISCOVER COMPILED FIELD-DATA YEARS ------------------------------------

compiled_files <- list.files(compiled_root,
  pattern = "^nbw_annual_sightings_[0-9]{4}_(compiled_deduplicated|deduplicated)\\.csv$"
)
available_years <- as.integer(str_extract(compiled_files, "[0-9]{4}"))
years <- if (year_arg == "all") sort(available_years) else as.integer(year_arg)
years <- years[years %in% available_years]
if (!length(years)) {
  stop(
    "No merged annual years found under ", compiled_root,
    ". Run 06_merge_annual_sightings.R first."
  )
}
message("Cross-referencing merged annual year(s): ", paste(years, collapse = ", "))

compiled <- map_dfr(years, function(yr) {
  read_csv(resolve_dedup_file(compiled_root, "nbw_annual_sightings", yr),
    col_types = cols(.default = col_character()), show_col_types = FALSE
  )
}) %>%
  transmute(
    compiled_sighting_id = record_id,
    compiled_source_year = as.integer(source_year),
    compiled_source_type = source_type,
    compiled_standard_date = standard_date,
    compiled_standard_time = standard_time,
    compiled_standard_datetime = standard_datetime,
    compiled_datetime = as_datetime(standard_datetime, tz = "UTC"),
    compiled_date = as.Date(standard_date),
    compiled_latitude = as_num(latitude),
    compiled_longitude = as_num(longitude),
    compiled_count = as_num(count),
    compiled_species_raw = species,
    compiled_source_file = source_file,
    compiled_source_sheet = source_sheet,
    compiled_source_row = source_row
  )

# ---- REFERENCE DATABASE READERS --------------------------------------------
# Each reader returns the same columns: ref_source_db, ref_source_row,
# ref_record_id, ref_datetime (UTC, or NA when the source has no reliable
# UTC basis), ref_date, ref_date_precision, ref_latitude, ref_longitude,
# ref_count, ref_species_raw, ref_comments, ref_source_file.

read_dfo_reference <- function(path) {
  read_excel(path, sheet = "Sheet1", col_types = "text") %>%
    mutate(ref_source_row = row_number()) %>%
    filter(str_detect(str_to_lower(coalesce(COMMONNAME, "")), nbw_species_regex)) %>%
    transmute(
      ref_source_db = "DFO",
      ref_source_row,
      ref_record_id = coalesce(na_text(WS_ID), as.character(ref_source_row)),
      ref_datetime = ymd_hms(WS_DATETIME_UTC, tz = "UTC", quiet = TRUE),
      ref_date = coalesce(
        as.Date(ref_datetime),
        as.Date(as_num(WS_DATE), origin = "1899-12-30")
      ),
      ref_date_precision = "datetime_utc",
      ref_latitude = as_num(LATITUDE),
      ref_longitude = as_num(LONGITUDE),
      ref_count = as_num(BEST_COUNT),
      ref_species_raw = COMMONNAME,
      ref_comments = COMMENTS,
      ref_source_file = basename(path)
    )
}

read_greenland_reference <- function(path) {
  # event_date mixes Excel-serial day numbers (stored as text) with season
  # placeholders such as "Summer"; those have no day-level date, so ref_date
  # is left NA and the record's `year` column is kept in ref_date_precision
  # instead. time_local has no stated UTC offset, so it is never used to
  # build ref_datetime -- it is kept only for audit, in ref_comments. Every
  # Greenland record therefore matches on date + coordinates only (the
  # date_coordinate_10m tier), never on exact time. The whole file is
  # pre-filtered to NBW by its source, so there is no species column to
  # filter on here.
  raw <- read_excel(path, sheet = "Sheet1", col_types = "text") %>%
    mutate(ref_source_row = row_number())
  event_date_num <- suppressWarnings(as.numeric(na_text(raw$event_date)))
  transmute(
    raw,
    ref_source_db = "Greenland",
    ref_source_row,
    ref_record_id = coalesce(na_text(record_id), as.character(ref_source_row)),
    ref_datetime = as.POSIXct(NA_real_, tz = "UTC"),
    ref_date = as.Date(event_date_num, origin = "1899-12-30"),
    ref_date_precision = if_else(is.na(event_date_num),
      paste0("year_only:", na_text(year)), "day"
    ),
    ref_latitude = as_num(decimal_latitude),
    ref_longitude = as_num(decimal_longitude),
    ref_count = coalesce(as_num(minimum_group_size), as_num(Catch_total)),
    ref_species_raw = "Northern Bottlenose Whale",
    ref_comments = paste0(
      coalesce(na_text(original_comments), ""),
      " | time_local (no UTC offset, not used for matching): ",
      coalesce(na_text(time_local), "NA")
    ),
    ref_source_file = basename(path)
  )
}

read_paamiut_reference <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(ref_source_row = row_number()) %>%
    filter(str_detect(str_to_lower(coalesce(Species, "")), nbw_species_regex)) %>%
    transmute(
      ref_source_db = "Paamiut",
      ref_source_row,
      ref_record_id = paste0("paamiut_", ref_source_row),
      ref_datetime = as.POSIXct(NA_real_, tz = "UTC"),
      ref_date = make_date(as.integer(Year), as.integer(Month), as.integer(Day)),
      ref_date_precision = "day",
      ref_latitude = as_num(Lat),
      ref_longitude = as_num(Long),
      ref_count = as_num(Count),
      ref_species_raw = Species,
      ref_comments = NA_character_,
      ref_source_file = basename(path)
    )
}

# Add a new reference database by appending one entry here.
reference_sources <- list(
  list(file = "DFO_NBW_Oct2026.xlsx", reader = read_dfo_reference),
  list(file = "Greenland_NBW_Oct2026.xlsx", reader = read_greenland_reference),
  list(file = "allnbwobs_paamiut_260522.csv", reader = read_paamiut_reference)
)

reference <- map_dfr(reference_sources, function(src) {
  path <- file.path(reference_root, src$file)
  if (!file.exists(path)) {
    message("Reference database not found, skipping: ", path)
    return(tibble())
  }
  src$reader(path)
})
if (!nrow(reference)) stop("No reference databases found under ", reference_root)
message(
  "Reference records loaded: ", nrow(reference), " across ",
  n_distinct(reference$ref_source_db), " database(s): ",
  paste(sort(unique(reference$ref_source_db)), collapse = ", ")
)

# ---- COORDINATE VALIDATION (reported, not fatal) ---------------------------
# Rows with missing or out-of-range coordinates are excluded from geometric
# matching but kept in the final output -- a missing-coordinate record is
# not a duplicate of anything and should not be dropped silently.

valid_coord <- function(lat, lon) {
  !is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180
}

valid_compiled <- compiled %>%
  filter(valid_coord(compiled_latitude, compiled_longitude))
valid_reference <- reference %>%
  filter(valid_coord(ref_latitude, ref_longitude))

validation <- bind_rows(
  tibble(
    dataset = "compiled_field_data",
    rows = nrow(compiled),
    usable_for_matching = nrow(valid_compiled),
    excluded_missing_or_invalid_coordinates = nrow(compiled) - nrow(valid_compiled)
  ),
  reference %>%
    group_by(dataset = ref_source_db) %>%
    summarise(
      rows = n(),
      usable_for_matching = sum(valid_coord(ref_latitude, ref_longitude)),
      excluded_missing_or_invalid_coordinates = sum(!valid_coord(ref_latitude, ref_longitude)),
      .groups = "drop"
    )
)
write_csv(validation, file.path(qa_dir, "nbw_compiled_reference_validation.csv"), na = "")
message(
  "Rows excluded from geometric matching for missing/invalid coordinates: ",
  sum(validation$excluded_missing_or_invalid_coordinates), " (kept in final output)"
)

# ---- TIERED CANDIDATE MATCHING ---------------------------------------------
# The first applicable tier (by rank) is kept as a pair's best tier.
# date_coordinate_10m is independent of time, so a reference row with no
# reliable UTC timestamp (Greenland, Paamiut) can still be matched.

match_rules <- tibble(
  match_tier = c("exact", "very_close", "close", "broad", "date_coordinate_10m"),
  tier_rank = 1:5
)

all_pairs <- merge(
  valid_compiled %>% mutate(join_id = 1L),
  valid_reference %>% mutate(join_id = 1L),
  by = "join_id"
) %>%
  as_tibble() %>%
  select(-join_id) %>%
  mutate(
    time_difference_seconds = abs(as.numeric(difftime(compiled_datetime, ref_datetime, units = "secs"))),
    distance_m = haversine_m(compiled_latitude, compiled_longitude, ref_latitude, ref_longitude),
    same_utc_date = !is.na(compiled_date) & !is.na(ref_date) & compiled_date == ref_date,
    date_coordinate_10m_match = same_utc_date & distance_m <= 10
  )

time_candidates <- all_pairs %>%
  mutate(
    match_tier = case_when(
      !is.na(time_difference_seconds) & time_difference_seconds <= 1 & distance_m <= 10 ~ "exact",
      !is.na(time_difference_seconds) & time_difference_seconds <= 60 & distance_m <= 100 ~ "very_close",
      !is.na(time_difference_seconds) & time_difference_seconds <= 300 & distance_m <= 500 ~ "close",
      !is.na(time_difference_seconds) & time_difference_seconds <= 900 & distance_m <= 1000 ~ "broad",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(match_tier))

date_coordinate_candidates <- all_pairs %>%
  filter(date_coordinate_10m_match) %>%
  mutate(match_tier = "date_coordinate_10m")

candidate_pairs <- bind_rows(time_candidates, date_coordinate_candidates) %>%
  left_join(match_rules, by = "match_tier") %>%
  group_by(compiled_sighting_id, ref_record_id, ref_source_db) %>%
  arrange(tier_rank, time_difference_seconds, distance_m, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

candidate_counts <- candidate_pairs %>% count(compiled_sighting_id, name = "compiled_candidate_count")
ref_candidate_counts <- candidate_pairs %>% count(ref_record_id, ref_source_db, name = "ref_candidate_count")

candidate_pairs <- candidate_pairs %>%
  left_join(candidate_counts, by = "compiled_sighting_id") %>%
  left_join(ref_candidate_counts, by = c("ref_record_id", "ref_source_db")) %>%
  mutate(
    one_to_many = compiled_candidate_count > 1,
    many_to_one = ref_candidate_count > 1,
    ambiguous_pair = one_to_many | many_to_one
  ) %>%
  arrange(tier_rank, time_difference_seconds, distance_m, compiled_sighting_id, ref_source_db)

candidate_output <- candidate_pairs %>%
  select(
    compiled_sighting_id, compiled_source_year, compiled_source_type,
    compiled_standard_datetime, compiled_latitude, compiled_longitude, compiled_count,
    ref_source_db, ref_record_id, ref_source_row, ref_date, ref_date_precision,
    ref_datetime, ref_latitude, ref_longitude, ref_count,
    time_difference_seconds, distance_m, match_tier, tier_rank,
    date_coordinate_10m_match, compiled_candidate_count, ref_candidate_count,
    one_to_many, many_to_one, ambiguous_pair,
    compiled_source_file, compiled_source_row, compiled_species_raw,
    ref_species_raw, ref_comments
  )
write_csv(candidate_output, file.path(qa_dir, "nbw_compiled_reference_candidate_pairs.csv"), na = "")
write_csv(candidate_output %>% filter(match_tier == "exact"),
  file.path(qa_dir, "nbw_compiled_reference_exact_matches.csv"),
  na = ""
)

# Keep one best candidate per compiled sighting; ties at the same tier are
# kept and flagged rather than arbitrarily broken.
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)

best_rank <- candidate_pairs %>%
  group_by(compiled_sighting_id) %>%
  summarise(
    best_tier_rank = min(tier_rank),
    best_time_difference_seconds = safe_min(time_difference_seconds[tier_rank == min(tier_rank)]),
    best_distance_m = safe_min(distance_m[tier_rank == min(tier_rank)]),
    .groups = "drop"
  )

best_matches <- candidate_pairs %>%
  inner_join(best_rank, by = "compiled_sighting_id") %>%
  filter(
    tier_rank == best_tier_rank,
    coalesce(time_difference_seconds == best_time_difference_seconds, TRUE),
    coalesce(distance_m == best_distance_m, TRUE)
  ) %>%
  group_by(compiled_sighting_id) %>%
  mutate(best_match_tie_count = n(), ambiguous_best_match = n() > 1) %>%
  arrange(ref_source_db, ref_record_id, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(
    compiled_sighting_id, compiled_standard_datetime, compiled_latitude, compiled_longitude,
    compiled_count, ref_source_db, ref_record_id, ref_date, ref_datetime,
    ref_latitude, ref_longitude, ref_count, time_difference_seconds, distance_m,
    match_tier, tier_rank, best_match_tie_count, ambiguous_best_match,
    compiled_candidate_count, ref_candidate_count, one_to_many, many_to_one,
    ambiguous_pair, compiled_source_file, compiled_source_row
  ) %>%
  arrange(compiled_sighting_id, tier_rank, time_difference_seconds, distance_m)
write_csv(best_matches, file.path(qa_dir, "nbw_compiled_reference_best_matches.csv"), na = "")

match_summary <- bind_rows(
  tibble(
    metric = c(
      "compiled_records", "reference_records", "candidate_pairs",
      "compiled_records_with_any_candidate", "compiled_records_without_candidate",
      "ambiguous_compiled_records"
    ),
    value = c(
      nrow(compiled), nrow(reference), nrow(candidate_pairs),
      n_distinct(candidate_pairs$compiled_sighting_id),
      nrow(compiled) - n_distinct(candidate_pairs$compiled_sighting_id),
      n_distinct(candidate_output$compiled_sighting_id[candidate_output$ambiguous_pair])
    )
  ),
  best_matches %>% count(ref_source_db, match_tier, name = "value") %>%
    transmute(metric = paste0(ref_source_db, "_best_", match_tier), value)
)
write_csv(match_summary, file.path(qa_dir, "nbw_compiled_reference_match_summary.csv"), na = "")

# ---- DEDUPLICATION ----------------------------------------------------------
# Only exact, very_close, or same-date-within-10m matches are treated as the
# same sighting already present in a reference database, and the compiled
# (field) record is omitted in favour of the reference record. close/broad
# candidates remain in the audit for manual review but are not auto-removed.

dedup_omit_ids <- candidate_output %>%
  filter(match_tier %in% c("exact", "very_close", "date_coordinate_10m")) %>%
  distinct(compiled_sighting_id) %>%
  pull(compiled_sighting_id)

deduplication_audit <- best_matches %>%
  mutate(
    deduplication_action = if_else(
      compiled_sighting_id %in% dedup_omit_ids,
      "omit_compiled_keep_reference",
      "retain_compiled"
    )
  ) %>%
  arrange(deduplication_action, compiled_sighting_id)
write_csv(deduplication_audit, file.path(qa_dir, "nbw_compiled_reference_deduplication_audit.csv"), na = "")

compiled_deduplicated <- compiled %>%
  filter(!compiled_sighting_id %in% dedup_omit_ids) %>%
  transmute(
    record_source = "field_compiled",
    record_id = compiled_sighting_id,
    standard_date = as.character(compiled_date),
    standard_time = compiled_standard_time,
    standard_datetime = compiled_standard_datetime,
    latitude = compiled_latitude,
    longitude = compiled_longitude,
    count = compiled_count,
    species = compiled_species_raw,
    source_detail = compiled_source_type,
    source_file = compiled_source_file,
    source_row = compiled_source_row,
    deduplication_status = "retained_field_compiled_no_reference_duplicate"
  )

reference_retained <- reference %>%
  transmute(
    record_source = ref_source_db,
    record_id = ref_record_id,
    standard_date = as.character(ref_date),
    standard_time = NA_character_,
    standard_datetime = if_else(is.na(ref_datetime), NA_character_,
      format(ref_datetime, "%Y-%m-%dT%H:%M:%SZ")
    ),
    latitude = ref_latitude,
    longitude = ref_longitude,
    count = ref_count,
    species = ref_species_raw,
    source_detail = ref_date_precision,
    source_file = ref_source_file,
    source_row = as.character(ref_source_row),
    deduplication_status = "retained_reference_database_record"
  )

deduplicated_combined <- bind_rows(reference_retained, compiled_deduplicated) %>%
  arrange(record_source, standard_datetime, record_id)
write_csv(deduplicated_combined,
  file.path(out_dir, "nbw_sightings_compiled_vs_reference_deduplicated.csv"),
  na = ""
)

# ---- QUICK SPATIAL QA MAP ---------------------------------------------------

map_data <- deduplicated_combined %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(map_date = suppressWarnings(as.Date(standard_date)))

map_path <- file.path(out_dir, "nbw_sightings_compiled_vs_reference_map.png")
if (nrow(map_data) > 0) {
  date_range <- range(map_data$map_date, na.rm = TRUE)
  map_land <- ne_countries(scale = 50, returnclass = "sf")
  map_eez <- NULL
  if (file.exists(eez_path)) {
    map_eez <- st_read(eez_path, quiet = TRUE) %>% st_transform(st_crs(map_land))
  } else {
    warning("EEZ layer not found at ", eez_path, "; map will omit EEZ boundaries.")
  }
  map_plot <- ggplot() +
    geom_sf(data = map_land, fill = "grey88", color = "grey55", linewidth = 0.25) +
    {
      if (!is.null(map_eez)) {
        geom_sf(
          data = map_eez, aes(color = "EEZ"), fill = NA,
          linewidth = 0.55, linetype = "dashed", inherit.aes = FALSE
        )
      }
    } +
    geom_point(
      data = map_data, aes(x = longitude, y = latitude, color = record_source),
      alpha = 0.6, size = 1.3
    ) +
    coord_sf(
      xlim = range(map_data$longitude, na.rm = TRUE) + c(-1, 1),
      ylim = range(map_data$latitude, na.rm = TRUE) + c(-1, 1),
      expand = FALSE
    ) +
    labs(
      title = "NBW sightings: compiled field data vs. reference databases",
      subtitle = paste0("Date range: ", date_range[1], " to ", date_range[2]),
      x = "Longitude", y = "Latitude", color = "Record source"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(map_path, map_plot, width = 10, height = 8, units = "in", dpi = 300)
  message("Map written to: ", map_path)
} else {
  message("No valid coordinates available; map not written.")
}

# ---- SUMMARY -----------------------------------------------------------------

summary_lines <- c(
  "Tiered compiled NBW sightings vs. reference database cross-reference",
  paste("Compiled field years:", paste(years, collapse = ", ")),
  paste("Compiled field records:", nrow(compiled)),
  paste(
    "Reference records:", nrow(reference), "across",
    paste(sort(unique(reference$ref_source_db)), collapse = ", ")
  ),
  paste("Candidate pairs within matching rules:", nrow(candidate_pairs)),
  paste("Compiled records with any candidate:", n_distinct(candidate_pairs$compiled_sighting_id)),
  paste(
    "Compiled records without candidate:",
    nrow(compiled) - n_distinct(candidate_pairs$compiled_sighting_id)
  ),
  paste("Compiled records omitted (duplicate of a reference record):", length(dedup_omit_ids)),
  paste(
    "Ambiguous compiled records (one-to-many or many-to-one):",
    n_distinct(candidate_output$compiled_sighting_id[candidate_output$ambiguous_pair])
  ),
  "",
  "Tier definitions:",
  "exact = <= 1 second and <= 10 m",
  "very_close = <= 1 minute and <= 100 m",
  "close = <= 5 minutes and <= 500 m",
  "broad = <= 15 minutes and <= 1 km",
  "date_coordinate_10m = same UTC calendar date and <= 10 m, even when a timestamp is missing",
  "Distance uses the haversine geodesic on a WGS84-like spherical Earth radius; source files were not modified.",
  "Deduplicated output omits only exact, very-close, or date/coordinate-within-10-m matches; close and broad candidates are retained for review.",
  "Greenland and Paamiut reference records have no reliable UTC timestamp and are matched on date + coordinates only."
)
writeLines(summary_lines, file.path(qa_dir, "nbw_compiled_reference_crossref_summary.txt"))
cat(paste(summary_lines, collapse = "\n"), "\n")
