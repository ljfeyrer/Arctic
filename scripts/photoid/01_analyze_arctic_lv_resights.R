# ==============================================================================
# Script: Arctic NBW Photo-ID Catalogue -- Resight & Association Analysis (step 2 of 2)
# Author: Laura Joan Feyrer
# Date Updated: 2026-11-16
# Description: Build the per-encounter sightings table, calculate all
#              pairwise resights per ID-side, categorize them (within/between
#              days/weeks/years), and find within-day associations (ID-sides
#              seen together). Reads the cleaned data written by
#              scripts/photoid/00_clean_arctic_lv.R; writes the tables
#              consumed by scripts/photoid/Arctic_NBW_PhotoID_Report.Rmd.
#
#              TERMINOLOGY: IDs include side (e.g., AR2081-L, AR2081-R are
#              separate IDs).
#
# Numbered 00_/01_ within scripts/photoid/ only -- unrelated to the main
# 00-08 sightings pipeline in scripts/.
#
# This script (and the Rmd) is the genuinely Arctic-specific layer -- the
# resighting/association/mapping analysis for this survey area. It is kept
# separate from the cleaning step so the catalogue can be re-cleaned (e.g.
# after a new LV export) without recomputing everything by hand, and so this
# analysis can move with the data if it is ever split into its own project
# (see the note in 00_clean_arctic_lv.R about mark-recapture).
#
# Inputs:
#   OUTPUT/PhotoID/LV_ARC_encounters.rds, OUTPUT/PhotoID/quality_ids_arctic.rds
#   (written by scripts/photoid/00_clean_arctic_lv.R)
#
# Outputs (all under OUTPUT/PhotoID/):
#   sightings_arctic.rds
#   resights_encounter_level.rds, resights_categorized.rds
#   {within_day,between_day,between_week,between_year}_resights.csv
#   association_frequency.csv
# ==============================================================================

# LIBRARIES ====================================================================
pacman::p_load(dplyr, here, tidyverse, stringr, readr, lubridate, purrr)

source(here("scripts", "lib", "helpers.R"))

output_dir <- here("OUTPUT", "PhotoID")

# LOAD CLEANED DATA =============================================================
cat("\n=== LOADING CLEANED DATA ===\n")

cleaned_files <- c("LV_ARC_encounters.rds", "quality_ids_arctic.rds")
missing <- cleaned_files[!file.exists(file.path(output_dir, cleaned_files))]
if (length(missing)) {
  stop(
    "Missing cleaned data in ", output_dir, ": ", paste(missing, collapse = ", "),
    ". Run scripts/photoid/00_clean_arctic_lv.R first."
  )
}

LV_ARC_encounters <- readRDS(file.path(output_dir, "LV_ARC_encounters.rds"))
quality_ids <- readRDS(file.path(output_dir, "quality_ids_arctic.rds"))
LV_ARC_quality <- LV_ARC_encounters # already filtered to quality IDs by the cleaning step

cat("Loaded", nrow(LV_ARC_encounters), "cleaned records,", nrow(quality_ids), "quality IDs\n")

encounter_summary <- LV_ARC_encounters %>%
  group_by(ID_side, ID, side, Date, encounter_id) %>%
  summarise(
    n_photos = n(),
    start_time = min(datetime),
    end_time = max(datetime),
    duration_mins = as.numeric(difftime(max(datetime), min(datetime), units = "mins")),
    .groups = "drop"
  )

# RESIGHTING ANALYSIS ==========================================================
cat("\n=== RESIGHTING ANALYSIS ===\n")

# Create a table of all sightings (unique ID_side-Date-Encounter combinations)
sightings <- encounter_summary %>%
  select(ID_side, ID, side, Date, encounter_id, start_time, n_photos, duration_mins) %>%
  arrange(ID_side, Date, start_time)

# Add coordinates for each encounter
sighting_coords <- LV_ARC_encounters %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  group_by(ID_side, Date, encounter_id) %>%
  summarise(
    Latitude = mean(Latitude, na.rm = TRUE),
    Longitude = mean(Longitude, na.rm = TRUE),
    has_gps = TRUE,
    .groups = "drop"
  )

sightings <- sightings %>%
  left_join(sighting_coords, by = c("ID_side", "Date", "encounter_id")) %>%
  mutate(has_gps = replace_na(has_gps, FALSE))

cat(
  "Sightings with GPS coordinates:", sum(sightings$has_gps),
  "of", nrow(sightings),
  paste0("(", round(100 * sum(sightings$has_gps) / nrow(sightings), 1), "%)\n")
)

# Calculate all pairwise resights for one ID_side's sightings, vectorized over
# every pair at once instead of looping row-by-row (this scales as more years
# of data accumulate). distances use the shared haversine_m() from
# scripts/lib/helpers.R rather than a separate geosphere dependency.
calculate_resights <- function(id_data) {
  if (nrow(id_data) < 2) {
    return(NULL)
  }

  pairs <- combn(nrow(id_data), 2)
  i <- pairs[1, ]
  j <- pairs[2, ]

  both_have_gps <- id_data$has_gps[i] & id_data$has_gps[j]
  distance_km <- rep(NA_real_, length(i))
  distance_km[both_have_gps] <- haversine_m(
    id_data$Latitude[i][both_have_gps], id_data$Longitude[i][both_have_gps],
    id_data$Latitude[j][both_have_gps], id_data$Longitude[j][both_have_gps]
  ) / 1000

  days_diff <- as.numeric(difftime(id_data$Date[j], id_data$Date[i], units = "days"))

  tibble(
    ID_side = id_data$ID_side[i],
    ID = id_data$ID[i],
    side = id_data$side[i],
    sighting1_date = id_data$Date[i],
    sighting1_time = id_data$start_time[i],
    sighting1_encounter = id_data$encounter_id[i],
    sighting1_has_gps = id_data$has_gps[i],
    sighting2_date = id_data$Date[j],
    sighting2_time = id_data$start_time[j],
    sighting2_encounter = id_data$encounter_id[j],
    sighting2_has_gps = id_data$has_gps[j],
    time_diff_hours = as.numeric(difftime(id_data$start_time[j], id_data$start_time[i], units = "hours")),
    days_diff = days_diff,
    weeks_diff = days_diff / 7,
    distance_km = distance_km,
    both_have_gps = both_have_gps,
    same_day = id_data$Date[i] == id_data$Date[j],
    same_week = abs(days_diff / 7) < 1,
    same_month = format(id_data$Date[i], "%Y-%m") == format(id_data$Date[j], "%Y-%m"),
    same_year = format(id_data$Date[i], "%Y") == format(id_data$Date[j], "%Y")
  )
}

# Calculate resights for all ID_sides
cat("Calculating pairwise resights...\n")
all_resights <- sightings %>%
  group_by(ID_side) %>%
  group_split() %>%
  map_dfr(calculate_resights)

if (!is.null(all_resights) && nrow(all_resights) > 0) {
  cat("Total resight pairs (encounter-level):", nrow(all_resights), "\n")

  # Categorize resights using weeks instead of months
  resights_categorized <- all_resights %>%
    mutate(
      resight_type = case_when(
        same_day & time_diff_hours < 24 ~ "Within_Day",
        weeks_diff <= 4 ~ "Within_Month", # ~4 weeks = 1 month
        weeks_diff <= 52 ~ "Within_Year", # ~52 weeks = 1 year
        TRUE ~ "Between_Years"
      )
    )

  cat("\nResight type distribution (encounter-level):\n")
  print(table(resights_categorized$resight_type))

  # CREATE DATE-LEVEL RESIGHTS (for summary tables) =============================
  # When the same ID-side was seen multiple times on the same date (different
  # encounters >30 mins apart), keep only one representative row per date-pair.
  # This prevents duplicate rows in summary tables while preserving the full
  # encounter-level detail in resights_categorized.
  cat("\nCreating date-level resights (deduplicating multiple encounters per date)...\n")

  resights_date_level <- resights_categorized %>%
    group_by(ID_side, side, sighting1_date, sighting2_date) %>%
    # Keep the pair with the longest time gap (most representative)
    slice_max(order_by = days_diff, n = 1, with_ties = FALSE) %>%
    ungroup()

  cat("Date-level resight pairs:", nrow(resights_date_level), "\n")
  cat(
    "Reduction:", nrow(resights_categorized) - nrow(resights_date_level),
    "duplicate date-pairs removed\n"
  )

  # Within-day resights (different encounters on same day)
  within_day_resights <- resights_categorized %>%
    filter(resight_type == "Within_Day") %>%
    arrange(ID_side, sighting1_date, time_diff_hours)

  cat("\nWithin-day resights (different 30-min encounters):", nrow(within_day_resights), "\n")

  # Between-day resights
  between_day_resights <- resights_categorized %>%
    filter(days_diff >= 1) %>%
    arrange(ID_side, sighting1_date)

  cat("Between-day resights:", nrow(between_day_resights), "\n")

  # Between-week resights (more than 1 week apart)
  between_week_resights <- resights_categorized %>%
    filter(weeks_diff >= 1) %>%
    arrange(ID_side, sighting1_date)

  cat("Between-week resights (>1 week apart):", nrow(between_week_resights), "\n")

  # Between-year resights
  between_year_resights <- resights_categorized %>%
    filter(!same_year) %>%
    arrange(ID_side, sighting1_date)

  cat("Between-year resights:", nrow(between_year_resights), "\n")

  # IDs with resights
  ids_with_resights <- unique(all_resights$ID_side)
  cat("IDs with resights:", length(ids_with_resights), "\n")

  # GPS coverage stats
  cat("\nGPS coverage in resights:\n")
  cat(
    "Both sightings have GPS:", sum(resights_categorized$both_have_gps),
    "of", nrow(resights_categorized),
    paste0("(", round(100 * sum(resights_categorized$both_have_gps) / nrow(resights_categorized), 1), "%)\n")
  )
} else {
  cat("No resights found\n")
  resights_categorized <- NULL
  resights_date_level <- NULL
  within_day_resights <- NULL
  between_day_resights <- NULL
  between_week_resights <- NULL
  between_year_resights <- NULL
}

# WITHIN-DAY ASSOCIATIONS ======================================================
cat("\n=== WITHIN-DAY ASSOCIATIONS ===\n")

# Find ID_sides seen together on the same day (within 30 minutes)
within_day_associations <- LV_ARC_encounters %>%
  select(ID_side, ID, side, Date, datetime, encounter_id, Latitude, Longitude) %>%
  inner_join(
    LV_ARC_encounters %>% select(ID_side, ID, side, Date, datetime, encounter_id),
    by = c("Date", "encounter_id"),
    relationship = "many-to-many",
    suffix = c("_1", "_2")
  ) %>%
  filter(ID_side_1 < ID_side_2) %>% # Avoid duplicates and self-matches
  distinct(ID_side_1, ID_side_2, ID_1, ID_2, side_1, side_2, Date, encounter_id) %>%
  arrange(Date, ID_side_1, ID_side_2)

if (nrow(within_day_associations) > 0) {
  cat("Total within-day associations:", nrow(within_day_associations), "\n")

  # Count association frequency by encounters
  association_frequency <- within_day_associations %>%
    mutate(
      side_combo = case_when(
        side_1 == "Left" & side_2 == "Left" ~ "Left",
        side_1 == "Right" & side_2 == "Right" ~ "Right",
        TRUE ~ "Left-Right"
      )
    ) %>%
    group_by(ID_side1 = ID_side_1, ID_side2 = ID_side_2, side_combo) %>%
    summarise(
      n_encounters_together = n(),
      n_days_together = n_distinct(Date),
      dates = paste(sort(unique(Date)), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_encounters_together), ID_side1, ID_side2)

  cat("Unique ID-side pairs with associations:", nrow(association_frequency), "\n")

  # Summary by side
  cat("\nAssociations by side:\n")
  side_summary <- association_frequency %>%
    group_by(side_combo) %>%
    summarise(
      n_pairs = n(),
      total_encounters = sum(n_encounters_together),
      .groups = "drop"
    )
  print(side_summary)

  # Show most frequent associations (exclude L-R pairs)
  cat("\nTop associations (ID-sides seen together in multiple encounters, same-side only):\n")
  same_side_assoc <- association_frequency %>%
    filter(n_encounters_together > 1, side_combo != "Left-Right")
  if (nrow(same_side_assoc) > 0) {
    print(head(same_side_assoc, 10))
  } else {
    cat("No same-side associations with multiple encounters\n")
  }
} else {
  cat("No within-day associations found\n")
  association_frequency <- NULL
}

# SAVE RESIGHT/ASSOCIATION DATA =================================================
cat("\n=== SAVING RESIGHT/ASSOCIATION DATA ===\n")

saveRDS(sightings, here(output_dir, "sightings_arctic.rds"))

if (!is.null(resights_categorized)) {
  # Save BOTH versions:
  # 1. Full encounter-level detail (for advanced analyses)
  saveRDS(resights_categorized, here(output_dir, "resights_encounter_level.rds"))

  # 2. Date-level deduplicated (for summary tables in RMarkdown)
  saveRDS(resights_date_level, here(output_dir, "resights_categorized.rds"))

  write_csv(within_day_resights, here(output_dir, "within_day_resights.csv"))
  write_csv(between_day_resights, here(output_dir, "between_day_resights.csv"))
  write_csv(between_week_resights, here(output_dir, "between_week_resights.csv"))
  write_csv(between_year_resights, here(output_dir, "between_year_resights.csv"))
}

if (!is.null(association_frequency)) {
  write_csv(association_frequency, here(output_dir, "association_frequency.csv"))
}

cat("\n=== RESIGHT/ASSOCIATION ANALYSIS COMPLETE ===\n")
cat("Saved to:", output_dir, "\n")
cat("\nNext step: knit scripts/photoid/Arctic_NBW_PhotoID_Report.Rmd\n")
