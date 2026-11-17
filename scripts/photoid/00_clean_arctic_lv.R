# ==============================================================================
# Script: Arctic NBW Photo-ID Catalogue -- Cleaning (step 1 of 2)
# Author: Laura Joan Feyrer
# Date Updated: 2026-11-16
# Description: Clean the Arctic LV catalogue export, classify photo side and
#              quality, and group photos into encounters (30-minute rule).
#              Focus on 2017+. Earlier data (2003-2016, ASO/LAB) is out of
#              scope for this analysis.
#
#              TERMINOLOGY: IDs include side (e.g., AR2081-L, AR2081-R are
#              separate IDs).
#
# This is step 1 of 2 (both numbered 00_/01_ within scripts/photoid/, not the
# main 00-08 sightings pipeline in scripts/ -- the two numbering sequences are
# unrelated). Run scripts/photoid/01_analyze_arctic_lv_resights.R next for the
# resighting/association analysis, then knit
# scripts/photoid/Arctic_NBW_PhotoID_Report.Rmd.
# This script was previously one file that also did the resight/association
# analysis; it's split so the cleaning step can be rerun on its own when a
# new LV export arrives, without recomputing everything downstream by hand.
#
# This script re-derives the base cleaning already done more thoroughly in
# ~/CODE/PhotoID's 01_clean_lv_2025.R -> 03_finalize_LV.r pipeline (a
# separate project covering all photo-ID catalogues, not just Arctic). The
# two have already drifted from each other; consuming that project's
# finalized output directly instead of this independent re-cleaning is a
# bigger change, deliberately deferred -- likely worth doing once this data
# also feeds a mark-recapture analysis.
# ==============================================================================

# LIBRARIES ====================================================================
pacman::p_load(dplyr, here, tidyverse, stringr, readr, lubridate)

source(here("scripts", "lib", "helpers.R"))

# CONFIGURATION ================================================================
# OUTPUT/PhotoID/ holds this catalogue analysis only. The yearly LV photo
# encounters compared against the Cetaceans sightings are a separate product
# under OUTPUT/LV_encounters/ (scripts/09_compare_lv_encounters_to_cetaceans.R).
output_dir <- here("OUTPUT", "PhotoID")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Define year threshold
YEAR_START <- 2017

# DATA IMPORT ==================================================================
cat("\n=== IMPORTING DATA ===\n")

# Import catalogue data (adjust path as needed)
LV_ARC <- read.csv(
  here("INPUT/catalogue_files/LV_Arctic_2017_2024.csv"),
  colClasses = "character"
)

cat("Total records imported:", nrow(LV_ARC), "\n")

# BASIC VARIABLE SETUP =========================================================
cat("\n=== BASIC VARIABLE SETUP ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    Date1 = Date.Original,
    keyword = Keyword.export,
    ID_original = Title,
    ID = Title,
    Folder_original = Folder # Keep original folder info (year or dataset)
  )

# Note: ID_side will be created after side classification to distinguish L/R photos

# CLEAN INDIVIDUAL ID ==========================================================
cat("\n=== ID CLEANING ===\n")

# Arctic IDs are alphanumeric codes (e.g., AR2002, LAB01, ASO02)
# Keep as character strings - do NOT convert to numeric

# Filter out invalid ID values
cat("Records before ID filtering:", nrow(LV_ARC), "\n")

# Check for IDs to remove
invalid_ids <- LV_ARC %>%
  filter(
    is.na(ID) | ID == "" | trimws(ID) == "" |
      grepl("see crops", ID, ignore.case = TRUE) |
      grepl("^UNK", ID, ignore.case = TRUE)
  ) %>%
  group_by(ID, ID_original, Folder_original) %>%
  summarise(N_Photos = n(), .groups = "drop") %>%
  arrange(desc(N_Photos))

if (nrow(invalid_ids) > 0) {
  cat("\nWARNING: Found", nrow(invalid_ids), "ID values to remove:\n")
  print(invalid_ids)
  write_csv(invalid_ids, here(output_dir, "removed_ids_arctic.csv"))
  cat("\nExported to: OUTPUT/PhotoID/removed_ids_arctic.csv\n")
}

# Remove invalid IDs
LV_ARC <- LV_ARC %>%
  filter(
    !is.na(ID),
    ID != "",
    trimws(ID) != "",
    !grepl("see crops", ID, ignore.case = TRUE),
    !grepl("^UNK", ID, ignore.case = TRUE)
  )

cat("Records after removing invalid IDs:", nrow(LV_ARC), "\n")
cat("Photos removed:", nrow(invalid_ids), "\n\n")

# Show ID format examples
cat("Example IDs:\n")
print(head(unique(LV_ARC$ID), 10))
cat("\nUnique IDs:", n_distinct(LV_ARC$ID), "\n")

# PHOTO SIDE CLASSIFICATION ====================================================
cat("\n=== SIDE CLASSIFICATION ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    side = case_when(
      grepl("Left|left", keyword) ~ "Left",
      grepl("Right|right", keyword) ~ "Right",
      TRUE ~ "UNK"
    )
  )

unknown_sides <- LV_ARC %>% filter(side == "UNK")
if (nrow(unknown_sides) > 0) {
  cat("WARNING: Found", nrow(unknown_sides), "photos without side information\n")
  cat("These will be removed\n")
}

LV_ARC <- LV_ARC %>% filter(side != "UNK")
cat("Records after removing unknown sides:", nrow(LV_ARC), "\n")

# CREATE ID (INCLUDES SIDE) ====================================================
cat("\n=== CREATING ID VARIABLE ===\n")

# Combine catalogue number and side to create THE ID used throughout analysis
# Example: AR2081-L and AR2081-R are treated as separate, independent IDs
LV_ARC <- LV_ARC %>%
  mutate(ID_side = paste(ID, side, sep = "-"))

cat("Unique Left IDs:", n_distinct(LV_ARC$ID_side[LV_ARC$side == "Left"]), "\n")
cat("Unique Right IDs:", n_distinct(LV_ARC$ID_side[LV_ARC$side == "Right"]), "\n")

cat("Examples: ", paste(head(unique(LV_ARC$ID_side), 5), collapse = ", "), "\n")

# PHOTO QUALITY RATING =========================================================
cat("\n=== QUALITY RATING ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    QRATE = case_when(
      grepl("\\* \\* \\* \\*", Rating) ~ 4,
      grepl("\\* \\* \\*", Rating) ~ 3,
      grepl("\\* \\*", Rating) ~ 2,
      grepl("\\*", Rating) ~ 1,
      TRUE ~ NA_real_
    )
  )

cat("Rating distribution:\n")
print(table(LV_ARC$QRATE, useNA = "ifany"))


# DATE AND TIME FORMATTING =====================================================
cat("\n=== DATE AND TIME FORMATTING ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    # Parse datetime
    datetime = ymd_hms(Date1, quiet = TRUE),
    Date = as.Date(datetime),
    YEAR = as.numeric(format(Date, "%Y")),
    MONTH = as.numeric(format(Date, "%m")),
    DAY = as.numeric(format(Date, "%d")),
    # Extract time components
    HOUR = hour(datetime),
    MINUTE = minute(datetime),
    time_decimal = HOUR + MINUTE / 60
  )

cat(
  "Date range:", format(min(LV_ARC$Date, na.rm = TRUE), "%Y-%m-%d"),
  "to", format(max(LV_ARC$Date, na.rm = TRUE), "%Y-%m-%d"), "\n"
)
cat(
  "Years:", min(LV_ARC$YEAR, na.rm = TRUE), "to",
  max(LV_ARC$YEAR, na.rm = TRUE), "\n"
)

LV_ARC <- LV_ARC %>% filter(YEAR >= 2017)

# DATASET CLASSIFICATION =======================================================
cat("\n=== DATASET CLASSIFICATION ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    Dataset = case_when(
      Folder_original == "ASO" ~ "ASO",
      Folder_original == "2003" ~ "LAB",
      YEAR >= YEAR_START ~ paste0("ARC_", YEAR),
      TRUE ~ "Early"
    ),
    Period = ifelse(YEAR >= YEAR_START, "ok", "other")
  )

cat("\nDataset distribution:\n")
print(table(LV_ARC$Dataset, useNA = "ifany"))


# SPATIAL DATA =================================================================
cat("\n=== SPATIAL DATA ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

# Count photos with coordinates
photos_with_coords <- sum(!is.na(LV_ARC$Latitude) & !is.na(LV_ARC$Longitude))
cat(
  "Photos with coordinates:", photos_with_coords,
  "of", nrow(LV_ARC),
  paste0("(", round(100 * photos_with_coords / nrow(LV_ARC), 1), "%)\n")
)

# RELIABILITY FLAG =============================================================
cat("\n=== RELIABILITY FLAG ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    Reliable = if_else(
      grepl("Indent|Notch|indent|notch|Nick", keyword),
      "Yes",
      "No"
    )
  )

cat("Reliable photos (with indents/notches):", sum(LV_ARC$Reliable == "Yes"), "\n")

# IDENTIFY QUALITY IDs =========================================================
cat("\n=== IDENTIFYING QUALITY IDs ===\n")

# First, identify which IDs have 3*+ quality photos
quality_id_list <- LV_ARC %>%
  filter(QRATE >= 3) %>%
  distinct(ID_side) %>%
  pull(ID_side)

cat("IDs with 3*+ photos:", length(quality_id_list), "\n")

# Filter dataset to only quality IDs for all downstream analyses
cat("\n=== FILTERING TO QUALITY IDs ONLY ===\n")
cat("Total records before filtering:", nrow(LV_ARC), "\n")

LV_ARC_quality <- LV_ARC %>%
  filter(ID_side %in% quality_id_list)

cat("Records after filtering to 3*+ IDs:", nrow(LV_ARC_quality), "\n")
cat("Left IDs retained:", LV_ARC %>%
  filter(side == "Left") %>%
  pull(ID_side) %>%
  n_distinct(), "\n")

cat("Right IDs retained:", LV_ARC %>%
  filter(side == "Right") %>%
  pull(ID_side) %>%
  n_distinct(), "\n")

# ENCOUNTER GROUPING (QUALITY IDS ONLY) =======================================
cat("\n=== ENCOUNTER GROUPING (QUALITY IDS ONLY) ===\n")
cat("Grouping photos into encounters (30-minute threshold)...\n")

# Group photos into encounters (within 30 minutes), per ID_side, using the
# same rule as the rest of the pipeline (scripts/lib/helpers.R).
LV_ARC_encounters <- LV_ARC_quality %>%
  arrange(ID_side, datetime) %>%
  group_by(ID_side) %>%
  mutate(encounter_id = group_into_encounters(datetime, gap_minutes = 30)) %>%
  ungroup()

# Summarize encounters
encounter_summary <- LV_ARC_encounters %>%
  group_by(ID_side, ID, side, Date, encounter_id) %>%
  summarise(
    n_photos = n(),
    start_time = min(datetime),
    end_time = max(datetime),
    duration_mins = as.numeric(difftime(max(datetime), min(datetime), units = "mins")),
    .groups = "drop"
  )

cat("Total encounters (quality IDs only):", nrow(encounter_summary), "\n")
cat(
  "Encounters per ID (average):",
  round(nrow(encounter_summary) / n_distinct(encounter_summary$ID_side), 1), "\n"
)

# Create full quality IDs summary
quality_ids <- LV_ARC_quality %>%
  filter(QRATE >= 3) %>%
  group_by(ID_side, ID, side) %>%
  summarise(
    n_photos_3plus = n(),
    n_photos_4star = sum(QRATE == 4),
    years_seen = paste(sort(unique(YEAR)), collapse = ", "),
    n_years = n_distinct(YEAR),
    first_year = min(YEAR),
    last_year = max(YEAR),
    .groups = "drop"
  )

# Add encounter counts
encounter_counts <- encounter_summary %>%
  group_by(ID_side) %>%
  summarise(
    n_encounters = n(),
    n_days = n_distinct(Date),
    .groups = "drop"
  )

quality_ids <- quality_ids %>%
  left_join(encounter_counts, by = "ID_side") %>%
  mutate(
    n_encounters = replace_na(n_encounters, 0),
    n_days = replace_na(n_days, 0)
  ) %>%
  arrange(desc(n_encounters), ID_side)

cat("Total IDs with 3*+ photos:", nrow(quality_ids), "\n")

# Quality IDs by year
quality_ids_by_year <- LV_ARC_quality %>%
  filter(QRATE >= 3) %>%
  group_by(YEAR, side) %>%
  summarise(
    n_photos = n(), n_unique_ids = n_distinct(ID_side),
    .groups = "drop"
  ) %>%
  filter(side %in% c("Left", "Right")) %>%
  tidyr::pivot_wider(
    names_from = side,
    values_from = c(n_photos, n_unique_ids),
    names_glue = "{side}_{.value}"
  ) %>%
  arrange(YEAR)

cat("\n3*+ IDs by year:\n")
print(quality_ids_by_year)

# SAVE CLEANED DATA ============================================================
cat("\n=== SAVING CLEANED DATA ===\n")

# Full dataset (quality IDs, 2017+, with encounter grouping)
saveRDS(LV_ARC_encounters, here(output_dir, "LV_ARC_encounters.rds"))

# Alias kept for clarity in downstream scripts; everything here is already
# filtered to YEAR >= YEAR_START, so this is currently identical to
# LV_ARC_encounters. Kept separate in case a future change (e.g. including
# sub-quality IDs upstream) makes the two diverge again.
saveRDS(LV_ARC_encounters, here(output_dir, "LV_ARC_reliable.rds"))

saveRDS(quality_ids, here(output_dir, "quality_ids_arctic.rds"))
write_csv(quality_ids_by_year, here(output_dir, "quality_ids_by_year.csv"))

cat("\n=== CLEANING COMPLETE ===\n")
cat("Saved cleaned data to:", output_dir, "\n")
cat("\nKey outputs:\n")
cat("- LV_ARC_encounters.rds / LV_ARC_reliable.rds: cleaned catalogue with ID-side and encounter grouping\n")
cat("- quality_ids_arctic.rds: ID-sides with 3*+ photos and encounter counts\n")
cat("- quality_ids_by_year.csv\n")
cat("\nNext step: Rscript scripts/photoid/01_analyze_arctic_lv_resights.R\n")
