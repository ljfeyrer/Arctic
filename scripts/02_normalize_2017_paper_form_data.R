#!/usr/bin/env Rscript

# Author: Laura Joan Feyrer
# Date created: 2026-11-17
# Date updated: 2026-11-17
# Description: Normalize the 2017 fisheries-survey paper forms (vessel:
# Pamiut, not Tarajoq -- these predate the Tarajoq digital tablet forms and
# were the template the later digital Cetaceans/Biopsy/Environment sheets
# were built from) into the same normalized shape as
# 01_normalize_yearly_form_data.R's output. Treated as Tarajoq-bucket data
# for now (source = "Tarajoq"); the real vessel is noted in every row's
# comments for traceability.
#
# Raw format is unlike every later year, so this gets its own script rather
# than branching 01_normalize_yearly_form_data.R's generic parsers:
#   - position is separate degree/minute columns (not a "67D23.4N" string),
#     with "MISS" as the missing-value placeholder;
#   - date and time-of-day are separate columns (a day serial plus a
#     fraction-of-day), not one combined datetime;
#   - count_min/count_best/count_max are headed "<"/"~"/">";
#   - BearingToAnimal combines bearing degrees and a port/starboard letter
#     in one cell (e.g. "160 P");
#   - there is no forms_exports/ per-sheet dedup step -- this is already a
#     single transcribed workbook, not several overlapping tablet exports.
#
# Input: input/orig_data/2017/forms_original/ArcticData2017_Clean.xlsx. Its
# data sheets are byte-identical to ArcticData2017.xlsx (confirmed
# 2026-11-17); "_Clean" only adds a "Meta data codes" legend sheet, used
# below to decode SPP_CER/Reaction. Original workbook is never modified.
#
# Output (same paths 03_compile_normalized_tarajoq_sightings.R and
# 08_compile_environment_biopsy.R already read for every other year -- no
# changes needed there):
#   input/normalized/2017/sightings/tarajoq_form_sightings_2017_standardized.csv
#   input/normalized/2017/environment/environment_2017_standardized.csv
#   input/normalized/2017/biopsy/biopsy_2017_standardized.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tibble)
})

source("scripts/lib/helpers.R")

year <- 2017L
path <- "input/orig_data/2017/forms_original/ArcticData2017_Clean.xlsx"
out_root <- file.path("input", "normalized", year)
vessel_note <- "vessel: Pamiut (2017 fisheries survey, paper forms transcribed to spreadsheet)"

# ---- SHARED PARSERS ----------------------------------------------------------

# Separate degree/minute columns, "MISS" as the missing-value placeholder.
# This dataset is Baffin Bay/Davis Strait only, so hemisphere is implied
# (always N/W) rather than given.
parse_dm <- function(deg, min) {
  deg <- na_text(deg)
  min <- na_text(min)
  deg[deg == "MISS"] <- NA_character_
  min[min == "MISS"] <- NA_character_
  suppressWarnings(as.numeric(deg) + as.numeric(min) / 60)
}

# Date is a bare day serial; time is a separate fraction-of-day. Combined
# only when both are present, so a missing time stays date-only rather than
# silently becoming midnight.
parse_serial_datetime <- function(date_serial, time_fraction) {
  date_serial <- suppressWarnings(as.numeric(na_text(date_serial)))
  time_fraction <- suppressWarnings(as.numeric(na_text(time_fraction)))
  out <- as.POSIXct(rep(NA_character_, length(date_serial)), tz = "UTC")
  has_time <- !is.na(date_serial) & !is.na(time_fraction)
  out[has_time] <- as.POSIXct((date_serial[has_time] + time_fraction[has_time]) * 86400,
    origin = "1899-12-30", tz = "UTC"
  )
  list(
    datetime = out,
    date_only = as.Date(ifelse(!is.na(date_serial), date_serial, NA), origin = "1899-12-30")
  )
}

# "160 P" -> bearing 160, port/starboard "P"; "180" -> bearing 180, no side.
parse_bearing_ps <- function(x) {
  x <- na_text(x)
  m <- str_match(x, "^([0-9.]+)\\s*([A-Za-z]*)$")
  list(bearing = suppressWarnings(as.numeric(m[, 2])), ps = na_text(m[, 3]))
}

# "400" -> 400; "> 1K" -> 1000 (parse_number() would otherwise silently drop
# the K-for-thousand suffix and return 1).
parse_distance <- function(x) {
  x <- na_text(x)
  multiplier <- if_else(str_detect(str_to_upper(x), "K"), 1000, 1)
  suppressWarnings(as.numeric(str_extract(x, "[0-9.]+"))) * multiplier
}

canonical_species <- function(x) {
  x <- na_text(x)
  is_nbw <- str_detect(str_to_lower(coalesce(x, "")), "northern bottlenose|northen bottlenose|bottlenose whale|\\bnbw\\b")
  if_else(is_nbw, "Northern Bottlenose Whale", x)
}

collapse_comment <- function(...) {
  out <- paste(..., sep = "; ")
  out <- str_replace_all(out, "(^|; )NA(?=;|$)", "")
  str_replace_all(na_text(out), "(^; +|; +$)", "")
}

# ---- CETACEANS -> SIGHTINGS --------------------------------------------------

cetaceans <- read_excel(path, sheet = "Cetaceans", col_types = "text", .name_repair = "unique") %>%
  mutate(source_row = row_number() + 1L)

start_dt <- parse_serial_datetime(cetaceans$Date, cetaceans$Time_Start)
end_dt <- parse_serial_datetime(cetaceans$Date, cetaceans$Time_End)
bearing_ps <- parse_bearing_ps(cetaceans$BearingToAnimal)

sightings <- tibble(
  record_id = paste0(year, "_paper_sighting_", cetaceans$source_row - 1L),
  source = "Tarajoq",
  source_year = year,
  standard_datetime = start_dt$datetime,
  standard_date = coalesce(as.Date(start_dt$datetime, tz = "UTC"), start_dt$date_only),
  standard_time = if_else(!is.na(start_dt$datetime), format(start_dt$datetime, "%H:%M:%S", tz = "UTC"), NA_character_),
  datetime_precision = if_else(!is.na(start_dt$datetime), "minute_or_better", "date_only"),
  latitude = parse_dm(cetaceans$Start_Lat_deg, cetaceans$Start_lat_min),
  longitude = -parse_dm(cetaceans$Start_Long_deg, cetaceans$start_long_min),
  latitude_end = parse_dm(cetaceans$End_Lat_deg, cetaceans$end_lat_min),
  longitude_end = -parse_dm(cetaceans$End_Long_deg, cetaceans$end_long_min),
  time_end = if_else(!is.na(end_dt$datetime), format(end_dt$datetime, "%H:%M:%S", tz = "UTC"), NA_character_),
  species = canonical_species(cetaceans$Species),
  species_raw = na_text(cetaceans$Species),
  count_min = as_num(cetaceans[["<"]]),
  count_best = as_num(cetaceans[["~"]]),
  count_max = as_num(cetaceans[[">"]]),
  distance_m = parse_distance(cetaceans$Distance_from_Boat),
  bearing_deg = bearing_ps$bearing,
  port_starboard = bearing_ps$ps,
  behaviour = na_text(cetaceans$Behavior),
  photo_id = na_text(cetaceans$Pics),
  source_file = basename(path),
  source_sheet = "Cetaceans",
  source_row = cetaceans$source_row,
  coordinate_source = if_else(!is.na(latitude) & !is.na(longitude), "2017 paper form (transcribed)", NA_character_),
  coordinate_use = NA_character_,
  comments = collapse_comment(
    vessel_note,
    if_else(na_text(cetaceans$SPP_CER) == "3", "species certainty: maybe/unsure", NA_character_),
    if_else(!is.na(na_text(cetaceans$Obs)), paste0("observer: ", na_text(cetaceans$Obs)), NA_character_),
    na_text(cetaceans$Comments)
  )
)

out_dir <- file.path(out_root, "sightings")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, paste0("tarajoq_form_sightings_", year, "_standardized.csv"))
write_csv(sightings, out_file, na = "")
message(year, " sightings: ", nrow(sightings), " rows -> ", out_file)

# ---- DAILYEFFORT -> ENVIRONMENT ----------------------------------------------

effort <- read_excel(path, sheet = "DailyEffort", col_types = "text", .name_repair = "unique") %>%
  mutate(source_row = row_number() + 1L)

env_start_dt <- parse_serial_datetime(effort$Date, effort$TimeStart_UTC)
env_end_dt <- parse_serial_datetime(effort$Date, effort$TimeEnd_UTC)

environment <- tibble(
  record_id = paste0(year, "_paper_environment_", effort$source_row - 1L),
  source = "Tarajoq",
  source_year = year,
  standard_datetime = env_start_dt$datetime,
  standard_date = coalesce(as.Date(env_start_dt$datetime, tz = "UTC"), env_start_dt$date_only),
  standard_time = if_else(!is.na(env_start_dt$datetime), format(env_start_dt$datetime, "%H:%M:%S", tz = "UTC"), NA_character_),
  time_end = if_else(!is.na(env_end_dt$datetime), format(env_end_dt$datetime, "%H:%M:%S", tz = "UTC"), NA_character_),
  latitude = parse_dm(effort$Start_Lat_deg, effort$Start_lat_min),
  longitude = -parse_dm(effort$Start_Long_deg, effort$start_long_min),
  latitude_end = parse_dm(effort$End_Lat_deg, effort$end_lat_min),
  longitude_end = -parse_dm(effort$End_Long_deg, effort$end_long_min),
  station_name = na_text(effort$STN),
  fishing_state = na_text(effort$ShipState),
  visibility_nm = as_num(effort$Vis_nm),
  beaufort_sea_state = as_num(effort$Beaufort),
  source_file = basename(path),
  source_sheet = "DailyEffort",
  source_row = effort$source_row,
  comments = collapse_comment(
    vessel_note,
    if_else(!is.na(na_text(effort$Encounters)), paste0("linked encounters: ", na_text(effort$Encounters)), NA_character_),
    if_else(!is.na(na_text(effort$EndReason)), paste0("end reason: ", na_text(effort$EndReason)), NA_character_),
    na_text(effort$Comments)
  )
)

out_dir <- file.path(out_root, "environment")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, paste0("environment_", year, "_standardized.csv"))
write_csv(environment, out_file, na = "")
message(year, " environment: ", nrow(environment), " rows -> ", out_file)

# ---- BIOPSY -------------------------------------------------------------------

biopsy_raw <- read_excel(path, sheet = "Biopsy", col_types = "text", .name_repair = "unique") %>%
  mutate(source_row = row_number() + 1L)

biopsy_dt <- parse_serial_datetime(biopsy_raw$Date, biopsy_raw$Time_UTC)

biopsy <- tibble(
  record_id = paste0(year, "_paper_biopsy_", biopsy_raw$source_row - 1L),
  source = "Tarajoq",
  source_year = year,
  standard_datetime = biopsy_dt$datetime,
  standard_date = coalesce(as.Date(biopsy_dt$datetime, tz = "UTC"), biopsy_dt$date_only),
  standard_time = if_else(!is.na(biopsy_dt$datetime), format(biopsy_dt$datetime, "%H:%M:%S", tz = "UTC"), NA_character_),
  latitude = parse_dm(biopsy_raw$LAT_degrees, biopsy_raw$Lat_minutes),
  longitude = -parse_dm(biopsy_raw$LONG_degrees, biopsy_raw$Long_minutes),
  sighting_no = na_text(biopsy_raw$Encounter),
  outcome = if_else(na_text(biopsy_raw$Hit) == "Miss", "Miss", "Hit"),
  outcome_raw = na_text(biopsy_raw$Hit),
  reaction = na_text(biopsy_raw$Reaction),
  species = canonical_species(biopsy_raw$Species),
  side = na_text(biopsy_raw$Side),
  photo_id = na_text(biopsy_raw$Photo_Video_RefNumber),
  source_file = basename(path),
  source_sheet = "Biopsy",
  source_row = biopsy_raw$source_row,
  comments = collapse_comment(
    vessel_note,
    paste0("general location: ", na_text(biopsy_raw$General_Location)),
    paste0("reaction scale (1=none, 2=slight, 3=moderate, 4=major/breach): ", na_text(biopsy_raw$Reaction)),
    na_text(biopsy_raw$Comments)
  )
)

out_dir <- file.path(out_root, "biopsy")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, paste0("biopsy_", year, "_standardized.csv"))
write_csv(biopsy, out_file, na = "")
message(year, " biopsy: ", nrow(biopsy), " rows -> ", out_file)
