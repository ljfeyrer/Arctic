# Match cetacean sighting stations to the Tarajoq station-position lookup.
# The 2025 match feeds the sightings compiler. Other tables document the matching.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(here)
})

station_path <- here::here("input", "orig_data", "2024", "station_ids",
                           "StationNames_Tarajoq_2024_2025.csv")
trawl_path <- here::here("input", "orig_data", "2025", "station_ids",
                         "TA2025TrawlTable.xlsx")
leg2_2024_path <- here::here("input", "orig_data", "2024", "sightings_original",
                             "Cetaceans_ArcticLeg2_2024.xlsx")
leg2_2025_path <- here::here("input", "orig_data", "2025", "sightings_original",
                             "Cetaceans_ArcticLeg2_2025.xlsx")
output_dir <- here::here("output", "StationMatches")
derived_dir <- here::here("input", "orig_data", "reference", "review",
                          "derived_station_matches")
matched_2025_path <- file.path(derived_dir, "Cetaceans_ArcticLeg2_2025_station_matched.csv")
matched_2024_path <- file.path(derived_dir, "Cetaceans_ArcticLeg2_2024_station_matched.csv")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

station_lookup_raw <- read_csv(station_path, show_col_types = FALSE)
station_lookup <- station_lookup_raw %>%
  transmute(
    lookup_year = as.integer(Year),
    set_number = as.integer(`Set Number`),
    fix_pos = `Fix Pos`,
    trawl_id = as.character(Trawl_ID),
    stratum = Stratum,
    study_area = `Study Area`,
    station_start_date = as.Date(`Start Date`, format = "%m/%d/%Y"),
    station_start_time = as.character(`Start Time`),
    station_end_date = if ("End Date" %in% names(station_lookup_raw))
      as.Date(`End Date`, format = "%m/%d/%Y") else as.Date(rep(NA_character_, n())),
    station_end_time = if ("End Time" %in% names(station_lookup_raw))
      as.character(`End Time`) else rep(NA_character_, n()),
    station_latitude = Latitude,
    station_longitude = Longitude
  ) %>%
  mutate(
    station_start_datetime = as.POSIXct(
      paste(station_start_date, station_start_time),
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    ),
    station_end_datetime = as.POSIXct(
      paste(station_end_date, station_end_time),
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    )
  )

deg_min <- function(degrees, minutes, longitude = FALSE) {
  value <- degrees + minutes / 60
  if (longitude) -value else value
}

trawl_lookup <- read_excel(trawl_path, sheet = "Sheet1") %>%
  transmute(
    trawl_id = as.character(Trawl_ID),
    fix_pos = FixPos,
    trawl_start_date = as.Date(`Start Date`),
    trawl_end_date = as.Date(`End Date`),
    trawl_start_time = format(`Start Time`, "%H:%M:%S"),
    trawl_end_time = format(`End Time`, "%H:%M:%S"),
    trawl_start_latitude = deg_min(`Start Lat Degree`, `Start Lat Minutes`),
    trawl_start_longitude = deg_min(`Start Long Degree`, `Start Long Minutes`, longitude = TRUE),
    trawl_end_latitude = deg_min(`End Lat Degree`, `End Lat Minutes`),
    trawl_end_longitude = deg_min(`End Long Degree`, `End Long Minutes`, longitude = TRUE)
  ) %>%
  mutate(
    trawl_start_datetime = as.POSIXct(paste(trawl_start_date, trawl_start_time),
                                      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    trawl_end_datetime = as.POSIXct(paste(trawl_end_date, trawl_end_time),
                                    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    trawl_midpoint_latitude = (trawl_start_latitude + trawl_end_latitude) / 2,
    trawl_midpoint_longitude = (trawl_start_longitude + trawl_end_longitude) / 2
  )

# 2025 uses numeric Set values that correspond directly to Set Number.
leg2_2025 <- read_excel(leg2_2025_path, sheet = "Leg 2 Sightings") %>%
  mutate(
    survey_year = 2025L,
    station_name = paste0("Set ", Set),
    set_number = as.integer(Set)
  ) %>%
  left_join(mutate(station_lookup, station_year = lookup_year),
            by = c("survey_year" = "lookup_year", "set_number")) %>%
  rename(lookup_year = station_year) %>%
  left_join(trawl_lookup, by = c("trawl_id", "fix_pos")) %>%
  mutate(
    match_status = case_when(
      !is.na(fix_pos) & !is.na(station_latitude) & !is.na(station_longitude) ~ "matched_with_coordinates",
      !is.na(fix_pos) ~ "matched_station_no_coordinates",
      TRUE ~ "unmatched"
    ),
    latitude = station_latitude,
    longitude = station_longitude,
    standard_datetime = coalesce(trawl_end_datetime, station_end_datetime,
                                 trawl_start_datetime, as.POSIXct(
                                   paste(station_start_date, station_start_time),
                                   format = "%Y-%m-%d %H:%M:%S", tz = "UTC")),
    datetime_basis = case_when(
      !is.na(trawl_end_datetime) ~ "trawl/station end time",
      !is.na(station_end_date) & !is.na(station_end_time) ~ "station end time",
      TRUE ~ "station start time (end unavailable)"
    ),
    standard_date = as.Date(standard_datetime),
    standard_time = format(standard_datetime, "%H:%M:%S", tz = "UTC"),
    coordinate_source = if_else(!is.na(latitude) & !is.na(longitude), "StationNames lookup", NA_character_),
    source_workbook = "Cetaceans_ArcticLeg2_2025.xlsx"
  )

# 2024 uses labels such as ST. 1. Match the extracted station number within
# survey year so 2024 and 2025 Set 1 values cannot be confused.
leg2_2024_raw <- read_excel(
  leg2_2024_path,
  sheet = "nbw_observation_TA_07_2024",
  skip = 1,
  .name_repair = "unique"
)

leg2_2024 <- leg2_2024_raw %>%
  rename(station_name = 1) %>%
  mutate(
    survey_year = 2024L,
    set_number = suppressWarnings(as.integer(str_extract(station_name, "[0-9]+"))),
    source_workbook = "Cetaceans_ArcticLeg2_2024.xlsx"
  ) %>%
  left_join(station_lookup, by = c("survey_year" = "lookup_year", "set_number")) %>%
  mutate(
    match_status = case_when(
      !is.na(fix_pos) & !is.na(station_latitude) & !is.na(station_longitude) ~ "matched_with_coordinates",
      !is.na(fix_pos) ~ "matched_station_no_coordinates",
      TRUE ~ "unmatched"
    ),
    latitude = station_latitude,
    longitude = station_longitude,
    standard_datetime = as.POSIXct(
      paste(station_end_date, station_end_time),
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    ),
    standard_datetime = coalesce(standard_datetime, as.POSIXct(
      paste(station_start_date, station_start_time),
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    )),
    datetime_basis = if_else(!is.na(station_end_date) & !is.na(station_end_time),
                             "station end time", "station start time (end unavailable)"),
    standard_date = as.Date(standard_datetime),
    standard_time = format(standard_datetime, "%H:%M:%S", tz = "UTC"),
    coordinate_source = if_else(!is.na(latitude) & !is.na(longitude), "StationNames lookup", NA_character_)
  )

matched_2025 <- leg2_2025 %>%
  select(survey_year, station_name, set_number, fix_pos, trawl_id, stratum,
         study_area, match_status, source_workbook, everything())

matched_2024 <- leg2_2024 %>%
  select(survey_year, station_name, set_number, fix_pos, trawl_id, stratum,
         study_area, match_status, source_workbook, everything())

combined <- bind_rows(matched_2024, matched_2025)

write_csv(matched_2025, matched_2025_path, na = "")
write_csv(matched_2024, matched_2024_path, na = "")
file.copy(matched_2024_path, file.path(output_dir, "Cetaceans_ArcticLeg2_2024_station_matched.csv"), overwrite = TRUE)
write_csv(combined, file.path(output_dir, "Cetaceans_ArcticLeg2_station_matched.csv"), na = "")

audit <- combined %>%
  count(source_workbook, match_status, name = "records") %>%
  arrange(source_workbook, match_status)
write_csv(audit, file.path(output_dir, "Cetaceans_ArcticLeg2_station_match_audit.csv"), na = "")

cat("Station matching summary:\n")
print(audit)
cat("Matched 2025 rows written to: ", matched_2025_path, "\n", sep = "")
cat("2024 and 2025 station names were matched within the year-specific station lookup.\n")
