# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# Collate NBW photo-ID records into encounter windows.
# Photos remain traceable in the photo-level output; no raw files are changed.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(readr)
  library(stringr)
})

source("scripts/lib/helpers.R")

YEAR <- as.integer(Sys.getenv("PHOTO_YEAR", "2022"))
PHOTO_ID_FILE <- Sys.getenv(
  "PHOTO_ID_FILE",
  unset = file.path("input", "data", as.character(YEAR), paste0("LV_", YEAR, ".csv"))
)
OUTPUT_DIR <- file.path("output", "PhotoID", paste0("photo_id_", YEAR))
LEG2_SIGHTINGS_FILE <- file.path(
  "input", "sightings", "derived",
  paste0("CetaceanSightings_leg2_", YEAR, ".csv")
)
ENCOUNTER_GAP_MINUTES <- 30
LATE_NOVEMBER_START_DAY <- as.integer(Sys.getenv("LATE_NOVEMBER_START_DAY", "15"))
FILTER_TO_NBW_KEYWORD <- FALSE

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

photo_id <- read_csv(
  PHOTO_ID_FILE,
  show_col_types = FALSE,
  name_repair = "unique"
)

pick_column <- function(candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(photo_id)][1]
  if (is.na(hit) && required) stop("Missing photo-ID column; tried: ", paste(candidates, collapse = ", "))
  hit
}

filename_col <- pick_column(c("File name", "Filename"))
datetime_col <- pick_column(c("Date Created", "UTC1"))
keyword_col <- pick_column(c("Keyword-export"), required = FALSE)
latitude_col <- pick_column(c("Latitude", "GPS Latitude", "OrigCamLat"), required = FALSE)
longitude_col <- pick_column(c("Longitude", "GPS Longitude", "OrigCamLon"), required = FALSE)
path_col <- pick_column(c("Path"), required = FALSE)
has_col <- function(x) length(x) == 1 && !is.na(x)

photo_id <- photo_id %>%
  mutate(
    photo_filename = as.character(.data[[filename_col]]),
    photo_datetime_raw = .data[[datetime_col]],
    keyword_raw = if (has_col(keyword_col)) as.character(.data[[keyword_col]]) else NA_character_,
    latitude_raw = if (has_col(latitude_col)) as.character(.data[[latitude_col]]) else NA_character_,
    longitude_raw = if (has_col(longitude_col)) as.character(.data[[longitude_col]]) else NA_character_,
    photo_path = if (has_col(path_col)) as.character(.data[[path_col]]) else NA_character_
  )

parse_photo_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as_datetime(x, tz = "UTC"))
  }
  parse_date_time(
    as.character(x),
    orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM"),
    tz = "UTC",
    quiet = TRUE
  )
}

nbw_photos <- photo_id %>%
  mutate(
    photo_datetime = parse_photo_datetime(photo_datetime_raw),
    latitude_num = suppressWarnings(parse_number(latitude_raw)),
    longitude_num = suppressWarnings(parse_number(longitude_raw)),
    is_nbw = str_detect(str_to_lower(coalesce(keyword_raw, "")), "\\bnbw\\b")
  ) %>%
  filter(!FILTER_TO_NBW_KEYWORD | is_nbw) %>%
  arrange(photo_datetime, photo_filename) %>%
  mutate(
    previous_photo_datetime = lag(photo_datetime),
    gap_from_previous_minutes = as.numeric(difftime(
      photo_datetime, previous_photo_datetime,
      units = "mins"
    )),
    new_encounter = row_number() == 1 | gap_from_previous_minutes > ENCOUNTER_GAP_MINUTES,
    encounter_number = group_into_encounters(photo_datetime, gap_minutes = ENCOUNTER_GAP_MINUTES),
    encounter_id = paste0(YEAR, "_photo_encounter_", sprintf("%03d", encounter_number)),
    sighting_date = as.Date(photo_datetime, tz = "UTC"),
    late_november_candidate = month(sighting_date) == 11 &
      day(sighting_date) >= LATE_NOVEMBER_START_DAY
  )

if (any(is.na(nbw_photos$photo_datetime))) {
  warning(sum(is.na(nbw_photos$photo_datetime)), " NBW photo rows have no parseable timestamp and were excluded.")
  nbw_photos <- nbw_photos %>% filter(!is.na(photo_datetime))
}

photo_output <- nbw_photos %>%
  select(
    encounter_id, encounter_number, photo_datetime, sighting_date,
    gap_from_previous_minutes, new_encounter, late_november_candidate,
    is_nbw, photo_filename, keyword_raw, latitude_raw, longitude_raw,
    latitude_num, longitude_num, photo_path, Rating
  )

encounters <- nbw_photos %>%
  group_by(encounter_id, encounter_number) %>%
  summarise(
    encounter_date = as.Date(min(photo_datetime, na.rm = TRUE), tz = "UTC"),
    encounter_start = min(photo_datetime, na.rm = TRUE),
    encounter_end = max(photo_datetime, na.rm = TRUE),
    duration_minutes = as.numeric(difftime(encounter_end, encounter_start, units = "mins")),
    photo_count = n(),
    nbw_keyword_photos = sum(is_nbw),
    latitude = if (any(!is.na(latitude_num))) mean(latitude_num, na.rm = TRUE) else NA_real_,
    longitude = if (any(!is.na(longitude_num))) mean(longitude_num, na.rm = TRUE) else NA_real_,
    latitude_first = first(latitude_num[!is.na(latitude_num)], default = NA_real_),
    longitude_first = first(longitude_num[!is.na(longitude_num)], default = NA_real_),
    latitude_last = last(latitude_num[!is.na(latitude_num)], default = NA_real_),
    longitude_last = last(longitude_num[!is.na(longitude_num)], default = NA_real_),
    photos_without_coordinates = sum(is.na(latitude_num) | is.na(longitude_num)),
    late_november_candidate = any(late_november_candidate),
    first_photo = first(photo_filename),
    last_photo = last(photo_filename),
    source_path_example = first(photo_path),
    keyword_summary = paste(sort(unique(na.omit(keyword_raw))), collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(encounter_start)

date_summary <- encounters %>%
  group_by(encounter_date) %>%
  summarise(
    encounters = n(),
    photos = sum(photo_count),
    first_encounter = min(encounter_start),
    last_encounter = max(encounter_end),
    encounters_with_coordinates = sum(!is.na(latitude) & !is.na(longitude)),
    late_november_encounters = sum(late_november_candidate),
    .groups = "drop"
  ) %>%
  arrange(encounter_date)

# Export later-November photo encounters in the same broad shape accepted by
# compile_nbw_sightings.R. Count is one encounter record; photo_count is kept
# separately and must not be interpreted as the number of whales.
leg2_sightings <- encounters %>%
  filter(late_november_candidate) %>%
  transmute(
    DateT_UTC = format(encounter_start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    DateT_UTC_END = format(encounter_end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    Species = "Northern Bottlenose Whale",
    Latitude = latitude,
    Longitude = longitude,
    Count = 1,
    Photo_count = photo_count,
    Survey_leg = "Leg 2",
    Comments = paste0(
      "Photo-ID encounter grouped within ", ENCOUNTER_GAP_MINUTES,
      " minutes; photo count = ", photo_count,
      "; approximate coordinates are the mean of available photo coordinates."
    ),
    Source_photo = first_photo
  )

dir.create(dirname(LEG2_SIGHTINGS_FILE), recursive = TRUE, showWarnings = FALSE)
write_csv(leg2_sightings, LEG2_SIGHTINGS_FILE, na = "")

gap_report <- nbw_photos %>%
  filter(!is.na(gap_from_previous_minutes), gap_from_previous_minutes > ENCOUNTER_GAP_MINUTES) %>%
  transmute(
    previous_photo_datetime = previous_photo_datetime,
    current_photo_datetime = photo_datetime,
    gap_minutes = gap_from_previous_minutes,
    current_photo = photo_filename,
    current_photo_path = photo_path,
    current_latitude = latitude_num,
    current_longitude = longitude_num
  )

write_csv(photo_output, file.path(OUTPUT_DIR, paste0("nbw_photo_records_", YEAR, ".csv")), na = "")
write_csv(encounters, file.path(OUTPUT_DIR, paste0("nbw_photo_encounters_", YEAR, ".csv")), na = "")
write_csv(date_summary, file.path(OUTPUT_DIR, paste0("nbw_photo_encounter_date_summary_", YEAR, ".csv")), na = "")
write_csv(gap_report, file.path(OUTPUT_DIR, paste0("nbw_photo_encounter_gaps_", YEAR, ".csv")), na = "")

map_path <- file.path(OUTPUT_DIR, paste0("nbw_photo_encounters_map_", YEAR, ".png"))
map_data <- encounters %>% filter(!is.na(latitude), !is.na(longitude))
if (nrow(map_data) > 0) {
  encounter_map <- ggplot(
    map_data,
    aes(x = longitude, y = latitude, size = photo_count, color = late_november_candidate)
  ) +
    geom_point(alpha = 0.75) +
    coord_quickmap() +
    scale_color_manual(values = c(`FALSE` = "#0072B2", `TRUE` = "#D55E00")) +
    labs(
      title = paste("NBW photo-ID encounters", YEAR),
      subtitle = paste0("Photos grouped when consecutive timestamps are <= ", ENCOUNTER_GAP_MINUTES, " minutes"),
      x = "Longitude",
      y = "Latitude",
      size = "Photos",
      color = "Later November"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(map_path, encounter_map, width = 10, height = 8, units = "in", dpi = 300)
}

summary_lines <- c(
  paste("Photo-ID file:", PHOTO_ID_FILE),
  paste("Total photo rows:", nrow(photo_id)),
  paste("Photo rows included:", nrow(nbw_photos)),
  paste("Rows with NBW keyword:", sum(nbw_photos$is_nbw)),
  paste("Keyword filter enabled:", FILTER_TO_NBW_KEYWORD),
  paste("Encounter groups:", nrow(encounters)),
  paste("Encounter gap threshold (minutes):", ENCOUNTER_GAP_MINUTES),
  paste("Later-November cutoff used for review:", paste0(YEAR, "-11-", sprintf("%02d", LATE_NOVEMBER_START_DAY))),
  paste("Encounters with later-November flag:", sum(encounters$late_november_candidate)),
  paste("Encounter date range:", min(encounters$encounter_date), "to", max(encounters$encounter_date)),
  paste("Encounter groups with coordinates:", sum(!is.na(encounters$latitude) & !is.na(encounters$longitude))),
  paste("Gaps greater than threshold:", nrow(gap_report)),
  paste("Leg 2 sightings sheet:", LEG2_SIGHTINGS_FILE),
  paste("Leg 2 photo-derived sightings:", nrow(leg2_sightings)),
  "",
  "The later-November flag is a review aid, not a confirmed Leg 2 assignment."
)
writeLines(summary_lines, file.path(OUTPUT_DIR, paste0("nbw_photo_encounters_summary_", YEAR, ".txt")))

cat(paste(summary_lines, collapse = "\n"), "\n\nDate summary:\n")
print(date_summary)
print(leg2_sightings %>% select(DateT_UTC, Latitude, Longitude, Photo_count, Survey_leg))
