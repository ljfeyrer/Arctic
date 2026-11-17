# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Confirm the nine later-November records in the 2023 Cetaceans workbook
# against the 2023 Leg 2 photo-ID export. Matching uses calendar date first;
# coordinates are reported when the photo export contains them.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(lubridate)
  library(stringr)
})

source("scripts/lib/helpers.R")

source_path <- "input/orig_data/2023/sightings_original/Cetaceans_Arctic2023.xlsx"
photo_path <- "input/orig_data/2023/LV/List View Leg2_2023.csv"
out_path <- "output/Sightings_QA/2023_leg2_cetaceans_vs_photo_id.csv"

source <- read_excel(source_path, col_types = "text") %>%
  mutate(
    source_row = row_number() + 1L,
    source_datetime = as.POSIXct(as.numeric(Local_Time) * 86400,
      origin = "1899-12-30", tz = "UTC"
    ),
    source_date = as.Date(source_datetime),
    source_latitude = as.numeric(Latitude),
    source_longitude = as.numeric(Longitude)
  ) %>%
  filter(source_date >= as.Date("2023-11-10"))

photo <- read_csv(photo_path, show_col_types = FALSE) %>%
  mutate(
    photo_datetime = parse_date_time(`Date Created`,
      orders = c("ymd HMS z", "ymd HMS"),
      tz = "UTC", quiet = TRUE
    ),
    photo_date = as.Date(photo_datetime),
    photo_latitude = suppressWarnings(as.numeric(Latitude)),
    photo_longitude = suppressWarnings(as.numeric(Longitude))
  )

review <- bind_rows(lapply(seq_len(nrow(source)), function(i) {
  same_date <- photo %>% filter(photo_date == source$source_date[i])
  if (nrow(same_date) == 0) {
    return(tibble(
      source_row = source$source_row[i], source_date = source$source_date[i],
      source_datetime = source$source_datetime[i], source_latitude = source$source_latitude[i],
      source_longitude = source$source_longitude[i], same_date_photo_rows = 0L,
      nearest_photo_distance_m = NA_real_, nearest_photo_file = NA_character_,
      photo_date_match = FALSE, coordinate_match_within_100m = FALSE
    ))
  }
  valid_coord <- !is.na(same_date$photo_latitude) & !is.na(same_date$photo_longitude) &
    !is.na(source$source_latitude[i]) & !is.na(source$source_longitude[i])
  distances <- rep(NA_real_, nrow(same_date))
  distances[valid_coord] <- haversine_m(
    source$source_latitude[i], source$source_longitude[i],
    same_date$photo_latitude[valid_coord], same_date$photo_longitude[valid_coord]
  )
  best <- if (all(is.na(distances))) NA_integer_ else which.min(ifelse(is.na(distances), Inf, distances))
  tibble(
    source_row = source$source_row[i], source_date = source$source_date[i],
    source_datetime = source$source_datetime[i], source_latitude = source$source_latitude[i],
    source_longitude = source$source_longitude[i], same_date_photo_rows = nrow(same_date),
    nearest_photo_distance_m = if (is.na(best)) NA_real_ else distances[best],
    nearest_photo_file = if (is.na(best)) NA_character_ else same_date$`File name`[best],
    photo_date_match = TRUE,
    coordinate_match_within_100m = !is.na(best) && distances[best] <= 100
  )
}))

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(review, out_path, na = "")
message("Wrote ", out_path)
print(review, n = Inf)
