#This script imports xls/ csv/ gpx field data output files collected during fieldwork 
# from fieldDataEntry matlab program into one table, formats GPS, 
# deduplicates records and renames variables
#Laura Feyrer 2024
#update 2025

#LIBRARIES------------
pacman::p_load(data.table, dplyr, readxl, purrr, readr, tidyr, stringr, sf, ggplot2, lubridate)

here::here()

#PARAMETERS------------
original_path <- paste0(here::here(),"/input/orig_data/2026/forms_original/data")
output_folder = paste0(here::here(),"/output/")
year = 2026
Trip = "_ArcticLeg1"

# External GPS is optional. "auto" uses the Time0 CSV when available,
# otherwise GPX, and otherwise runs with no external track.
# Allowed values: "auto", "csv", "gpx", or "none".
GPS_SOURCE <- "auto"
GPS_CSV <- here::here("input", "data", as.character(year),
                      paste0("Time0_", year, ".csv"))
GPS_GPX_FOLDER <- here::here("input", "data", as.character(year), "GPS")
# Inclusive UTC dates for GPS backfill and cetacean sightings.
SURVEY_WINDOW <- if (year == 2026) {
  list(start = as.Date("2026-10-19"), end = as.Date("2026-11-03"))
} else {
  NULL
}

#HELPER FUNCTIONS------------

# Function to parse coordinate strings
parse_coordinates <- function(pos_string) {
  tibble(
    LatD = as.numeric(str_extract(pos_string, "^\\d+")),
    LatM = as.numeric(str_extract(pos_string, "(?<=^\\d{2}d)[\\d\\.]+")) / 60,
    Latitude = LatD + LatM,
    LongD = as.numeric(str_extract(pos_string, "(?<=\\s)\\d+(?=d)")),
    LongM = as.numeric(str_extract(pos_string, "(?<=\\s\\d{2}d)[\\d\\.]+")) / 60,
    Longitude = (LongD + LongM) * -1
  )
}

# Function to validate coordinates
validate_coordinates <- function(data) {
  invalid <- data %>%
    filter(
      is.na(Latitude) | is.na(Longitude) |
        Latitude < 0 | Latitude > 90 |
        Longitude > 0 | Longitude < -180
    )
  
  if (nrow(invalid) > 0) {
    warning(paste("Found", nrow(invalid), "records with invalid coordinates"))
    print(invalid %>% select(DateT, StartPos, Latitude, Longitude))
  }
  
  return(data)
}

# Function to backfill coordinates from ship track
backfill_coordinates <- function(data, ship_data, time_col) {
  data %>%
    mutate(timestamp = floor_date({{time_col}}, unit = "minute")) %>%
    left_join(
      ship_data %>% select(timestamp, ship_lat = Latitude, ship_lon = Longitude),
      by = "timestamp"
    ) %>%
    mutate(
      Latitude = coalesce(Latitude, ship_lat),
      Longitude = coalesce(Longitude, ship_lon)
    ) %>%
    select(-ship_lat, -ship_lon)
}

# Function to write outputs
write_outputs <- function(data, sheet_name, trip, year, csv_dir = NULL) {
  output_dir <- here::here("output", sheet_name)
  output_name <- paste0(sheet_name, trip, year)
  if (is.null(csv_dir)) csv_dir <- output_dir
  csv_path <- file.path(csv_dir, paste0(output_name, ".csv"))
  rds_path <- file.path(output_dir, paste0(output_name, ".rds"))

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(data, csv_path)
  write_rds(data, rds_path)

  message("Saved ", csv_path, " and ", rds_path)
}

# Function to process each xls data sheet file in the folder
process_folder <- function(folder_path, sheet, output_folder) {
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE, recursive = T)
  
  for (f in files) {
    try({
      df <- read_xlsx(f, sheet = sheet)
      csv_file <- gsub("xlsx$", "csv", f)
      write.csv(df, csv_file, row.names = FALSE)
      destination = file.path(output_folder, basename(csv_file))
      file.copy(csv_file, destination, overwrite = T )
      file.remove(csv_file)
    }, silent = TRUE)
  }
  
  cat("Processing and moving CSV files complete.\n")
}

# Simple helper - handles date parsing
parse_date <- function(x, ref_date = NULL) {
  x <- as.character(x)  # Ensure it's character
  x <- sub("^E/?", "", x)  # Remove E or E/ prefix
  x <- trimws(x)
  
  result <- case_when(
    # Handle NA
    is.na(x) | x == "" ~ as.POSIXct(NA),
    # DD/MM/YY HH:MM format (contains "/")
    grepl("/", x) ~ {
      clean_x <- sub("^((?:\\S+\\s+\\S+)).*", "\\1", x)
      dmy_hms(clean_x, tz = "UTC", truncated = 2, quiet = TRUE)
    },
    # ISO timestamps from CSV or Excel datetime cells
    grepl("^\\d{4}-\\d{2}-\\d{2}", x) ~
      ymd_hms(x, tz = "UTC", truncated = 3, quiet = TRUE),
    # Excel serial date (pure numeric, possibly with decimal)
    grepl("^[0-9]+(\\.[0-9]+)?$", x) ~ {
      as.POSIXct(suppressWarnings(as.numeric(x)) * 86400, origin = "1899-12-30", tz = "UTC")
    },
    # Default
    TRUE ~ as.POSIXct(NA)
  )
  
  return(result)
}

#TRACK-------
# Function to read and process a single GPX file
process_gpx <- function(gpx_file) {
  tryCatch({
    track <- st_read(gpx_file, layer = "track_points", quiet = TRUE)
    
    track_clean <- track %>%
      mutate(
        Longitude = st_coordinates(.)[,1],
        Latitude = st_coordinates(.)[,2],
        DateT = as.POSIXct(time, tz = "UTC"),
        Year = lubridate::year(DateT),
        Date = format(DateT, "%d/%m/%Y"),
        Time = format(DateT, "%H:%M:%S"),
        UTC1 = format(DateT, "%Y-%m-%d %H:%M"),
        timestamp = floor_date(DateT, unit = "minute")
      ) %>%
      filter(!is.na(Latitude), !is.na(Longitude)) %>%
      dplyr::select(Latitude, Longitude, Year, Date, Time, DateT, UTC1, timestamp)
    
    return(track_clean)
  }, error = function(e) {
    warning(paste("Failed to process", basename(gpx_file), ":", e$message))
    return(NULL)
  })
}

# Function to parse DMS coordinates from the Time0 CSV format.
parse_dms <- function(x) {
  deg  <- as.numeric(str_extract(x, "^\\d+"))
  mins <- as.numeric(str_extract(x, "(?<=-)[0-9.]+(?=-[NSEW])")) / 60
  hemi <- str_extract(x, "[NSEW]$")
  dd <- deg + mins
  ifelse(hemi %in% c("S", "W"), -dd, dd)
}

# Function to read the 2024 Time0 CSV, including trailing fields that are
# present in the data rows but not listed in the CSV header.
process_time0_csv <- function(csv_file) {
  raw_lines <- readLines(csv_file)
  header_line <- which(str_detect(raw_lines, regex("latitude", ignore_case = TRUE)))[1]
  if (is.na(header_line)) stop("No Latitude header found in: ", csv_file)

  data_start <- which(str_detect(raw_lines, "^\\s*\\d+[-,]"))[1]
  if (is.na(data_start)) stop("No Time0 data rows found in: ", csv_file)

  header_names <- str_split(raw_lines[header_line], ",")[[1]] %>%
    str_trim() %>%
    make.names(unique = TRUE)
  n_data_cols <- length(str_split(raw_lines[data_start], ",")[[1]])
  if (n_data_cols > length(header_names)) {
    header_names <- c(header_names,
                      paste0("X", seq_len(n_data_cols - length(header_names))))
  }

  raw <- read.csv(csv_file, skip = data_start - 1, header = FALSE,
                  col.names = header_names, fill = TRUE,
                  stringsAsFactors = FALSE)

  raw %>%
    mutate(
      Latitude = parse_dms(Latitude),
      Longitude = parse_dms(Longitude),
      across(c(Year, Month, Day, Hour, Minute, Second), as.numeric),
      DateT = as.POSIXct(
        sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                Year, Month, Day, Hour, Minute, Second),
        format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
      ),
      Date = format(DateT, "%d/%m/%Y"),
      Time = format(DateT, "%H:%M:%S"),
      UTC1 = format(DateT, "%Y-%m-%d %H:%M"),
      timestamp = floor_date(DateT, unit = "minute")
    ) %>%
    filter(!is.na(DateT), !is.na(Latitude), !is.na(Longitude)) %>%
    select(Latitude, Longitude, Year, Date, Time, DateT, UTC1, timestamp)
}

# Select an external GPS source. The datasheet coordinates remain the
# primary source; this track is only used by backfill_coordinates().
gpx_files <- if (dir.exists(GPS_GPX_FOLDER)) {
  list.files(GPS_GPX_FOLDER, pattern = "\\.gpx$", full.names = TRUE,
             ignore.case = TRUE)
} else {
  character(0)
}

gps_source <- GPS_SOURCE
if (gps_source == "auto") {
  gps_source <- if (file.exists(GPS_CSV)) "csv" else if (length(gpx_files) > 0) "gpx" else "none"
}
if (!gps_source %in% c("csv", "gpx", "none")) {
  stop("GPS_SOURCE must be one of: auto, csv, gpx, none")
}

if (gps_source == "csv" && file.exists(GPS_CSV)) {
  message("Using Time0 CSV for optional GPS backfill: ", GPS_CSV)
  ship <- process_time0_csv(GPS_CSV)
} else if (gps_source == "gpx" && length(gpx_files) > 0) {
  message("Using GPX files for optional GPS backfill: ", length(gpx_files), " file(s)")
  ship <- gpx_files %>% lapply(process_gpx) %>% bind_rows()
} else {
  if (gps_source != "none") {
    warning("Requested GPS source is unavailable; continuing without external GPS track.")
  } else {
    message("External GPS disabled; using coordinates present in the datasheets.")
  }
  ship <- tibble(
    Latitude = numeric(), Longitude = numeric(), Year = numeric(),
    Date = character(), Time = character(), DateT = as.POSIXct(character()),
    UTC1 = character(), timestamp = as.POSIXct(character())
  )
}

if (!is.null(SURVEY_WINDOW)) {
  ship <- ship %>%
    filter(between(as.Date(DateT, tz = "UTC"), SURVEY_WINDOW$start, SURVEY_WINDOW$end))
}

ship <- ship %>%
  arrange(DateT) %>%
  distinct(UTC1, .keep_all = TRUE)

# Create spatial objects only when an external GPS track is available.
if (nrow(ship) > 0) {
  ship_sf <- st_as_sf(ship, coords = c("Longitude", "Latitude"), crs = 4326) %>%
    mutate(LINE_ID = rep(1:ceiling(n()/2), each = 2, length.out = n()))

  track <- ship_sf %>%
    arrange(DateT) %>%
    st_combine() %>%
    st_cast("LINESTRING")

  print(paste("Total points:", nrow(ship)))
  print(paste("GPS coverage (UTC):", min(ship$DateT), "to", max(ship$DateT)))
  plot(st_geometry(track))
} else {
  ship_sf <- NULL
  track <- NULL
  message("No external GPS track loaded; skipping GPS map and shapefile export.")
}

# Save cleaned data
dir.create(paste0(output_folder,"Track/"), recursive = TRUE, showWarnings = FALSE)
write.csv(ship,"output/Track/ship_track_clean.csv", row.names = FALSE)
if (!is.null(ship_sf)) {
  st_write(ship_sf, "output/Track/ship_track.shp", delete_dsn = TRUE, quiet = T)
}

#CETACEANS------------
sheet = "Cetaceans"
message("📊 Processing Cetaceans data...")

workbooks <- list.files(original_path, pattern = "[.]xlsx$", full.names = TRUE,
                        recursive = TRUE, ignore.case = TRUE)
workbooks <- workbooks[!startsWith(basename(workbooks), "~$")]
cetacean_workbooks <- keep(workbooks, ~ sheet %in% excel_sheets(.x))
if (length(cetacean_workbooks) == 0) {
  stop("No workbooks with a Cetaceans sheet found in ", original_path)
}

all_data <- map_dfr(cetacean_workbooks, ~ read_xlsx(.x, sheet = sheet, col_types = "text")) %>%
  filter(if_any(everything(), ~ !is.na(.x))) %>%
  mutate(Enc_start = parse_date(DateT)) %>%
  filter(!is.na(Enc_start))

if (!is.null(SURVEY_WINDOW)) {
  all_data <- all_data %>%
    filter(between(as.Date(Enc_start, tz = "UTC"), SURVEY_WINDOW$start, SURVEY_WINDOW$end))
}

if (year == 2025) {
  all_data <- all_data %>%
    mutate(StartPos = if_else(StartPos == "d  d", "66d50.77 59d11.03", StartPos))
}

all_data <- all_data %>%
  bind_cols(parse_coordinates(.$StartPos)) %>%
  mutate(
    Port_Star = PS,
    Species = case_when(
      tolower(Species) %in% c("nbw", "northen bottlenose") ~ "Northern Bottlenose",
      TRUE ~ Species
    )
  ) %>%
  distinct(Enc_start, .keep_all = TRUE) %>%
  arrange(Enc_start) %>%
  validate_coordinates()

message(paste("Final records after processing:", nrow(all_data)))


Sharedata = all_data %>%
  mutate(DateT = Enc_start) %>%
  dplyr::select(DateT, Species, Latitude, Longitude, Min, Best, Max, Dist, Bearing,
                Port_Star, Behaviour, TimeEnd = `END encounter`, Pic_no, Comments, Enc_start)



Whales2025 <- Sharedata %>%
  mutate(
    Enc_end = parse_date(TimeEnd),
    Enc_time = as.numeric(difftime(Enc_end, Enc_start, units = "mins"))
  ) %>%
  backfill_coordinates(ship, Enc_start)

write_outputs(Whales2025, sheet, Trip, year,
              csv_dir = here::here("input", "sightings", "derived"))
message(paste("✓ Processed", nrow(Whales2025), "cetacean records"))

#ENVIRONMENT-----
sheet = "Environment"
message("📊 Processing Environment data...")

process_folder(original_path, sheet, paste0(here::here(),"/input/orig_data/2026/form_exports/Environment"))

all_data <- list.files(
  path = paste0(here::here(),"/input/orig_data/2026/form_exports/Environment"),
  pattern = "*.csv",
  full.names = TRUE
) %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = col_character())))

# Separate records with and without coordinates
has_coords <- all_data %>%
  filter(!is.na(StartPos), StartPos != "", !str_detect(StartPos, "^d\\s+d$"))

no_coords <- all_data %>%
  filter(is.na(StartPos) | StartPos == "" | str_detect(StartPos, "^d\\s+d$"))

message(paste("Records with coordinates:", nrow(has_coords)))
message(paste("Records without coordinates (will use ship track):", nrow(no_coords)))

# Parse coordinates only for records that have them
has_coords <- has_coords %>%
  bind_cols(parse_coordinates(.$StartPos))

# Add empty coordinate columns to no_coords
no_coords <- no_coords %>%
  mutate(Latitude = NA_real_, Longitude = NA_real_)

# Combine back together
all_data <- bind_rows(has_coords, no_coords) %>%
  distinct(DateT, .keep_all = TRUE) %>%
  select(-StartPos)  # Remove StartPos since we've processed it

EnvData <- all_data %>%
  mutate(
    Env_start = parse_date(DateT),
    Env_end = parse_date(TimeEnd),
    Env_start = if_else(is.na(Env_start), Env_end, Env_start),
    Env_time = as.numeric(difftime(Env_end, Env_start, units = "mins"))
  ) %>%
  backfill_coordinates(ship, Env_start)

write_outputs(EnvData, sheet, Trip, year)
message(paste("✓ Processed", nrow(EnvData), "environment records"))

#BIOPSY-----
sheet = "Biopsy"
message("📊 Processing Biopsy data...")

process_folder(original_path, sheet, paste0(here::here(),"/input/orig_data/2026/form_exports/Biopsy"))

all_data <- list.files(
  path = paste0(here::here(),"/input/orig_data/2026/form_exports/Biopsy"),
  pattern = "*.csv",
  full.names = TRUE
) %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = col_character()))) 

message(paste("Total biopsy records loaded:", nrow(all_data)))

# Older biopsy exports use different names. Normalize those names so the
# duplicate check and final output work for both older and newer sheets.
if (!"Subsample_B_no" %in% names(all_data) && "Sample_no" %in% names(all_data)) {
  all_data <- all_data %>% rename(Subsample_B_no = Sample_no)
}
if (!"Biopsy_outcome" %in% names(all_data) && "Sample_success" %in% names(all_data)) {
  all_data <- all_data %>%
    mutate(Biopsy_outcome = if_else(
      str_detect(str_to_lower(coalesce(Sample_success, "")), "hit|tissue"),
      "Hit", "Miss"
    ))
}
if (!"Subsample_A_no" %in% names(all_data)) {
  all_data$Subsample_A_no <- NA_character_
}
if (!"Comments" %in% names(all_data)) {
  all_data$Comments <- NA_character_
}

# Parse coordinates if they exist
all_data <- all_data %>%
  mutate(
    has_coords = !is.na(StartPos) & StartPos != "" & !str_detect(StartPos, "^d\\s+d$")
  )

# Separate and process records with coordinates
if (sum(all_data$has_coords, na.rm = TRUE) > 0) {
  with_coords <- all_data %>%
    filter(has_coords) %>%
    bind_cols(parse_coordinates(.$StartPos)) %>%
    select(-has_coords, -LatD, -LatM, -LongD, -LongM)
  
  without_coords <- all_data %>%
    filter(!has_coords) %>%
    mutate(Latitude = NA_real_, Longitude = NA_real_) %>%
    select(-has_coords)
  
  all_data <- bind_rows(with_coords, without_coords)
} else {
  all_data <- all_data %>%
    mutate(Latitude = NA_real_, Longitude = NA_real_) %>%
    select(-has_coords)
}

# Enhanced parse_date_biopsy function to handle the "H" and "M" prefixes and force 2025
parse_date_biopsy <- function(x) {
  x <- as.character(x)
  x <- sub("^[HME]/?", "", x)  # Remove H, M, or E prefix
  x <- trimws(x)
  x <- sub("^((?:\\S+\\s+\\S+)).*", "\\1", x)  # Keep first two tokens
  
  result <- case_when(
    is.na(x) | x == "" ~ as.POSIXct(NA),
    # DD/MM/YY HH:MM format
    grepl("/", x) ~ {
      parsed <- dmy_hms(x, tz = "UTC", truncated = 2)
      # Force year to 2025 using if_else (vectorized)
      },
    # Excel serial date
    grepl("^[0-9]+(\\.[0-9]+)?$", x) ~ as.POSIXct(as.numeric(x) * 86400, origin = "1899-12-30", tz = "UTC"),
    TRUE ~ as.POSIXct(NA)
  )
  
  return(result)
}
# Check for duplicates based on Subsample_B_no
message("Checking for duplicates based on Subsample_B_no...")
duplicates <- all_data %>%
  filter(!is.na(Subsample_B_no), Subsample_B_no != "") %>%
  group_by(Subsample_B_no) %>%
  filter(n() > 1) %>%
  arrange(Subsample_B_no)

if (nrow(duplicates) > 0) {
  message(paste("⚠ WARNING: Found", nrow(duplicates), "duplicate records with same Subsample_B_no"))
  message("These need manual curation:")
  print(duplicates %>% select(Subsample_B_no, DateT, Sighting_no, Species, Location, Biopsy_outcome, Reliable))
  
  # Save duplicates for manual review
  dup_file <- here::here("output", "Biopsy", paste0("DUPLICATES_REVIEW_", Trip, year, ".csv"))
  dir.create(dirname(dup_file), recursive = TRUE, showWarnings = FALSE)
  write_csv(duplicates, dup_file)
  message(paste("📝 Duplicates saved to:", dup_file))
}

# Deduplicate: keep first occurrence of each Subsample_B_no
all_data <- all_data %>%
  group_by(Subsample_B_no) %>%
  slice(1) %>%
  ungroup()

message(paste("Records after deduplication:", nrow(all_data)))

# Clean and format biopsy data
BiopsyData <- all_data %>%
  filter(Biopsy_outcome == "Hit") %>%  # Only keep successful biopsies
  mutate(
    # Parse dates with enhanced function
    Biopsy_start = parse_date_biopsy(DateT),
    Biopsy_end = parse_date_biopsy(Biopsy_Time),
    
    # Use Biopsy_start if Biopsy_end is NA
    Biopsy_start = if_else(is.na(Biopsy_start), Biopsy_end, Biopsy_start),
    
    # Create timestamp for joining with ship track
    timestamp = floor_date(Biopsy_start, unit = "minute"),
    
    # Clean species names
    Species = case_when(
      tolower(Species) %in% c("nbw", "northen bottlenose") ~ "Northern Bottlenose",
      TRUE ~ Species
    ),
    
    # Ensure numeric fields are numeric
    Sighting_no = as.numeric(Sighting_no)
  ) %>%
  # Backfill coordinates from ship track if missing
  backfill_coordinates(ship, Biopsy_start) %>%
  # Select final columns
  select(
    Biopsy_start,
    Sighting_no,
    Latitude,
    Longitude,
    Biopsy_outcome,
    Reaction,
    Side,
    Reliable,
    Subsample_A_no,
    Pic_no,
    Comments
  ) %>%
  arrange(Biopsy_start)

# Write outputs
write_outputs(BiopsyData, sheet, Trip, year)
message(paste("✓ Processed", nrow(BiopsyData), "biopsy records"))
