# ============================================================
# Laura Joan Feyrer
# Ship Track Import and Processing -- Arctic Fieldwork
# Last updated: 2025-04-22
#
# Purpose: Robustly imports ship track data from one or more
#          vessel CSV exports (Own Ship logger format) and/or
#          GPX files, with optional KML plotting. Accepts a
#          vector of file paths or a folder path for each format.
#          Parses DMS lat/lon, builds datetime, deduplicates
#          to one record per minute across all input files,
#          and produces sf point and line objects ready for
#          mapping or export.
#
# Changes from previous version:
#   - CSV_PATH replaced with CSV_PATHS: accepts a character
#     vector of file paths OR a single folder path. This
#     allows multiple per-year log files to be combined
#     without pre-merging them (e.g. 2022 has two CSVs).
#   - Single-file reading extracted into read_one_ship_csv()
#     so the loop over multiple files stays readable.
#   - Source filename tracked in a source_file column for
#     the audit trail -- preserved in the output CSV.
#   - Deduplication (one record per minute) is applied once
#     after all files are combined, so minute-boundary rows
#     at file edges are handled consistently.

# ============================================================


# ---- 0. LIBRARIES -------------------------------------------

library(dplyr)
library(stringr)
library(sf)
library(ggplot2)


# ---- 1. USER PARAMETERS  ------------------------------------
# Edit these for each season. Everything else is automatic.

YEAR <- 2026

# Inclusive UTC dates for the survey track. Set SURVEY_WINDOW to NULL to use all fixes.
SURVEY_WINDOW <- if (YEAR == 2026) {
  list(start = as.Date("2026-10-19"), end = as.Date("2026-11-03"))
} else {
  NULL
}

# Optional Time Zero CSV input. Set to character(0) to disable or use:
#   (a) a character vector of file paths, e.g.:
#       c("/path/to/file1.csv", "/path/to/file2.csv")
#   (b) a single folder path -- all .csv files in the folder
#       will be read and combined, e.g.:
#       "/path/to/2022_logs/"
#
# The 2022 season has two separate log files; list them both.
# For 2024 onward there is typically one file per season.
CSV_PATHS <- ("/Users/chirp/CODE/Arctic/input/orig_data/2026/gps/TimeZero")


# Optional GPX input: set to character(0) to disable, or provide a folder
# containing GPX files or a character vector of individual GPX paths.
GPX_PATHS <- '/Users/chirp/CODE/Arctic/input/orig_data/2026/gps/GPX'

# Output path for the cleaned CSV
CSV_OUT <- paste0('/Users/chirp/CODE/Arctic/output/Track/ship_track_clean_', YEAR, '.csv')

# Set to NULL if you have no KML for this year
KML_PATH <- NULL   # e.g. "input/original_data/Time0/Canada2025.kml"

# UTM zone for optional reprojection (20N = 32620 for eastern Arctic)
UTM_CRS  <- 32620

# Set to TRUE to write shapefiles at the end
WRITE_SHP <- FALSE
SHP_DIR   <- "shapes"


# ---- 2. HELPER: RESOLVE INPUT FILE LIST ---------------------
# Missing paths and empty folders are skipped for either input format.

resolve_track_paths <- function(paths, extension) {
  if (length(paths) == 0) return(character(0))
  format_name <- toupper(extension)
  if (length(paths) == 1 && dir.exists(paths)) {
    found <- list.files(paths, pattern = paste0("[.]", extension, "$"),
                        full.names = TRUE, ignore.case = TRUE)
    found <- found[!dir.exists(found)]
    message("Found ", length(found), " ", format_name, " file(s) in ", paths)
    return(found)
  }
  available <- file.exists(paths) & !dir.exists(paths)
  if (any(!available)) {
    message("Skipping unavailable ", format_name, " input(s):\n  ",
            paste(paths[!available], collapse = "\n  "))
  }
  paths[available]
}


# ---- 3. HELPER: PARSE DMS STRING TO DECIMAL DEGREES --------
# Handles formats: "69-58.5164-N", "066-00.1912-W"
# Works for any degree-digit count (2 or 3) and all four hemispheres.

parse_dms <- function(x) {
  deg  <- as.numeric(str_extract(x, "^\\d+"))
  mins <- as.numeric(str_extract(x, "(?<=-)[0-9.]+(?=-[NSEW])")) / 60
  hemi <- str_extract(x, "[NSEW]$")
  dd   <- deg + mins
  ifelse(hemi %in% c("S", "W"), -dd, dd)
}


# ---- 4. HELPER: READ ONE SHIP CSV ---------------------------
# Reads a single Own Ship logger CSV, handles variable column
# counts (trailing commas in 2022 files) and the optional
# "#Own Ship" comment line before the header, or the newer
# headerless "TK,1,2" export followed by a "#Date:" line.
# Returns a data frame with parsed lat/lon and datetime.
# The source_file column records the basename of the input
# file for the audit trail.

read_one_ship_csv <- function(path) {
  
  message("Reading: ", basename(path))
  raw_lines <- readLines(path)
  
  # locate the header row (first line containing "Latitude"). The newer
  # TK export has no field-name row, so header_line will be NA for it.
  header_line <- which(str_detect(raw_lines,
                                  regex("latitude", ignore_case = TRUE)))[1]
  
  # locate the first data row (first line that starts with a digit)
  data_start <- which(str_detect(raw_lines, "^\\s*[0-9]"))[1]
  if (is.na(data_start))  stop("No data rows found in: ", path)
  
  n_data_cols   <- length(str_split(raw_lines[data_start],  ",")[[1]])

  if (is.na(header_line)) {
    # TK exports use this fixed layout:
    # record, status, latitude, longitude, ..., year, month, day,
    # hour, minute, second, ...
    header_names <- c(
      "Record", "Status", "Latitude", "Longitude",
      "X1", "Depth", "X2", "X3", "Year", "Month", "Day",
      "Hour", "Minute", "Second", "X4", "X5", "X6"
    )
    if (n_data_cols > length(header_names)) {
      header_names <- c(header_names,
                        paste0("X", seq_len(n_data_cols - length(header_names))))
    }
    header_names <- header_names[seq_len(n_data_cols)]
    message("  Headerless TK export detected; using positional column names")
  } else {
    # count columns in the header and in the first data row
    n_header_cols <- length(str_split(raw_lines[header_line], ",")[[1]])

    # build column names; pad with X1, X2 ... if data has trailing commas
    header_names <- str_split(raw_lines[header_line], ",")[[1]] %>%
      str_trim() %>%
      make.names(unique = TRUE)

    if (n_data_cols > n_header_cols) {
      n_extra      <- n_data_cols - n_header_cols
      header_names <- c(header_names, paste0("X", seq_len(n_extra)))
      message("  ", n_extra, " padding column(s) added for trailing commas")
    }
  }
  
  # read the file, skipping everything above the first data row
  raw <- read.csv(
    path,
    skip             = data_start - 1,
    header           = FALSE,
    col.names        = header_names,
    fill             = TRUE,
    stringsAsFactors = FALSE
  )
  
  message("  ", nrow(raw), " raw rows imported")
  
  # parse coordinates and build datetime
  out <- raw %>%
    mutate(
      source_file = basename(path),
      LatitudeD   = parse_dms(Latitude),
      LongitudeD  = parse_dms(Longitude)
    ) %>%
    filter(!is.na(LatitudeD), !is.na(LongitudeD)) %>%
    mutate(
      across(c(Year, Month, Day, Hour, Minute, Second), as.numeric),
      DateT = as.POSIXct(
        sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                Year, Month, Day, Hour, Minute, Second),
        tz     = "UTC",
        format = "%Y-%m-%d %H:%M:%S"
      ),
      UTC1 = format(DateT, "%Y-%m-%d %H:%M")
    ) %>%
    # keep core columns plus any environmental columns that are present
    select(
      source_file,
      LatitudeD, LongitudeD,
      Year, Month, Day, Hour, Minute, Second,
      DateT, UTC1,
      any_of(c("Depth", "COG", "SOG", "Heading", "STW",
               "TrueWindSpeed", "AppWindSpeed", "AtmPressure",
               "AirTemp", "SST"))
    )
  
  message("  ", nrow(out), " rows after coordinate parse")
  out
}


# ---- 5. HELPER: READ ONE GPX TRACK ---------------------------
# Reads the standard GPX track_points layer and converts it to the
# same columns produced by read_one_ship_csv().

read_one_ship_gpx <- function(path) {
  message("Reading GPX: ", basename(path))

  gpx <- st_read(path, layer = "track_points", quiet = TRUE)
  if (nrow(gpx) == 0) {
    warning("No track points found in GPX: ", path)
    return(tibble())
  }

  coords <- st_coordinates(gpx)
  date_time <- as.POSIXct(gpx$time, tz = "UTC")

  out <- tibble(
    source_file = basename(path),
    LatitudeD   = coords[, "Y"],
    LongitudeD  = coords[, "X"],
    DateT       = date_time
  ) %>%
    filter(!is.na(DateT), !is.na(LatitudeD), !is.na(LongitudeD)) %>%
    mutate(
      Year   = as.numeric(format(DateT, "%Y")),
      Month  = as.numeric(format(DateT, "%m")),
      Day    = as.numeric(format(DateT, "%d")),
      Hour   = as.numeric(format(DateT, "%H")),
      Minute = as.numeric(format(DateT, "%M")),
      Second = as.numeric(format(DateT, "%S")),
      UTC1   = format(DateT, "%Y-%m-%d %H:%M")
    ) %>%
    select(source_file, LatitudeD, LongitudeD,
           Year, Month, Day, Hour, Minute, Second, DateT, UTC1)

  message("  ", nrow(out), " GPX track points imported")
  out
}


# ---- 5. READ AND COMBINE ALL INPUT FILES --------------------

csv_files <- resolve_track_paths(CSV_PATHS, "csv")
gpx_files <- resolve_track_paths(GPX_PATHS, "gpx")
file_list <- c(csv_files, gpx_files)
if (length(file_list) == 0) stop("No CSV or GPX input files found. Check CSV_PATHS and GPX_PATHS.")

# read each file into a list, then bind rows
ship_list <- c(
  lapply(csv_files, read_one_ship_csv),
  lapply(gpx_files, read_one_ship_gpx)
)
ship_raw  <- bind_rows(ship_list)
if (nrow(ship_raw) == 0) stop("The CSV and GPX inputs contained no usable track points.")

if (!is.null(SURVEY_WINDOW)) {
  ship_raw <- ship_raw %>%
    filter(between(as.Date(DateT, tz = "UTC"), SURVEY_WINDOW$start, SURVEY_WINDOW$end))
  if (nrow(ship_raw) == 0) stop("No track points fall within the configured survey dates.")
}

message("\nTotal rows across all files before dedup: ", nrow(ship_raw))


# ---- 6. DEDUPLICATE TO ONE RECORD PER MINUTE ----------------
# Applied once across the combined dataset so that minute-boundary
# rows at file edges are handled consistently.
# When two files overlap in time, the first row seen is kept
# (files are processed in the order listed in CSV_PATHS).

ship <- ship_raw %>%
  arrange(DateT) %>%
  filter(!duplicated(UTC1))

message("After dedup (one record per minute): ", nrow(ship), " records")

# QA: identify long breaks in the plotted track. This is checked after
# deduplication because `ship` is the data used for mapping and export.
track_times <- ship %>%
  arrange(DateT) %>%
  transmute(
    previous_time = lag(DateT),
    next_time     = DateT,
    gap_hours     = as.numeric(difftime(DateT, lag(DateT), units = "hours"))
  ) %>%
  filter(!is.na(gap_hours), gap_hours > 12)

if (nrow(track_times) == 0) {
  message("QA: No track gaps greater than 12 hours detected.")
} else {
  message("QA WARNING: ", nrow(track_times),
          " track gap(s) greater than 12 hours detected:")
  print(track_times)
}

track_start <- min(ship$DateT, na.rm = TRUE)
track_end   <- max(ship$DateT, na.rm = TRUE)
track_range_label <- paste(
  format(track_start, "%Y-%m-%d %H:%M UTC"),
  "to",
  format(track_end, "%Y-%m-%d %H:%M UTC")
)
message("Track range: ", track_range_label)


# ---- 7. WRITE CLEANED CSV -----------------------------------

# Create output directory if it does not exist
out_dir <- dirname(CSV_OUT)
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  message("Created output directory: ", out_dir)
}

write.csv(ship, CSV_OUT, row.names = FALSE)
message("Cleaned CSV written to: ", CSV_OUT)


# ---- 8. BUILD SF OBJECTS ------------------------------------

ship_sf <- st_as_sf(
  ship,
  coords = c("LongitudeD", "LatitudeD"),
  crs    = 4326,
  remove = FALSE   # keep coordinate columns in the data frame
)

track <- ship_sf %>%
  arrange(DateT) %>%
  st_combine() %>%
  st_cast("LINESTRING")


# ---- 9. QUICK PLOTS -----------------------------------------

plot(st_geometry(track), main = paste("Ship track", YEAR))

print(
  ggplot() +
    geom_sf(data = ship_sf, size = 0.3, colour = "steelblue") +
    coord_sf() +
    theme_minimal() +
    labs(title = paste("Ship track", YEAR),
         subtitle = paste(track_range_label, "|", nrow(ship_sf),
                          "position fixes from", length(file_list), "file(s)"))
)


# ---- 10. OPTIONAL: KML IMPORT -------------------------------

if (!is.null(KML_PATH)) {
  
  if (!file.exists(KML_PATH)) {
    warning("KML file not found: ", KML_PATH, " -- skipping KML section.")
  } else {
    
    message("Reading KML: ", KML_PATH)
    print(st_layers(KML_PATH))
    
    trk_raw <- st_read(KML_PATH, quiet = TRUE)
    
    geom_types <- unique(st_geometry_type(trk_raw))
    message("Geometry types in KML: ", paste(geom_types, collapse = ", "))
    
    trk_lines  <- trk_raw %>%
      filter(st_geometry_type(geometry) %in%
               c("LINESTRING", "MULTILINESTRING"))
    
    trk_points <- trk_raw %>%
      filter(st_geometry_type(geometry) == "POINT")
    
    if (nrow(trk_lines) > 0) {
      print(
        ggplot() +
          geom_sf(data = trk_lines, colour = "red") +
          coord_sf() + theme_minimal() +
          labs(title = paste("KML track", YEAR))
      )
    }
    
    if (nrow(trk_points) > 0) {
      print(
        ggplot() +
          geom_sf(data = trk_points, size = 0.4) +
          coord_sf() + theme_minimal() +
          labs(title = paste("KML points", YEAR))
      )
    }
  }
}


# ---- 11. OPTIONAL: REPROJECT AND WRITE SHAPEFILES ----------

if (WRITE_SHP) {
  if (!dir.exists(SHP_DIR)) dir.create(SHP_DIR, recursive = TRUE)
  
  ship_utm  <- st_transform(ship_sf, UTM_CRS)
  track_utm <- st_transform(track,   UTM_CRS)
  
  pts_path  <- file.path(SHP_DIR, paste0("shiptrackpts_", YEAR, ".shp"))
  line_path <- file.path(SHP_DIR, paste0("shiptrack_",    YEAR, ".shp"))
  
  st_write(ship_utm,  pts_path,  delete_layer = TRUE)
  st_write(track_utm, line_path, delete_layer = TRUE)
  
  message("Shapefiles written to: ", SHP_DIR)
}
