library(dplyr)
library(stringr)
library(sf)
library(ggplot2)

# read the 2025 track file ---------------------------------------

ship_2025 <- read.csv(
  "input/original_data/Time0/Canada2025_trk.csv",
  skip   = 2,      # skip "TK,1,2" and "#Date: ..."
  header = FALSE,
  stringsAsFactors = FALSE
)

# give columns names so the rest is readable
names(ship_2025) <- c(
  "RecID", "TrackID", "Latitude", "Longitude",
  "Col5", "Col6", "Col7", "Col8",
  "Year", "Month", "Day",
  "Hour", "Minute", "Second",
  "Col15", "Col16", "Col17"
)


# convert to decimal degrees and build datetime -------------------

ship <- ship_2025 %>%
  mutate(
    # lat "64-46.4731-N"
    LatD = as.numeric(str_sub(Latitude, 1, 2)),
    LatM = as.numeric(str_extract(Latitude, "(?<=-).+(?=-N)")) / 60,
    LatitudeD = LatD + LatM,
    
    # lon "054-42.3196-W"
    LongD = as.numeric(str_sub(Longitude, 1, 3)),
    LongM = as.numeric(str_extract(Longitude, "(?<=-).+(?=-W)")) / 60,
    LongitudeD = -(LongD + LongM),
    
    Date = paste(Day, Month, Year, sep = "/"),
    Time = paste(Hour, Minute, Second, sep = ":"),
    DateT = as.POSIXct(
      paste(Date, Time),
      tz = "UTC",
      format = "%d/%m/%Y %H:%M:%S"
    ),
    UTC1 = format(DateT, "%Y-%m-%d %H:%M")
  ) %>%
  # dedupe to one record per minute, 
  filter(!duplicated(UTC1)) %>%
  filter(!is.na(LatitudeD)) %>%
  select(
    Latitude  = LatitudeD,
    Longitude = LongitudeD,
    Year, Date, Time, DateT, UTC1
  )

# points and line track as sf -------------------------------------

ship_sf <- st_as_sf(
  ship,
  coords = c("Longitude", "Latitude"),
  crs = 4326
) 

track <- ship_sf %>%
  arrange(UTC1) %>%
  st_combine() %>%
  st_cast("LINESTRING")

# sanity check - plot------
plot(st_geometry(track))
ggplot() + geom_sf(data = ship_sf, size = 0.3)

# write outputs if you want
# write_sf(ship_sf, "shapes/shiptrackpts_2025.shp")
# write_sf(track,   "shapes/shiptrack_2025.shp")

# --- SET KML file PATH # ----------------------------------------------------------
kml_path <- "input/original_data/Time0/Canada2025.kml"

# ----------------------------------------------------------
# READ KML + INSPECT LAYERS
# ----------------------------------------------------------
print(st_layers(kml_path))

# If the KML has a single layer, this works:
trk_raw <- st_read(kml_path, quiet = TRUE)

# If it has multiple layers and you need to pick one:
# trk_raw <- st_read(kml_path, layer = "NameOfLayer", quiet = TRUE)

# ----------------------------------------------------------
# SEPARATE GEOMETRY TYPES
# ----------------------------------------------------------
geom_types <- unique(st_geometry_type(trk_raw))
print(geom_types)

trk_lines <- trk_raw %>%
  filter(st_geometry_type(geometry) %in% c("LINESTRING", "MULTILINESTRING"))

trk_points <- trk_raw %>%
  filter(st_geometry_type(geometry) == "POINT")

# ----------------------------------------------------------
# QUICK BASE PLOTS
# ----------------------------------------------------------
if (nrow(trk_lines) > 0) {
  plot(st_geometry(trk_lines), col = "red")
}

if (nrow(trk_points) > 0) {
  plot(st_geometry(trk_points), col = "blue", add = TRUE)
}

# ----------------------------------------------------------
# GGPlot versions
# ----------------------------------------------------------

if (nrow(trk_lines) > 0) {
  print(
    ggplot() +
      geom_sf(data = trk_lines, color = "red") +
      coord_sf() +
      theme_minimal()
  )
}

if (nrow(trk_points) > 0) {
  print(
    ggplot() +
      geom_sf(data = trk_points, size = 0.4) +
      coord_sf() +
      theme_minimal()
  )
}

# ----------------------------------------------------------
# OPTIONAL: REPROJECT + EXPORT SHAPEFILES
# ----------------------------------------------------------

# Example: UTM Zone 20N
trk_lines_utm  <- if (nrow(trk_lines)  > 0) st_transform(trk_lines, 32620) else NULL
trk_points_utm <- if (nrow(trk_points) > 0) st_transform(trk_points, 32620) else NULL

# Uncomment if you want shapefiles:
# if (!is.null(trk_lines_utm))  st_write(trk_lines_utm,  "shiptrack_2025.shp",    delete_layer = TRUE)
# if (!is.null(trk_points_utm)) st_write(trk_points_utm, "shiptrackpts_2025.shp", delete_layer = TRUE)
