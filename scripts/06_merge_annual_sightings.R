#!/usr/bin/env Rscript

# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: Merge the within-source-deduplicated Tarajoq NBW sightings
# with the normalized Kiviuq sightings into one annual, multi-vessel NBW
# sightings table per year, with a conservative cross-vessel duplicate audit.
# Also produces one bathymetry-background map, faceted by year with point
# shape/colour by vessel, for QA review and reuse as a presentation figure.
#
# This step must run after 03_compile_normalized_tarajoq_sightings.R (for the
# Tarajoq side, under output/Cetaceans/) and 02_normalize_kiviuq_sightings.R
# (for the Kiviuq side), and before 07_crossref_compiled_against_reference_dbs.R,
# which now reads this script's output instead of the Tarajoq-only one.
# output/Sightings/ is reserved for this merge-across-sources result and the
# reference-database cross-reference -- the single-vessel/program compiled
# sightings live under output/Cetaceans/ instead. A year with only one
# vessel's data (e.g. 2018 Kiviuq-only, 2025 Tarajoq-only) is handled -- the
# other side just contributes zero rows.
#
# Inputs (never modified):
#   output/Cetaceans/tarajoq_nbw_sightings_<year>_{compiled_deduplicated,deduplicated}.csv
#   input/normalized/<year>/sightings/kiviuq_sightings_<year>_standardized.csv
#   input/basemaps/bathymetry_marmap_res15.csv (0.25-degree global bathymetry grid)
#   input/basemaps/eez_greenland.json (Greenland EEZ only; no Canada EEZ layer
#     is available in-repo yet)
#
# Outputs per year: either "nbw_annual_sightings_<year>_compiled_deduplicated.csv"
# when no cross-vessel duplicates were found, or the pair
# "nbw_annual_sightings_<year>_{compiled,deduplicated}.csv" when some were
# (see write_compiled_dedup() in scripts/lib/helpers.R). Also:
#   output/Sightings_QA/nbw_annual_sightings_<year>_duplicate_candidates.csv
#   output/Sightings_QA/nbw_annual_sightings_<year>_deduplication_audit.csv
#   output/FIGS/nbw_annual_sightings_by_year_bathymetry.png

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(terra)
})

source("scripts/lib/helpers.R")

nbw_species_regex <- "northern bottlenose|northen bottlenose|bottlenose whale|\\bnbw\\b"

year_arg <- get_arg("year", "all")

cetaceans_root <- "output/Cetaceans"
sightings_root <- "output/Sightings"
normalized_root <- "input/normalized"
qa_root <- "output/Sightings_QA"
figs_root <- "output/FIGS"
bathy_path <- "input/basemaps/bathymetry_marmap_res15.csv"
eez_path <- "input/basemaps/eez_greenland.json"
dir.create(sightings_root, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_root, recursive = TRUE, showWarnings = FALSE)
dir.create(figs_root, recursive = TRUE, showWarnings = FALSE)

tarajoq_years <- list.files(
  cetaceans_root,
  pattern = "^tarajoq_nbw_sightings_[0-9]{4}_(compiled_deduplicated|deduplicated)\\.csv$"
) %>%
  str_extract("[0-9]{4}") %>%
  as.integer()
kiviuq_years <- list.files(file.path(normalized_root), pattern = "^[0-9]{4}$") %>%
  keep(~ file.exists(file.path(normalized_root, .x, "sightings", paste0("kiviuq_sightings_", .x, "_standardized.csv")))) %>%
  as.integer()
available_years <- sort(union(tarajoq_years, kiviuq_years))
years <- if (identical(year_arg, "all")) available_years else as.integer(year_arg)
years <- years[years %in% available_years]
if (!length(years)) stop("No Tarajoq-compiled or Kiviuq-normalized years found.")

# ---- READERS ----------------------------------------------------------------
# Both return the same columns so they can be row-bound directly:
# record_id, source_year, source_type, vessel, standard_datetime,
# standard_date, standard_time, latitude, longitude, count, species,
# survey_leg, coordinate_source, coordinate_use, source_file, source_sheet,
# source_row, source_comments, source_path.

read_tarajoq_side <- function(year) {
  path <- resolve_dedup_file(cetaceans_root, "tarajoq_nbw_sightings", year)
  if (is.na(path)) {
    return(tibble())
  }
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    transmute(
      record_id,
      source_year = as.integer(source_year), source_type, vessel = "Tarajoq",
      standard_datetime = as_datetime(standard_datetime, tz = "UTC"),
      standard_date = as.Date(standard_date), standard_time,
      latitude = as_num(latitude), longitude = as_num(longitude), count = as_num(count),
      species, survey_leg, coordinate_source, coordinate_use,
      source_file, source_sheet, source_row, source_comments,
      source_path = normalizePath(path, winslash = "/", mustWork = FALSE)
    )
}

read_kiviuq_side <- function(year) {
  path <- file.path(
    normalized_root, as.character(year), "sightings",
    paste0("kiviuq_sightings_", year, "_standardized.csv")
  )
  if (!file.exists(path)) {
    return(tibble())
  }
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    filter(str_detect(str_to_lower(coalesce(species, "")), nbw_species_regex)) %>%
    transmute(
      record_id,
      source_year = as.integer(source_year), source_type = "kiviuq", vessel = "Kiviuq",
      standard_datetime = as_datetime(standard_datetime, tz = "UTC"),
      standard_date = as.Date(standard_date), standard_time,
      latitude = as_num(latitude), longitude = as_num(longitude), count = as_num(count),
      species = "Northern Bottlenose Whale", survey_leg,
      coordinate_source, coordinate_use = NA_character_,
      source_file, source_sheet, source_row,
      source_comments = comments,
      source_path = normalizePath(path, winslash = "/", mustWork = FALSE)
    )
}

# ---- CROSS-VESSEL DUPLICATE CANDIDATES --------------------------------------
# Same conservative rule as 03_compile_normalized_tarajoq_sightings.R: only an
# exact datetime+coordinate match, or a same-date+coordinate match where one
# side has no time, is auto-deduplicated. A merely "very_close" pair is kept
# in the audit for manual review but never auto-removed.

find_candidates <- function(data) {
  if (nrow(data) < 2) {
    return(tibble())
  }
  map_dfr(seq_len(nrow(data) - 1), function(i) {
    map_dfr((i + 1):nrow(data), function(j) {
      if (data$vessel[i] == data$vessel[j]) {
        return(tibble())
      }
      dt_diff <- abs(as.numeric(difftime(data$standard_datetime[i], data$standard_datetime[j], units = "secs")))
      distance <- if (all(!is.na(c(data$latitude[i], data$longitude[i], data$latitude[j], data$longitude[j])))) {
        haversine_m(data$latitude[i], data$longitude[i], data$latitude[j], data$longitude[j])
      } else {
        NA_real_
      }
      same_date <- !is.na(data$standard_date[i]) && !is.na(data$standard_date[j]) &&
        data$standard_date[i] == data$standard_date[j]
      exact <- !is.na(dt_diff) && dt_diff <= 1 && !is.na(distance) && distance <= 10
      date_coord <- same_date && !is.na(distance) && distance <= 10 &&
        (is.na(data$standard_datetime[i]) || is.na(data$standard_datetime[j]))
      very_close <- !is.na(dt_diff) && dt_diff <= 60 && !is.na(distance) && distance <= 100
      if (!(exact || date_coord || very_close)) {
        return(tibble())
      }
      tibble(
        record_id_a = data$record_id[i], record_id_b = data$record_id[j],
        vessel_a = data$vessel[i], vessel_b = data$vessel[j],
        datetime_difference_seconds = dt_diff, distance_m = distance,
        match_class = case_when(
          exact ~ "exact_datetime_coordinate",
          date_coord ~ "same_date_coordinate_missing_time",
          TRUE ~ "very_close"
        ),
        auto_deduplicate = exact || date_coord
      )
    })
  })
}

priority <- c(Tarajoq = 1L, Kiviuq = 2L)
all_years_dedup <- list()

for (year in years) {
  combined <- bind_rows(read_tarajoq_side(year), read_kiviuq_side(year)) %>%
    arrange(standard_datetime, vessel, source_file, source_row)
  if (!nrow(combined)) {
    message(year, ": no Tarajoq or Kiviuq NBW records")
    next
  }

  candidates <- find_candidates(combined)

  parent <- seq_len(nrow(combined))
  find_root <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]
      x <- parent[x]
    }
    x
  }
  if (nrow(candidates)) {
    for (k in which(candidates$auto_deduplicate)) {
      a <- match(candidates$record_id_a[k], combined$record_id)
      b <- match(candidates$record_id_b[k], combined$record_id)
      ra <- find_root(a)
      rb <- find_root(b)
      if (ra != rb) parent[rb] <- ra
    }
  }
  groups <- vapply(seq_len(nrow(combined)), find_root, integer(1))
  group_id <- ifelse(duplicated(groups) | duplicated(groups, fromLast = TRUE),
    paste0(year, "_annual_dedup_", groups), NA_character_
  )
  combined <- combined %>%
    mutate(
      dedup_group_id = group_id,
      source_priority = unname(priority[vessel]),
      retained = TRUE,
      dedup_status = if_else(is.na(dedup_group_id), "unique", "duplicate_group_representative_pending")
    )
  for (g in unique(na.omit(combined$dedup_group_id))) {
    ix <- which(combined$dedup_group_id == g)
    keep <- ix[order(combined$source_priority[ix], is.na(combined$standard_datetime[ix]), combined$source_file[ix])[1]]
    combined$retained[ix] <- FALSE
    combined$retained[keep] <- TRUE
    combined$dedup_status[ix] <- "duplicate_removed"
    combined$dedup_status[keep] <- "retained_duplicate_representative"
  }

  candidate_file <- file.path(qa_root, paste0("nbw_annual_sightings_", year, "_duplicate_candidates.csv"))
  audit_file <- file.path(qa_root, paste0("nbw_annual_sightings_", year, "_deduplication_audit.csv"))
  write_compiled_dedup(combined, sightings_root, "nbw_annual_sightings", year)
  write_csv(candidates, candidate_file, na = "")
  write_csv(combined %>% count(vessel, retained, dedup_status, name = "records"), audit_file, na = "")

  message(
    year, ": ", sum(combined$vessel == "Tarajoq"), " Tarajoq + ",
    sum(combined$vessel == "Kiviuq"), " Kiviuq NBW records -> ",
    sum(combined$retained), " retained after cross-vessel deduplication"
  )
  all_years_dedup[[as.character(year)]] <- combined %>% filter(retained)
}

# ---- ANNUAL BATHYMETRY FACET MAP --------------------------------------------

map_data <- bind_rows(all_years_dedup) %>%
  filter(!is.na(latitude), !is.na(longitude))

map_path <- file.path(figs_root, "nbw_annual_sightings_by_year_bathymetry.png")
# Higher-resolution bathymetry (15 arc-second GEBCO 2020), matching the one
# used in scripts/legacy/baseMap_arctic_2025.R for output/FIGS/NBW_Sightings2025.png.
# This path is a personal, machine-specific location outside the repo, so a
# year with no access to it falls back to the coarser in-repo 0.25-degree
# grid at bathy_path instead of failing.
bathy_hires_path <- path.expand("~/CODE/shapefiles/Bathymetry/GEBCO_bathy/gebco_2020.tif")

read_bathy <- function(xlim, ylim) {
  if (file.exists(bathy_hires_path)) {
    message("Using high-resolution GEBCO bathymetry: ", bathy_hires_path)
    r <- terra::rast(bathy_hires_path)
    r <- terra::crop(r, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2]))
    r <- terra::aggregate(r, fact = 3, fun = "mean")
    out <- as.data.frame(r, xy = TRUE)
    names(out) <- c("longitude", "latitude", "depth_m")
    return(filter(out, depth_m < -10))
  }
  message("GEBCO raster not found at ", bathy_hires_path, "; using the in-repo 0.25-degree grid instead.")
  read_csv(bathy_path,
    skip = 1, col_names = c("longitude", "latitude", "depth_m"),
    col_types = "ddd", show_col_types = FALSE
  ) %>%
    filter(
      longitude >= xlim[1], longitude <= xlim[2],
      latitude >= ylim[1], latitude <= ylim[2],
      depth_m < -10
    )
}

if (!nrow(map_data)) {
  message("No valid coordinates available across merged years; map not written.")
} else {
  pad <- 1
  xlim <- range(map_data$longitude, na.rm = TRUE) + c(-pad, pad)
  ylim <- range(map_data$latitude, na.rm = TRUE) + c(-pad, pad)

  bathy <- read_bathy(xlim, ylim)
  land <- ne_countries(scale = 50, returnclass = "sf")
  eez <- if (file.exists(eez_path)) st_read(eez_path, quiet = TRUE) %>% st_transform(st_crs(land)) else NULL

  # A few, well-spaced axis breaks instead of ggplot's dense defaults, which
  # overlap once squeezed into one of six facet panels.
  x_breaks <- pretty(xlim, n = 3)
  y_breaks <- pretty(ylim, n = 4)

  # Size the output so each panel's plotted width:height matches the true
  # geographic aspect (longitude degrees scaled by cos(latitude) vs. latitude
  # degrees). Without this, coord_sf's fixed aspect pads equal whitespace on
  # either side of every panel to fit a wide grid cell.
  n_facets <- n_distinct(map_data$source_year)
  facet_ncol <- 3
  facet_nrow <- ceiling(n_facets / facet_ncol)
  panel_aspect <- (diff(xlim) * cos(mean(ylim) * pi / 180)) / diff(ylim)
  fig_height <- 10
  fig_width <- fig_height * panel_aspect * facet_ncol / facet_nrow

  facet_map <- ggplot() +
    geom_raster(data = bathy, aes(x = longitude, y = latitude, fill = depth_m)) +
    scale_fill_gradient2(low = "#011f4b", mid = "#a2c0d0", high = "#b3cde0", midpoint = -1000, name = "Depth (m)") +
    {
      if (!is.null(eez)) {
        geom_sf(data = eez, colour = "white", fill = NA, linewidth = 0.4, linetype = "dashed", inherit.aes = FALSE)
      }
    } +
    geom_sf(data = land, fill = "grey50", colour = NA, inherit.aes = FALSE) +
    geom_point(
      data = map_data,
      aes(x = longitude, y = latitude, shape = vessel, colour = vessel),
      size = 1.6, alpha = 0.75
    ) +
    scale_shape_manual(values = c(Tarajoq = 16, Kiviuq = 17)) +
    scale_colour_manual(values = c(Tarajoq = "#D55E00", Kiviuq = "#0072B2")) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    facet_wrap(~source_year, ncol = facet_ncol) +
    labs(
      title = "NBW sightings by year", subtitle = "Fishery survey field observations",
      x = NULL, y = NULL, shape = "Vessel", colour = "Vessel"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom", panel.grid = element_blank())

  ggsave(map_path, facet_map, width = fig_width, height = fig_height + 1.3, units = "in", dpi = 220, limitsize = FALSE)
  message("Annual bathymetry facet map written to: ", map_path)
}
