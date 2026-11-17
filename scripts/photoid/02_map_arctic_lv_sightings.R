# ==============================================================================
# Script: Arctic NBW Photo-ID Catalogue -- Sightings map (standalone figure)
# Author: Laura Joan Feyrer
# Date Updated: 2026-11-16
# Description: Draws one map of every catalogue photo location (2017+) on the
#              shared Arctic basemap and saves it as a PNG, so the figure is
#              available without knitting Arctic_NBW_PhotoID_Report.Rmd.
#              The report's own maps colour the same points by year.
#
# Run after scripts/photoid/00_clean_arctic_lv.R.
#
# Input:  OUTPUT/PhotoID/LV_ARC_reliable.rds
# Output: OUTPUT/FIGS/arctic_photoid_sightings_map.png
# ==============================================================================

pacman::p_load(dplyr, here, ggplot2, sf, terra)

source(here("scripts", "photoid", "arctic_basemap.R"))

sightings_file <- here("OUTPUT", "PhotoID", "LV_ARC_reliable.rds")
if (!file.exists(sightings_file)) {
  stop("Missing ", sightings_file, ". Run scripts/photoid/00_clean_arctic_lv.R first.")
}

photo_points <- readRDS(sightings_file) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

sightings_map <- arctic_basemap +
  geom_sf(
    data = st_jitter(photo_points, factor = 0.005),
    aes(colour = "Photo-ID sightings"), shape = 18, size = 2, alpha = 0.6
  ) +
  scale_colour_manual(
    values = c("Photo-ID sightings" = "#D55E00"), name = NULL,
    guide = guide_legend(order = 1, override.aes = list(alpha = 1))
  ) +
  arctic_extent

out_file <- here("OUTPUT", "FIGS", "arctic_photoid_sightings_map.png")
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
ggsave(out_file, sightings_map, width = 5, height = 7, units = "in", dpi = 300)
cat("Map written to:", out_file, "\n")
