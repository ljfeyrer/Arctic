# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: Builds the shared Arctic basemap (bathymetry, DFO marine
# refuges, Canadian EEZ, land, place names) used by the photo-ID report and
# by 02_map_arctic_lv_sightings.R. Source it after loading ggplot2, sf,
# terra, dplyr and here; it defines `arctic_basemap`, `arctic_extent`,
# `xlims` and `ylims`. Add point layers, then `+ arctic_extent`.
#
# Basemap files are read from ARCTIC_BASEMAP_DIR (default ~/CODE/shapefiles),
# which is outside this repository.

# Use the same basemap assets and extent as baseMap_arctic_2025.R.
basemap_dir <- path.expand(Sys.getenv("ARCTIC_BASEMAP_DIR", "~/CODE/shapefiles"))
bathy_path <- file.path(basemap_dir, "Bathymetry/GEBCO_bathy/gebco_2020.tif")
eez_path <- file.path(basemap_dir, "EEZ/EEZ_can.shp")
land_path <- file.path(basemap_dir, "coastline/worldcountries/ne_50m_admin_0_countries.shp")
basemap_paths <- c(bathy_path, eez_path, land_path)
if (any(!file.exists(basemap_paths))) {
  stop("Missing basemap files. Set ARCTIC_BASEMAP_DIR to the shapefiles folder:\n",
       paste(basemap_paths[!file.exists(basemap_paths)], collapse = "\n"))
}

xlims <- c(-74, -51)
ylims <- c(60, 73)

bathy <- terra::rast(bathy_path)
# Align the crop to the original 2 × 2 aggregation grid before downsampling.
grid <- terra::res(bathy) * 2
origin <- c(terra::xmin(bathy), terra::ymax(bathy))
snap_out <- function(limits, origin, resolution) {
  origin + c(floor((limits[1] - origin) / resolution),
             ceiling((limits[2] - origin) / resolution)) * resolution
}
bathy <- terra::crop(bathy, terra::ext(c(snap_out(xlims, origin[1], grid[1]),
                                       snap_out(ylims, origin[2], grid[2]))))
bathy <- terra::aggregate(bathy, fact = 2)
names(bathy) <- "Depth"
bathy_df <- as.data.frame(bathy, xy = TRUE) %>%
  mutate(Depth = if_else(Depth >= -10, NA_real_, Depth))

eez <- st_read(eez_path, quiet = TRUE)
# DFO marine refuges (OECMs) that intersect the map extent; drawn faintly
# under the EEZ line. Skipped if the shapefile is not present.
source(here("scripts", "lib", "helpers.R"))
oecm_colour <- "#FFD400"
oecm <- read_oecm_in_extent(
  xlims, ylims,
  path = here("input", "basemaps", "DFO_OECM_MPO_AMCEZ_SHP", "DFO_OECM_MPO_AMCEZ.shp")
)
land <- st_read(land_path, quiet = TRUE) %>%
  filter(CONTINENT == "North America")

arctic_extent <- coord_sf(crs = 4326, xlim = xlims, ylim = ylims,
                          lims_method = "orthogonal", expand = FALSE)
cities <- tibble(x = c(-52, -68.5), y = c(64, 63.7),
                 name = c("Nuuk", "Iqaluit"))
arctic_basemap <- ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = Depth)) +
  scale_fill_gradient2(low = "#011f4b", mid = "#a2c0d0", high = "#b3cde0",
                       midpoint = -100, guide = "none") +
  {
    if (!is.null(oecm) && nrow(oecm)) {
      geom_sf(data = oecm, aes(linetype = "Marine refuges"),
              fill = alpha(oecm_colour, oecm_fill_alpha),
              color = oecm_colour, linewidth = 0.3)
    }
  } +
  geom_sf(data = eez, color = "white", fill = NA, linetype = "aa",
          linewidth = 0.5) +
  geom_sf(data = land, color = NA, fill = "grey50") +
  geom_text(data = cities, aes(x = x, y = y, label = name),
            fontface = "bold", size = 3) +
  scale_linetype_manual(
    values = c("Marine refuges" = "solid"), name = NULL,
    guide = guide_legend(order = 2, override.aes = list(
      fill = alpha(oecm_colour, oecm_fill_alpha), colour = oecm_colour
    ))
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.92, 0.97),
        legend.justification.inside = c(1, 1))
