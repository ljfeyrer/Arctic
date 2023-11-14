# ### 1. Import basemap data:
# library(rgeos)

#libraries----------
#might not need all these...

# install.packages("pacman")
pacman::p_load(sp, terra, plyr, dplyr, sf, viridis, ggplot2, ggrepel, stringr, here, ggtext, readr,
               rnaturalearth, rnaturalearthdata, pals, tidyr, fuzzyjoin, patchwork,mapsf,readxl,
               ggforce, readr, raster, ggspatial, rgeos, lubridate, stars, patchwork, scales, RColorBrewer, grafify)




#filepath hacks------

file.shapes = "~/CODE/TowedArray/Shapes/"
shapefiles ="~/CODE/Mapping/shapefiles/"

# import NBW Sightings---------
      sightHa1 = read_csv(here::here("data/cetacean_sightings_trip1.csv"))%>%filter(Species == "Northern Bottlenose")
      sightHa1 = st_as_sf(sightHa1, coords = c("Longitude", "Latitude"), crs = 4326)
      
      #from LV data
      sightHa2 = read_rds(here::here("data/sightHa_location.rds"))
      sightHa2 = sightHa2[!duplicated(sightHa2$Latitude, sightHa2$Longitude ), ]
      
      sightHa2 = st_as_sf(sightHa2, coords = c("Longitude", "Latitude"), crs = 4326)


# import ship track -----------
    ship = read_sf(here::here("shapes/ships_track2022.shp"))%>%st_transform(4326)

#extent----
    ext <- extent( lims )
    
    # Get x and y limits
    adj <- round( ext@xmin*0.1 )
    lims <- list( x=c(ext@xmin-adj, ext@xmax+adj), y=c(ext@ymin-adj, ext@ymax+adj) )
    
    lims = list(  x = c(-65, -51), y = c(60, 68.25))


#read eez shapefile----
EEZ <- read_sf(paste(shapefiles, "World_EEZ_v11_20191118/eez_v11.shp", sep = ""))%>%filter(TERRITORY1 == "Canada")


# bathy data -------

      #for more detailed contours use GEBCO
      
      r <- terra::rast("~/CODE/Mapping/shapefiles/GEBCO_bathy/gebco_2020.tif")
      st_crs(r)
      
      #need to downsample bc too big
      bathy = terra::aggregate(r, fact = 2)
      # bathy_df <- as.data.frame(bathy, xy = T)%>%dplyr::rename(Depth = gebco_2020)
      
      # ggplot(bathy_df, aes(x = x, y = y, fill = Depth)) +
      #   geom_raster() +
      #   scale_fill_viridis_c(direction = 1)
      # ext(bathy)
      
      #now crop to extent of study area
      bathy_crop = crop(bathy, ext)
      
      bathy_crop <- as.data.frame(bathy_crop, xy = T)%>%dplyr::rename(Depth = gebco_2020)%>%
        mutate(Depth = ifelse(Depth >=-10, NA, Depth))
      
      
      ggplot(bathy_crop, aes(x = x, y = y, fill = Depth)) + 
        geom_raster() + 
        scale_fill_viridis_c(direction = 1)


# north america for reference------
land <- ne_countries( scale = "large",continent = "north america", returnclass = "sf")


land <- read_sf(here::here("~/CODE/Mapping/shapefiles/countries/ne_50m_admin_0_countries.shp"))%>%
  dplyr::filter(CONTINENT == "North America")

#  plot basemap as object m-----
m<-ggplot() +
  # add bathy--
  geom_raster(data = bathy_crop, aes(x= x, y=y, fill = Depth)) +
  
  scale_fill_gradient2(high = "#b3cde0", 
                       mid = "#a2c0d0", 
                       low = "#011f4b", 
                       midpoint = -100)+
  
  #add eez
  geom_sf(data = EEZ, col = "grey50", fill = NA, lty = "aa", size = 0.2) +
  
# add land region
  geom_sf(  data = land, color=NA, fill="grey50") +
  
  #add ship track
  
  geom_sf(data = ship, col= "yellow", lty = 2,
          ) +
  #add sightings of NBW
  # geom_sf(data = sightHa1, col = "blue", fill = NA,
  #         size = 3, shape = 8) +
  geom_sf(data = sightHa2, col = "#ee7600", fill = "#ee7600", alpha = 0.35,
          size = 3, shape = 21) +
  # set map limits
  coord_sf(lims_method = "orthogonal",
           xlim=lims$x, ylim=lims$y, expand = F
  )+
  
  # add text annotation for Nuuk
  annotate(geom = "text", y = 64.17, x = -51.7, label = "Nuuk",fontface = "bold",
           color = "black", size = 4, ) +

  # format axes
  ylab("") + 
  xlab("") +
  theme(plot.margin = margin(.1, .5, .1, .1, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
       legend.title = element_blank() , legend.position = "none"   )
m

 
#save map
 
 gg_Fig2path =  here::here("FIGS/NBW_Sightings2022.png")
ggsave(gg_Fig2path, m, width = 5, height = 7, units = "in", dpi = 300)


