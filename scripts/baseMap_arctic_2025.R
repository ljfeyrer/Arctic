# ### 1. Import basemap input:
# library(rgeos)

#libraries----------
#might not need all these...

# install.packages("pacman")
pacman::p_load(sp, terra, plyr, dplyr, sf, viridis, ggplot2, ggrepel, stringr, 
               here, ggtext, readr,
               pals, tidyr, fuzzyjoin, 
               patchwork,mapsf,readxl,
               ggforce, readr, ggspatial, lubridate, stars, patchwork, scales, 
               RColorBrewer, grafify)

here::here()

#filepath hacks------

shapefiles ="~/CODE/shapefiles/"

# import NBW Sightings---------
      # #2022
      # sightHa1 = read_csv(here::here("input/cetacean_sightings_trip1.csv"))%>%filter(Species == "Northern Bottlenose")
      # sightHa1_2022 = st_as_sf(sightHa1, coords = c("Longitude", "Latitude"), crs = 4326)
      # 
      # #from LV input
      # sightHa2 = read_rds(here::here("input/sightHa_location.rds"))
      # sightHa2 = sightHa2[!duplicated(sightHa2$Latitude, sightHa2$Longitude ), ]
      # 
      # sightHa2_2022 = st_as_sf(sightHa2, coords = c("Longitude", "Latitude"), crs = 4326)%>%mutate(Year = "2022")
      # 
      # 
      # #2023
      # NBW2023 = read.csv("output/Arctic2023_AllCetaceans.csv")%>%filter(Species == "Northern Bottlenose")%>%
      #   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)%>%mutate(Year = "2023")
      
      #2025
      NBW2025 = read.csv(here::here("output/Cetaceans/Cetaceans_ArcticLeg12025.csv"))%>%filter(Species != "seal", Species != "Unknown Baleen whale")%>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)%>%mutate(Year = "2025")
      
      

# import ship tracks -----------
    ship_2025 = read_sf(here::here("output/Track/ship_track.shp"))%>%st_transform(4326)

#import stations from leg 2 
Stns_L2_2025 = read.csv(here::here("input/compiled/Track/Stns_2025_leg2.csv"))%>%
  # fix lat long
  mutate(
    Start.Lat.Degree  = as.numeric(Start.Lat.Degree),
    Start.Lat.Minutes = as.numeric(Start.Lat.Minutes),
    Start.Long.Degree  = as.numeric(Start.Long.Degree),
    Start.Long.Minutes = as.numeric(Start.Long.Minutes),
    End.Lat.Degree  = as.numeric(End.Lat.Degree),
    End.Lat.Minutes = as.numeric(End.Lat.Minutes),
    End.Long.Degree  = as.numeric(End.Long.Degree),
    End.Long.Minutes = as.numeric(End.Long.Minutes),
    
    # decimal degrees, N positive, W negative
    start_lat_dd = Start.Lat.Degree + Start.Lat.Minutes/60,
    start_lon_dd = -(Start.Long.Degree + Start.Long.Minutes/60),
    end_lat_dd   = End.Lat.Degree   + End.Lat.Minutes/60,
    end_lon_dd   = -(End.Long.Degree   + End.Long.Minutes/60),
    Species = "Northern Bottlenose"
  )%>%select(Set.Number,Start.Date,Start.Depth,End.Depth,Biopsy,Photos, Species, Whale_min, end_lon_dd, end_lat_dd)%>%
  st_as_sf(coords=c("end_lon_dd","end_lat_dd"), crs=4326)


#read eez shapefile----
EEZ <- read_sf(paste(shapefiles, "EEZ/EEZ_can.shp", sep = ""))
      
      #extent----
      
      # Get the vertical and horizontal limits
      
      lims = list(  x = c(-74, -51), y = c(60,73))
      
      ext <- raster::extent( lims )
      
      # # Get x and y limits
      # adj <- round( ext@xmin*0.1 )
      # lims <- list( x=c(ext@xmin-adj, ext@xmax+adj), y=c(ext@ymin-adj, ext@ymax+adj) )


# bathy input -------

      #for more detailed contours use GEBCO
      
      r <- terra::rast(paste(shapefiles,"Bathymetry/GEBCO_bathy/gebco_2020.tif", sep = ""))
      # st_crs(r)
      
      #need to downsample bc too big
      bathy = terra::aggregate(r, fact = 2)

      
      #now crop to extent of study area
      bathy_crop = crop(bathy, ext)
      
      bathy_crop <- as.data.frame(bathy_crop, xy = T)%>%dplyr::rename(Depth = gebco_2020)%>%
        mutate(Depth = ifelse(Depth >=-10, NA, Depth))
      
      
# north america for reference------

land <- read_sf(here::here("~/CODE/shapefiles/coastline/worldcountries/ne_50m_admin_0_countries.shp"))%>%
  dplyr::filter(CONTINENT == "North America")

#  plot basemap as object m-----
      
      #extent----
      
      # Get the vertical and horizontal limits
      
      lims = list(  x = c(-74, -51), y = c(60,73))
      
      ext <- raster::extent( lims )
      
      # # Get x and y limits
      # adj <- round( ext@xmin*0.1 )
      # lims <- list( x=c(ext@xmin-adj, ext@xmax+adj), y=c(ext@ymin-adj, ext@ymax+adj) )
      
      cols = c("Northern Bottlenose" ="#ffcb00", "Bowhead"="#ce1c25", "seal" = "blue")

      
m<-ggplot() +
  # add bathy--
  geom_raster(data  = bathy_crop, aes(x= x, y=y, fill = Depth)) +
  
  scale_fill_gradient2(high = "#b3cde0", 
                       mid = "#a2c0d0", 
                       low = "#011f4b", 
                       midpoint = -100)+
  
  #add eez
  geom_sf(data  = EEZ, col = "white", fill = NA, lty = "aa", size = 0.5) +
  
# add land region
  geom_sf(  data  = land, color=NA, fill="grey50") +
  
  #add ship track
  
  # geom_sf(data  = ship_2022, col= "#ff8700", size = 0.5
  #         ) +
        geom_sf(data  = ship_2025, col= "darkgrey",size = 0.25
        ) +
  #add sightings of NBW
   geom_sf(data  = NBW2025, aes(col = Species),  fill = "white", shape = 18, alpha = .5,
          size = 3) +
  geom_sf(data  = Stns_L2_2025, aes(col = Species),  fill = "white", shape = 18, alpha = .5,
          size = 3) +
  # set map limits
  coord_sf(lims_method = "orthogonal",
           xlim=lims$x, ylim=lims$y, expand = F
  )+
  
  # add text annotation for Nuuk
  annotate(geom = "text", y = 64., x = -52, label = "Nuuk",fontface = "bold",
           color = "black", size =3, ) +
  annotate(geom = "text", y = 63.7, x = -68.5, label = "Iqaluit",fontface = "bold",
           color = "black", size =3, ) +
  

  #legend
  scale_color_manual(values = cols)+
  guides(fill = "none", color = guide_legend(override.aes = list(shape = 18, size = 3)))+
  theme_bw()+
  # format axes
  ylab("") + 
  xlab("") +
  labs(col = "Whale Sightings")+
  theme(plot.margin = margin(.1, .1, .1, .1, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
       legend.key = element_blank(), legend.position = c(.74,.9) )
  
m

 
#save map
 
 gg_Fig2path =  here::here("output/FIGS/NBW_Sightings2025.png")
ggsave(gg_Fig2path, m, dpi = 300, height = 7, width = 5, units = "in")


