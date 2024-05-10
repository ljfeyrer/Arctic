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



#filepath hacks------

shapefiles ="~/CODE/shapefiles/"

# import NBW Sightings---------
      #2022
      sightHa1 = read_csv(here::here("input/cetacean_sightings_trip1.csv"))%>%filter(Species == "Northern Bottlenose")
      sightHa1_2022 = st_as_sf(sightHa1, coords = c("Longitude", "Latitude"), crs = 4326)
      
      #from LV input
      sightHa2 = read_rds(here::here("input/sightHa_location.rds"))
      sightHa2 = sightHa2[!duplicated(sightHa2$Latitude, sightHa2$Longitude ), ]
      
      sightHa2_2022 = st_as_sf(sightHa2, coords = c("Longitude", "Latitude"), crs = 4326)%>%mutate(Year = "2022")
      
      
      #2023
      NBW2023 = read.csv("output/Arctic2023_AllCetaceans.csv")%>%filter(Species == "Northern Bottlenose")%>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)%>%mutate(Year = "2023")
      
      
      

# import ship tracks -----------
    ship_2022 = read_sf(here::here("shapes/ships_track2022.shp"))%>%st_transform(4326)
      ship_2023 = read_sf(here::here("shapes/shiptrack_2023.shp"))%>%st_transform(4326)
      

# soundtrap location
      # Soundtrap location in DMS
      soundtrap <- "66°51'12.0\"N, 59°03'18.0\"W"
      
      # Function to convert DMS to decimal
      dms_to_decimal <- function(dms) {
        parts <- strsplit(dms, '[°\'\"]')[[1]]
        degrees <- as.numeric(parts[1])
        minutes <- as.numeric(parts[2])
        seconds <- as.numeric(parts[3])
        sign <- ifelse(grepl('S|W', dms), -1, 1)
        return(sign * (degrees + minutes / 60 + seconds / 3600))
      }
      
      # Splitting the coordinate string into latitude and longitude
      coords <- strsplit(soundtrap, ', ')[[1]]
      latitude <- dms_to_decimal(coords[1])
      longitude <- dms_to_decimal(coords[2])
      
      # Creating an sf object
      soundtrap = st_as_sf(data.frame(ID = "sound trap", longitude, latitude), coords = c("longitude", "latitude"),crs = 4326)  # CRS 4326 for GPS coordinates

      soundtrap

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
      
      
      ggplot(bathy_crop, aes(x = x, y = y, fill = Depth)) + 
        geom_raster() + 
        scale_fill_viridis_c(direction = 1)


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
      
      cols = c("2022" ="#ffcb00", "2023"="#ce1c25", "sound trap" = "blue")

      
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
        geom_sf(data  = ship_2023, col= "#931c1b",size = 0.5
        ) +
  #add sightings of NBW
  geom_sf(data  = sightHa2_2022, aes(col = Year), fill = "#ffcb00", shape = 19, alpha = .5,
          size = 2) +
  geom_sf(data  = NBW2023, aes(col = Year),  shape = 21, fill = "#ce1c25",  alpha = .5,
          size = 2) +

  geom_sf(data  = soundtrap, aes(col = ID),  shape = 22, fill = "blue",  alpha = .6,
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
  guides(fill = "none", color = guide_legend(override.aes = list(shape = 19, size = 2)))+
  theme_bw()+
  # format axes
  ylab("") + 
  xlab("") +
  labs(col = "NBW Sightings")+
  theme(plot.margin = margin(.1, .5, .1, .1, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
       legend.key = element_blank(), legend.position = c(.85,.9) )
  
m

 
#save map
 
 gg_Fig2path =  here::here("FIGS/NBW_Sightings2022-2023.png")
ggsave(gg_Fig2path, m, dpi = 300, height = 7, width = 5, units = "in")


