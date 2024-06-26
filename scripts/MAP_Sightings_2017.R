#Map Validation effort and NBWs
#depends on merge_acoustic_effort_GPS.R
library(ggplot2)
library(dplyr)
library(rgdal)
 # install.packages("marmap")
library(marmap)
# install.packages("ggmap")

library(ggmap)
library(maps)
library(mapdata)
# install.packages("mapdata")

 # install.packages("PBSmapping")

library(PBSmapping)
 # install.packages("ocedata")

library(ocedata)
data("coastlineWorldFine")
# Get bathy for the world with a decent resolution (careful: ca. 21 Mo)
world <- getNOAA.bathy(-180, 180, -90, 90, res = 15, keep = TRUE)

#visual effort?
library(readxl)
Vis_NBW <- read_excel("~/Documents/FOR STEVE FERGUSON/Sightings_GPS.xlsx")
Effort_GPS <- read_excel("~/Documents/FOR STEVE FERGUSON/Sightings_GPS.xlsx", 
                         +     sheet = "effort")
View(Effort_GPS)

#Turn sightings to coords  first---------

Vis_NBW1 = Vis_NBW
Vis_NBW1= select(Vis_NBW1, c( "Long_start", "Lat_start"))
Vis_NBW1 = Vis_NBW1%>%filter( !is.na(Long_start))

Biopsy_NBW = filter(Vis_NBW, Comments == "successful biopsy attempt") %>%select(c( "Long_start", "Lat_start"))
#Turn effort to coords 
Ef_GPS1 = Effort_GPS
Ef_GPS1= select(Ef_GPS1, c( "Long_start", "Lat_start"))
Ef_GPS1 = Ef_GPS1%>%filter( !is.na(Long_start))


# get bathymetry data
b = getNOAA.bathy(lon1 = -70, lon2 = -40, lat1 = 59, lat2 = 75, 
                  resolution = 1)
# convert bathymetry
bathyLon = as.numeric(rownames(b))
bathyLat = as.numeric(colnames(b))
bathyZ = as.numeric(b)
dim(bathyZ) = dim(b)


#for printing####
png("Arctic_NBW sightings.png",
    width = 8, height = 7, units = "in", 
    bg = "white",  res = 150)
# plot coastline (no projection)
span = 1800
#span = 2600

plot(coastlineWorldFine, clon = -57, clat = 66, span = span, border = "grey")

# plot bathymetry
contour(bathyLon,bathyLat,bathyZ,
        levels = c(-500, -1000, -2000, -3000),
        lwd = c(1, 2, 1, 1),
        lty = c(3, 1, 3, 1),
        drawlabels = F, add = TRUE, col = 'darkgray')

#plot points

# points(GPS_TRACK, pch=17, cex=.4, col ="dark blue")
points(Ef_GPS1, pch=19, cex=.75, col ="turquoise")
points(Vis_NBW1, pch=19, cex=.5, col ="orange")
points(Biopsy_NBW,  pch=19, cex=.6, col ="red")

# add depth legend
legend("bottomright", seg.len = 3, bty="n" ,
       lwd = c(1, 2, 1, 1),
       lty = c(3, 1, 3, 1),
       legend = c("500", "1000", "2000", "3000"),
       
       col = 'darkgray', title = "Depth [m]", bg= "white")
legend("bottomleft", seg.len = 3, bty="n" ,bg= "white",
       # lwd = c(1, 2, 1, 1),
       # lty = c(3, 1, 3, 1),
       legend = c("Effort","Sighting", "Biopsy"),
       pch=c(19,19,19),
       
       col = c('turquoise','orange',"red"), title = "NBW Survey Data")

dev.off()

# ##########try to do inset######-----
# plot.new()
# pushViewport(viewport())
# install.packages("gridBase")
# library(gridBase)
# library(grid)
# 
# 
# pushViewport(viewport(x = 0.67, y = 0.97, width = 0.3, height = 0.3, just = c("left", "top")))
# grid.rect(gp = gpar(fill = "white", lwd = 0.5))
# par(plt = gridPLT(), bg = "white", new = TRUE)

# Plot with ggplot2
autoplot(b, geom=c("raster", "contour")) + scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high="darkgreen") 



