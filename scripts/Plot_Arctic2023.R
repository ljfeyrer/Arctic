#2023 data-----
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(readr)


#TRACK-------
ship2a = read.csv(here::here("~/CODE/Arctic/data/Time0/Canada2023leg1.csv"), skip = 1, header = T, stringsAsFactors = F)
ship2b = read.csv(here::here("~/CODE/Arctic/data/Time0/Canada2023leg2.csv"), skip = 1, header=T, stringsAsFactors=F)%>%select(-c(24, 25))

ship_2023 = rbind(ship2a, ship2b)


ship = ship_2023  %>% # fix lat long
  mutate(LatD = as.numeric(str_sub(ship_2023$Latitude,1,2)))%>%
  mutate(LatM = as.numeric(str_extract(ship_2023$Latitude,"(?<=-).+(?=-N)"))/60)%>%
  mutate(LatitudeD = LatD+LatM)%>%
  mutate(LongD = as.numeric(str_sub(ship_2023$Longitude,1,3)))%>%
  mutate(LongM = as.numeric(str_extract(ship_2023$Longitude,"(?<=-).+(?=-W)"))/60)%>%
  mutate(LongitudeD = (LongD+LongM)*-1)%>%
  mutate(Date = paste(Day, Month, Year,  sep = "/"))%>%
  mutate(Time = paste(Hour, Minute, Second, sep = ":"))%>%
  mutate(DateT = as.POSIXct(paste(Date, Time,  sep = ""), tz = "UTC", 
                            format = "%d/%m/%Y %H:%M:%S"))%>%
  mutate(UTC1 = format(as.POSIXct(DateT), "%Y-%m-%d %H:%M"))

# deduplicate based on minute
ship=ship[!duplicated(ship$UTC1), ]

#write clean track for mapping
ship = ship%>%dplyr::select(Latitude = LatitudeD, Longitude = LongitudeD, Year, Date, Time, DateT, UTC1)

#write clean track for mapping
ship = ship%>%filter(!is.na(Latitude))

ship = st_as_sf(ship, coords = c("Longitude", "Latitude"), crs = 4326)%>%mutate(LINE_ID = c(rep(1:(nrow(ship)/2), each = 2)))
tail(ship)

track = ship%>%arrange(UTC1)%>%st_combine()%>%st_cast("LINESTRING")
plot(st_geometry(track))

ggplot()+  geom_sf(data = track, col= "orange", lty = 2)

#write track data

# write_csv(ship, "data/shipTrack_2023.csv")
# write_rds(ship, "data/shipTrack_2023.rds")
write_sf(ship, "shapes/shiptrackpts_2023.shp")
write_sf(track, "shapes/shiptrack_2023.shp")


#cetaceans------
NBW2023 = read.csv("data/2023/ArcticNBW2023_Cetaceans.csv")%>%filter(Species == "Northern Bottlenose")%>%st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
