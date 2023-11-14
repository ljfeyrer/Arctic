#2023 data
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(readr)

ship2 = read.csv(here::here("~/CODE/Arctic/data/Time0/Time0_2023.csv"), skip = 1, header = T, stringsAsFactors = F)
# ship2 = ship2%>%dplyr::select(V1, V3, V4)

# colnames(ship2)= c("V1", "Latitude", "Longitude" )


ship = ship2  %>% # fix lat long
  mutate(LatD = as.numeric(str_sub(ship2$Latitude,1,2)))%>%
  mutate(LatM = as.numeric(str_extract(ship2$Latitude,"(?<=-).+(?=-N)"))/60)%>%
  mutate(LatitudeD = LatD+LatM)%>%
  mutate(LongD = as.numeric(str_sub(ship2$Longitude,1,3)))%>%
  mutate(LongM = as.numeric(str_extract(ship2$Longitude,"(?<=-).+(?=-W)"))/60)%>%
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

ship = st_as_sf(ship, coords = c("Longitude", "Latitude"), crs = 4326)

track = st_cast(ship$geometry, "MULTILINESTRING")

ggplot()+  geom_sf(data = ship, col= "orange", lty = 2)

#write track data

write_csv(ship, "data/shipTrack_2023.csv")
write_rds(ship, "data/shipTrack_2023.rds")

