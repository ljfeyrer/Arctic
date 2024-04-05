#2023 data-----
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(readr)
library(lubridate)


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
# plot(st_geometry(track))

ggplot()+  geom_sf(data = ship%>%filter(Date == "4/11/2023"), col= "orange", lty = 2)
# 
# #write track data
# 
# # write_csv(ship, "data/shipTrack_2023.csv")
# # write_rds(ship, "data/shipTrack_2023.rds")
# write_sf(ship, "shapes/shiptrackpts_2023.shp")
# write_sf(track, "shapes/shiptrack_2023.shp")


#cetaceans------
Whales2023 = read.csv("data/2023/ArcticNBW2023_Cetaceans.csv")%>%mutate(X = NULL)


Whales2023 = Whales2023%>% mutate(Enc_start = as.POSIXct(Local_Time, "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-2"),
                            Enc_end = as.POSIXct(Local_Time_End, "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-2" ), 
                            UTC = as.POSIXct(Enc_start, tz = "UTC"), 
                            Enc_time = difftime(Enc_end,Enc_start, units = "mins"))

#only Leg 1
#fixed UTC time variable here
write.csv(Whales2023%>%select(UTC, Date_Local = Enc_start, Latitude, Longitude, Enc_mins = Enc_time, Min, Best, Max, Dist, 
                              Photo = Pic_no, Species, Behaviour), row.names = F, "output/ArcticWhales_2023.csv")


#LV------
#read in List view csv file for Leg 2
sightHa_2023 = read.csv("data/2023/List View Leg2_2023.csv", colClasses = "character")


#clean date format-----
sightHa_2023$Date =  as.Date(sightHa_2023$Date.Original, "%m/%d/%Y")
summary(sightHa_2023$Date)
sightHa_2023 = sightHa_2023%>%mutate(YEAR = as.numeric(format(Date, "%Y")),  
                                     Time = as.POSIXct(Date.Created,   format = "%Y %H:%M:%S", tz = "Etc/GMT-2"), 
                                     UTC = as.POSIXct(Time, tz = "UTC"))


#deduplicate based on enc time

sightHa_2023=sightHa_2023[!duplicated(sightHa_2023$Time), ]
sightHa_2023 = sightHa_2023%>%mutate(enc_time = round_date(Time, unit="15 mins"), )
sightHa_2023=sightHa_2023[!duplicated(sightHa_2023$enc_time), ]

#remove erroneous pic from the harbour on Nov 4
sightHa_2023 = sightHa_2023%>%filter( Date != "2023-11-04")


#make simple version for manual editing
sightHa = sightHa_2023%>%dplyr::select(Date,Time,UTC, Longitude, Latitude,
)

# one station was missing lat/ long from field notes
# this was taken from station info below 
# notes were added manually and saved as xls
# manual edits to output> ArcticNBW2023_Cetaceans.xlsx to add leg 1 +2 and create report table
# the combined data with all fields was saved as a csv in output> Arctic2023_AllCetaceans.csv


#Set infos

stations = read.csv("data/2023/2023NAFO_set_coords_LFeyrer.csv")

#missing lat longs for NBW sighting based on field notes
stations%>%filter(Start.Date == "10/11/2023", Set.Number == 118)
%>%dplyr::select(Latitude, Longitude)

sightHa = sightHa%>%mutate( Longitude = ifelse( Date == "2023-11-10", "-58.721", Longitude),
                                                Latitude = ifelse( Date == "2023-11-10", "64.118", Latitude))

write_csv(sightHa%>%select(Date, Local_Time = Time, UTC, Longitude, Latitude),  "output/sightHa_LV2023.csv")
