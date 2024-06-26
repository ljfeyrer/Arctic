#This script imports xls/ csv field data output files collected during fieldwork 
# from fieldDataEntry matlab program into one table, formats GPS, 
# deduplicates records and renames variables
#Laura Feyrer 2024


#LIBRARIES------------
pacman::p_load(data.table, dplyr, readxl, purrr, readr, tidyr, stringr)

here::here()

# # # Read each file and write it to csv  
#function to translate xlsx to csv, need to specify sheet
csv_maker =function(f) {
  df = read_xlsx(f, sheet = sheet)
  write.csv(df, gsub("xlsx", "csv", f), row.names=FALSE) }

#function to move csvs to new folder in input based on sheet name
move.files <- function(x){
  file.rename( from = file.path(valid_path, x) ,
               to = file.path(paste(here::here(), "/input/",sheet, sep = ""), x) )
}    

#CETACEANS------------
    original_path <- (here::here("input/originals"))
    sheet = "Cetaceans"
    year = 2022
    Trip = "Arctic_Leg1"
 
#read in data files
     files.to.read = list.files(original_path, pattern="xlsx", full.names = T)
#write csvs to same folder
    lapply(files.to.read, csv_maker)
     
#Move csvs to new folder
     files.csv= list.files(original_path, pattern="csv")
     lapply(files.csv, move.files)
    
# Compile and clean variables from multiple csv data----
     
     csv_path <- file.path(paste(here::here(), "/input/",sheet, sep = ""))
     
    all_tables <- list.files(path = csv_path, pattern = "*.csv", full.names = T)
    all_data <- all_tables%>%map(read_csv)%>%bind_rows()
    
    summary(all_data)
    
    # create a tibble for cleaned up validated data 
    #note this will warn of NAs for observations without coordinates
    all_data<-all_data  %>% # pipe - create a sequence of data cleaning operations 
      mutate(LatD = as.numeric(str_sub(all_data$StartPos,1,2)))%>%
      
    mutate(LatM = as.numeric(str_extract(all_data$StartPos,"(?<=d).+(?=N)"))/60)%>%
      mutate(Latitude = LatD+LatM)%>%
      mutate(LongD = as.numeric(str_extract(all_data$StartPos,"(?<=\\s).+(?=d)")))%>%
      mutate(LongM = as.numeric(str_extract(all_data$StartPos,"[\\d\\.]+(?=W)"))/60)%>%
      mutate(Longitude = (LongD+LongM)*-1)%>%
      mutate(Port_Star = PS)%>%  # change field name to something that is clear
        mutate(Species = ifelse(Species == "nbw", "Northern Bottlenose",
                                ifelse(Species == "northen bottlenose", "Northern Bottlenose",
                                       Species)))
    #deduplicate
    all_data = all_data[!duplicated(all_data$DateT), ]
    
    
    #clean working variables out for writing output
    Sharedata = all_data%>%
      dplyr::select(DateT, Species, Latitude, Longitude,Min, Best, Max, Dist, Bearing, Port_Star, Behaviour, 
                    TimeEnd, Pic_no, Comments)
        
    
    #write csv
    write.csv(Sharedata, file = paste0("output/", sheet, Trip, year, ".csv"), row.names = FALSE)
    write_rds(Sharedata, file = paste0("output/", sheet, Trip, year, ".rds"))
    


#ENVIRONMENT----------------
    #uses csv_maker and move.files functions above

    sheet = "Environment"
    original_path <- (here::here("input/originals"))
    year = 2022
    Trip = "Arctic_Leg1"
    
    #read in data files
    files.to.read = list.files(original_path, pattern="xlsx", full.names = T)
    #write csvs to same folder
    lapply(files.to.read, csv_maker)
    
    #Move csvs to new folder
    files.csv= list.files(original_path, pattern="csv")
    lapply(files.csv, move.files)
    
    # Compile and clean variables from multiple csv data----
    
    csv_path <- file.path(paste(here::here(), "/input/",sheet, sep = ""))
    
    all_tables <- list.files(path = csv_path, pattern = "*.csv", full.names = T)
    all_data <- all_tables%>%map(read_csv)%>%bind_rows()
    
    summary(all_data)
    
     # create a tibble for cleaned up validated Environment data 
    ###
    all_data<-all_data  %>% # pipe - create a linear sequence of operations 
      mutate(LatD = as.numeric(str_sub(all_data$Pos,1,2)))%>%
      
      mutate(LatM = as.numeric(str_extract(all_data$Pos,"(?<=d).+(?=N)"))/60)%>%
      mutate(Latitude = LatD+LatM)%>%
      mutate(LongD = as.numeric(str_extract(all_data$Pos,"(?<=\\s).+(?=d)")))%>%
      mutate(LongM = as.numeric(str_extract(all_data$Pos,"[\\d\\.]+(?=W)"))/60)%>%
      mutate(Longitude = (LongD+LongM)*-1)
    
    #deduplicate
    all_data = all_data[!duplicated(all_data$DateT), ]
    
    Sharedata = all_data%>%dplyr::select(-Pos, -LatD,-LatM, -LongD, - LongM,)
    
    #write csv
    write.csv(Sharedata, file = paste0("output/", sheet, Trip, year, ".csv"), row.names = FALSE)
    write_rds(Sharedata, file = paste0("output/", sheet, Trip, year, ".rds"))

#### import ship track ####
    cleantrack = function(ship1){
      ship1 =  ship1%>%dplyr::select(Latitude,Longitude, Year , Month, Day, Hour, Minute, Second, Depth)%>%
        mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day), Hour = as.numeric(Hour), 
               Minute = as.numeric(Minute),
               Second = as.numeric(Second), Depth = as.numeric(Depth))%>%filter(!is.na(Year))
      
      ship = ship1  %>% # fix lat long
        mutate(LatD = as.numeric(str_sub(ship1$Latitude,1,2)))%>%
        mutate(LatM = as.numeric(str_extract(ship1$Latitude,"(?<=-).+(?=-N)"))/60)%>%
        mutate(LatitudeD = LatD+LatM)%>%
        mutate(LongD = as.numeric(str_sub(ship1$Longitude,1,3)))%>%
        mutate(LongM = as.numeric(str_extract(ship1$Longitude,"(?<=-).+(?=-W)"))/60)%>%
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
      
      # add variable for daylight
      sunrise = "9:00 am"
      sunset = "4:00 pm"
      ship= ship%>% 
        mutate(TIME = data.table::as.ITime(as.POSIXct(DateT, format = '%I:%M %p')), 
               sunrise = as.ITime(as.POSIXct(sunrise, format = '%I:%M %p')), 
               sunset = as.POSIXct(sunset, format = '%I:%M %p'), 
               daylight = as.character(as.integer(TIME >= sunrise & TIME <= sunset)))
      
    }
    
    #trip 1
    headers = read.csv(here::here("data/GPS/TOGT8LAURA.csv"), skip = 1, header = F, nrows = 1, as.is = T, stringsAsFactors = F)
    ship1 = read.csv(here::here("data/GPS/TOGT8LAURA.csv"), skip = 2, header = F, stringsAsFactors = F)
    colnames(ship1)= headers
    
   ship1 = cleantrack(ship1) 
    
    #trip 2
    ship2 = read.csv(here::here("data/GPS/Trip2/TOGT8LAURA01111111.csv"), skip = 2, header = F, stringsAsFactors = F)
    colnames(ship2)= headers
    
    ship2 = cleantrack(ship2) 
    
    # Trip3
    ship3 = read.csv(here::here("data/GPS/Trip2/TOGT915112611.csv"), skip = 2, header = F, stringsAsFactors = F)
    colnames(ship3)= headers
    
    ship3 = cleantrack(ship3) 
    
    #merge all cleaned track files and deduplicate
    
    track_2022 = rbind(ship1, ship2, ship3)
    # deduplicate based on minute
    track_2022=track_2022[!duplicated(track_2022$UTC1), ]
    
#create a shapefile for trips---------
    ship = st_as_sf(track_2022, coords = c("Longitude", "Latitude"), crs = 4326)
    write_sf(ship, "shapes/shipstrack_pts.shp")
#used ArcGiS to change this pt file to a line shapefile :()

#write track data
    
    write_csv(track_2022, "data/shipTrack.csv")
    write_rds(track_2022, "data/shipTrack.rds")
    