#This script imports xls/ csv field data output files collected during fieldwork 
# from fieldDataEntry matlab program into one table, formats GPS, 
# deduplicates records and renames variables
#Laura Feyrer 2024


#LIBRARIES------------
pacman::p_load(data.table, dplyr, readxl, purrr, readr, tidyr, stringr, sf, ggplot2, lubridate)

here::here()
#TRACK-------
#manually deleted first row to leave clean headers
ship2024 = read.csv(here::here("~/CODE/Arctic/input/2024/Time0_L12024.csv"), header = T, stringsAsFactors = F)

# ship_2024 = rbind(ship2a, ship2b)


ship = ship2024  %>% # fix lat long
  mutate(LatD = as.numeric(str_sub(ship2024$Latitude,1,2)))%>%
  mutate(LatM = as.numeric(str_extract(ship2024$Latitude,"(?<=-).+(?=-N)"))/60)%>%
  mutate(LatitudeD = LatD+LatM)%>%
  mutate(LongD = as.numeric(str_sub(ship2024$Longitude,1,3)))%>%
  mutate(LongM = as.numeric(str_extract(ship2024$Longitude,"(?<=-).+(?=-W)"))/60)%>%
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

#create utc var rounded to minute for merging
ship <- ship %>%
  mutate(timestamp = floor_date(DateT, unit = "minute"))

#spatial shapefile
ship_sf = st_as_sf(ship, coords = c("Longitude", "Latitude"), crs = 4326)%>%mutate(LINE_ID = c(rep(1:(nrow(ship)/2), each = 2)))
tail(ship_sf)

track = ship_sf%>%arrange(UTC1)%>%st_combine()%>%st_cast("LINESTRING")
# plot(st_geometry(track))

# ggplot()+  geom_sf(data = ship_sf, col= "orange", lty = 2)
# 
# #write track data
# 
write_csv(ship, "output/2024/shipTrack_2024.csv")
write_rds(ship, "output/2024/shipTrack_2024.rds")
write_sf(ship_sf, "shapes/shiptrackpts_2024.shp")
write_sf(track, "shapes/shiptrack_2024.shp")

# # # Read each file and write it to csv  
# function to process each xls file in the folder and move to new folder based on sheet name

process_folder <- function(folder_path, sheet, output_folder) {
  # Ensure output folder exists
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # List all Excel files in the folder
  files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE, recursive = T)
  
  # Loop over each file
  for (f in files) {
    try({
      # Read the specified sheet
      df <- read_xlsx(f, sheet = sheet)
      
      # Create the CSV file path
      csv_file <- gsub("xlsx$", "csv", f)
      
      # Write CSV in original folder
      write.csv(df, csv_file, row.names = FALSE)
      
      # Move the CSV to the output folder
      file.rename(csv_file, file.path(output_folder, basename(csv_file)))
    }, silent = TRUE)
  }
  
  cat("Processing and moving CSV files complete.\n")
}

# Usage example:
# process_folder("path/to/your/folder", "desired_sheet_name", "path/to/output/folder")


#CETACEANS------------
original_path <- paste(here::here(), "/input/2024/DATA SHEETS/", sep = "")
sheet = "Cetaceans"
output = paste(here::here(), "/input/2024/CSVS/Cetaceans", sep = "")
year = 2024
Trip = "Arctic_Leg1"

#process_folder so that all files are read even if there is no cetacean tab

process_folder(original_path, sheet, output)


# Compile and clean variables from multiple csv data----


all_tables <- list.files(path = output, pattern = "*.csv", full.names = T)
all_data <- all_tables%>%map(~ read_csv(.x, col_types = cols(.default = col_character())))%>%bind_rows()

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
                TimeEnd = `END encounter`, Pic_no, Comments)

#create whales data-----
        Whales2024 <- Sharedata %>%
          mutate(
            # Step 1: Parse Enc_start and Enc_end as UTC to avoid DST adjustments
            Enc_start = ymd_hms(DateT, tz = "UTC"),
            Enc_end = ymd_hms(TimeEnd, tz = "UTC"),
            
            # Step 2: Replace specific date (2024-08-17) with 2024-10-17 while keeping the time
            Enc_start = if_else(
              date(Enc_start) == ymd("2024-08-17"),
              ymd_hms(paste("2024-10-17", format(Enc_start, "%H:%M:%S")), tz = "UTC"),
              Enc_start
            ),
            Enc_end = if_else(
              date(Enc_end) == ymd("2024-08-17"),
              ymd_hms(paste("2024-10-17", format(Enc_end, "%H:%M:%S")), tz = "UTC"),
              Enc_end
            ),
            
            # Convert to character format for UTC and calculate time difference in minutes
            Enc_time = as.numeric(difftime(Enc_end, Enc_start, units = "mins")),
            
            # Step 3: Apply a manual offset based on date
            # Before October 27, Nuuk was at UTC-2 (daylight saving time)
            # After October 27, Nuuk is at UTC-1 (permanent daylight saving policy)
            UTC_Enc_start = if_else(
              date(Enc_start) < ymd("2024-10-27"),
              Enc_start + hours(2),  # Add 2 hours for dates before October 27
              Enc_start + hours(1)   # Add 1 hour for dates on or after October 27
            ),
            UTC_Enc_end = if_else(
              date(Enc_end) < ymd("2024-10-27"),
              Enc_end + hours(2),    # Add 2 hours for dates before October 27
              Enc_end + hours(1)     # Add 1 hour for dates on or after October 27
            )
            
          )
#LOOKS like some issues with enc length based on end times being entered late


#to fill in blank Lat/Longs, prioritize ships gps log due to delays in tablet gps

Whales2024 <- Whales2024 %>%
  mutate(timestamp = floor_date(UTC_Enc_start, unit = "minute"))

# Join the ship coordinates to Whales2024 based on timestamp----
Whales2024 <- Whales2024 %>%
  left_join(ship %>% select(timestamp, Latitude, Longitude), by = "timestamp") %>%
  select(15:22,2, 5:14)%>%select(-timestamp, -TimeEnd)

#write csv
write.csv(Whales2024, file = paste0("output/2024/", sheet, Trip, year, ".csv"), row.names = FALSE)
write_rds(Whales2024, file = paste0("output/2024/", sheet, Trip, year, ".rds"))



#ENVIRONMENT----------------
#uses functions above

sheet = "Environment"
original_path <- paste(here::here(), "/input/2024/DATA SHEETS/", sep = "")
output = paste(here::here(), "/input/2024/CSVS/Enviro", sep = "")
year = 2024
Trip = "Arctic_Leg1"

process_folder(original_path, sheet, output)

# Compile and clean variables from multiple csv data----


all_tables <- list.files(path = output, pattern = "*.csv", full.names = T)
all_data <- all_tables %>%
  map(~ read_csv(.x, col_types = cols(.default = col_character()))) %>%
  bind_rows()

summary(all_data)

# create a tibble for cleaned up validated Environment data 
###
all_data<-all_data  %>% # pipe - create a linear sequence of operations 
  mutate(LatD = as.numeric(str_sub(all_data$StartPos,1,2)))%>%
  
  mutate(LatM = as.numeric(str_extract(all_data$StartPos,"(?<=d).+(?=N)"))/60)%>%
  mutate(Latitude = LatD+LatM)%>%
  mutate(LongD = as.numeric(str_extract(all_data$StartPos,"(?<=\\s).+(?=d)")))%>%
  mutate(LongM = as.numeric(str_extract(all_data$StartPos,"[\\d\\.]+(?=W)"))/60)%>%
  mutate(Longitude = (LongD+LongM)*-1)

#deduplicate
all_data = all_data[!duplicated(all_data$DateT), ]

Sharedata = all_data%>%dplyr::select(-StartPos, -LatD,-LatM, -LongD, - LongM,)

#create env data-----
        EnvData = Sharedata %>%
          mutate(
        # Step 1: Parse Enc_start and Enc_end as UTC to avoid DST adjustments
            # Check if DateT has characters typical of a standard date
            DateT = sub("^E", "", DateT),  # Remove "E" at the start
            Env_start = case_when(
              # Check for the "DD/MM/YY HH:MM" format using "/"
              grepl("^\\d{2}/\\d{2}/\\d{2} \\d{2}:\\d{2}$", DateT) ~ dmy_hm(DateT, tz = "UTC"),
              # Check for a standard date-time format with "-" or ":"
              grepl("[-:]", DateT) ~ ymd_hms(DateT, tz = "UTC"),
              
              
              
              # Treat as an Excel serial date if no other indicators are found
              TRUE ~ as.POSIXct(as.numeric(DateT) * 86400, origin = "1899-12-30", tz = "UTC")
            ),
                 
        # Remove the "E" prefix and parse the remaining date-time string
        TimeEnd = sub("^E/", "", TimeEnd),  # Remove "E/" at the start
        TimeEnd = sub("^E", "", TimeEnd),  # Remove "E" at the start
        # Step 1: Keep only the first timestamp by removing everything after the second whitespace
        TimeEnd = sub("^((?:\\S+\\s+\\S+)).*", "\\1", TimeEnd),
        
        Env_end = case_when(
          grepl("[-:]", TimeEnd) ~ dmy_hm(TimeEnd, tz = "UTC"),
          grepl("^\\d{2}/\\d{2}/\\d{2} \\d{2}:\\d{2}$", TimeEnd) ~ dmy_hm(TimeEnd, tz = "UTC"),
          
          # If TimeEnd is a decimal (0 < TimeEnd < 1), extract date from Enc_start and add time portion
          as.numeric(TimeEnd) > 0 & as.numeric(TimeEnd) < 1 ~ 
            as.POSIXct(as.Date(Env_start)) + as.numeric(TimeEnd) * 86400,
          
          # Otherwise, treat as Excel serial date
          TRUE ~ as.POSIXct(as.numeric(TimeEnd) * 86400, origin = "1899-12-30", tz = "UTC")
        ),

        # Step 2: Replace specific date (2024-08-17) with 2024-10-17 while keeping the time
        Env_start = if_else(
          date(Env_start) == ymd("2024-08-17"),
          ymd_hms(paste("2024-10-17", format(Env_start, "%H:%M:%S")), tz = "UTC"),
          Env_start
        ),
        Env_end = if_else(
          date(Env_end) == ymd("2024-08-17"),
          ymd_hms(paste("2024-10-17", format(Env_end, "%H:%M:%S")), tz = "UTC"),
          Env_end
        ),
        
        # Convert to character format for UTC and calculate time difference in minutes
        Env_time = as.numeric(difftime(Env_end, Env_start, units = "mins")),
        
        # Step 3: Apply a manual offset based on date
        # Before October 27, Nuuk was at UTC-2 (daylight saving time)
        # After October 27, Nuuk is at UTC-1 (permanent daylight saving policy)
        UTC_Env_start = if_else(
          date(Env_start) < ymd("2024-10-27"),
          Env_start + hours(2),  # Add 2 hours for dates before October 27
          Env_start + hours(1)   # Add 1 hour for dates on or after October 27
        ),
        UTC_Env_end = if_else(
          date(Env_end) < ymd("2024-10-27"),
          Env_end + hours(2),    # Add 2 hours for dates before October 27
          Env_end + hours(1)     # Add 1 hour for dates on or after October 27
        )                )

        
        
        # Join the ship coordinates to Environment based on timestamp-----
        EnvData <- EnvData %>%
           mutate(timestamp = floor_date(UTC_Env_start, unit = "minute"))

        EnvData <- EnvData %>%
          left_join(ship %>% select(timestamp, Latitude, Longitude), by = "timestamp")%>%
          select(22:29, 3:15,20:21 )%>%select(-timestamp, TimeEnd)
        
        
        
        #write csv
write.csv(EnvData, file = paste0("output/2024/", sheet, Trip, year, ".csv"), row.names = FALSE)
write_rds(EnvData, file = paste0("output/2024/", sheet, Trip, year, ".rds"))

