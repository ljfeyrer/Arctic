#clean Listview export from NBW dorsal catalogue and format for socprog 
    #results in sightHa2 data table 
#this can be run from top to bottom


#libraries------------
library("dplyr")
library(stringr)
library(tidyr)
library(readr)

#read in List view csv file
sightHa2 = read.csv(here::here("data/LV_2022.csv"))



#clean variables--------
    sightHa2$Date1 = sightHa2$Date.Original
    
    sightHa2$keyword=as.character(sightHa2$Keyword.export)

#clean date format
    
    #clean date format-----
    sightHa2$Date1 =  as.character(sightHa2$Date1)
    sightHa2$Date =  as.Date(sightHa2$Date1, "%m/%d/%Y")
    summary(sightHa2$Date)
    sightHa2 = sightHa2%>%mutate(YEAR = as.numeric(format(Date, "%Y")), Time = as.POSIXct(Date1,  tz = "UTC", format = "%m/%d/%Y %H:%M"))
    
  #recode SIDE------
  
  sightHa2=sightHa2%>%mutate(side = ifelse(grepl("Left", keyword), "Left", 
                                     ifelse(grepl("Right", keyword), "Right","UNK")))
 # side =  sightHa2[sightHa2$side =="UNK",]   

 
#create qrate variable from *------


sightHa2=sightHa2%>%mutate(QRATE = ifelse(grepl("\\* \\* \\* \\*", Rating), "4", 
                                    ifelse(grepl("\\* \\* \\*", Rating),"3",
                                           ifelse(grepl("\\* \\*", Rating),"2", 
                                                  ifelse(grepl("\\*", Rating),"1","??")))))

# sightHa2[sightHa2$QRATE =="??",]   
# sightHa2$QRATE = as.numeric(sightHa2$QRATE)

#deduplicate based on time

sightHa2=sightHa2[!duplicated(sightHa2$Time), ]

#filter out no GPS
sightHa2 = sightHa2%>%filter(!is.na(Latitude))

#make simple version for merging
sightHa = sightHa2%>%dplyr::select(File.name, QRATE,Date,Longitude, Latitude,
                              )
write_csv(sightHa,  here::here("data/sightHa_location.csv"))
write_rds(sightHa, "data/sightHa_location.rds")


####OTHER THINGS DOWN HERE####
    # Reliable-----
    sightHa2$Reliable =NA
    sightHa2=sightHa2%>%mutate(Reliable = ifelse(grepl("Indent", keyword), "Yes", ifelse(grepl("Notch", keyword), "Yes",
                                          "No")))
    
    
    # #clean ID
    # sightHa2$Title =as.character(sightHa2$Title)
    # sightHa2=sightHa2%>%mutate(ID = ifelse(grepl("unk", Title), NA, 
    #                                  ifelse(grepl("see crops", Title), NA,
    #                                         ifelse(grepl("56R", Title),"56", Title))))
    # 
    # summary(as.factor(sightHa2$ID))
    # sightHa2 <- sightHa2[!is.na(sightHa2$ID),]
    
    # # location-----
    # sightHa2=sightHa2%>%mutate(Location = ifelse(grepl("Gully", keyword), "Gully", 
    #                                        ifelse(grepl("Haldimand", keyword), "Haldimand",
    #                                               ifelse(grepl("Shortland", keyword),"Shortland", "UNK"))))
    
   #  sightHa2=sightHa2%>%mutate(Location = ifelse(Location == "UNK" & Latitude >= 44, "Shortland", ifelse(Location == "UNK" & Latitude <= 44, "Gully", Location)))
   #  # sightHa2=sightHa2%>%mutate(Location = ifelse(is.na(Location) & Date == "2019-07-27", "Gully", ifelse(is.na(Location)  & Date == "2019-08-06", "Gully", Location)))
   #  
   # sightHa2[sightHa2$Location =="UNK",]   

   #biopsy----
   # sightHa2=sightHa2%>%mutate(Biopsy = ifelse(grepl("biopsy", keyword), "YES", NA))
   # sightHa2 = sightHa2%>%group_by(ID)%>%fill(Biopsy)
   
   
    #sex code-------
   
   # ALL M/FJ categories-----
   
    # sightHa2=sightHa2%>%mutate( Sex = ifelse(grepl("Female,", keyword), "FemaleJ", 
    #                                     ifelse(grepl("Male,", keyword), "MaleM",
    #                                            ifelse(grepl("MM,", keyword), "MaleM",
    #                                                   ifelse(grepl("FJ", keyword), "FemaleJ",
    #                                                          
    #                                                          
    #                                             NA)))))
    # # Total IDs
    # Total_ID = sightHa2%>%group_by(ID, Sex)%>%summarise(N = n())
    
     
            
            # #create clean ID table for matching------
            # sightHa2 = sightHa2 %>% ungroup()%>%
            #   group_by(ID, side) %>%mutate(side1 = ifelse(side == "Right", "RIGHT", 
            #                                               ifelse(side == "Left", "LEFT","ack")),
            #                                ID.side = paste0(ID,  sep = "-", side1))%>%ungroup()%>%
            #   mutate(Sex1 = ifelse(is.na(Sex1), "UNK", Sex1))
            # 
            # 
            # Id_day = sightHa2 %>% filter(QRATE>2)%>%
            #   group_by(Date)%>%summarise()
            #  
            # 
            # Id_Year = sightHa2 %>% 
            #   group_by(ID, side)%>%
            #   mutate(FirstDate = min(Date), LastDate = max(Date)) 
            
            #make year
            Id_Year = Id_Year%>%mutate(YEAR1 = as.numeric(format(FirstDate, "%Y")), 
                                       YEARLAST = as.numeric(format(LastDate, "%Y")))
            Id_Year = as.data.frame(Id_Year%>%ungroup%>%select(ID, ID.side, side, Sex, Sex1, QRATE, Reliable, keyword, YEAR1, YEARLAST))
           
             #one photo from each year
            Id_Year = unique(Id_Year)

            
            #make animal years
            Id_Year = Id_Year%>%mutate(ANIMAL_YRS = YEARLAST-YEAR1)
            
            #add back animal years to phot based on ID links
            sightHa22 = left_join(sightHa2, Id_Year)
            
            
            
            #creat an master ID - summary table
            Id_Year2 =Id_Year%>%group_by(ID, ID.side, Sex, Sex1, YEAR1, YEARLAST, ANIMAL_YRS)%>%summarise(N = n())%>%ungroup()%>%mutate(ID = as.numeric(ID))

            write_csv(Id_Year2, here::here( "Photo_ID/catalogue_files/ID_SEX_MASTER.csv"))
            
            #make simple version for merging
            sightHa21 = sightHa2%>%select(ID.side, QRATE,Date,Location,
                                    Reliable,Sex, ID )
      
            
       
            
            

           