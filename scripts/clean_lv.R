#clean Listview export from NBW dorsal catalogue and format for socprog 
    #results in sightHa2 input table 
#this can be run from top to bottom


#libraries------------
library("dplyr")
library(stringr)
library(tidyr)
library(readr)

#read in List view csv file
sightHa2 = read.csv(here::here("input/LV_2022.csv"))



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
write_csv(sightHa,  here::here("input/sightHa_location.csv"))
write_rds(sightHa, "input/sightHa_location.rds")


      
            
       
            
            

           