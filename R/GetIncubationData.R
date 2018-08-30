#' @title GetIncubationData
#'
#' @importFrom plyr join
#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB and returns the boat-based incubation data. 
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param x Denote in parentheses to return all boat-based incubation surveys.
#'
#' @return This function returns the raw boat-based incubation survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#'@examples 
#' GetIncubationData(x)
#' @export

### This script connects to the backend of NETN's Coastal Bird Access DB and returns the raw incubation survey data 

## ODBC BE file is: NETN_CoastalBirds_BE_20180309.accdb

#https://science.nature.nps.gov/im/units/netn/monitor/vitalSigns/birds/coastalBirds.cfm for further details 

## Version 0.0.1 Aaron Weed 03/06/2018

GetIncubationData<-function(x){
  # Connect to database BE 
  
  con <- odbcConnect("NETNCB")
  
  ###################### Import data and lookup tables used for the query   ################
  #"tbl_Events","tbl_Group_Observations"    
  
  # import dataframes of each tables within the DB
  
  event <- sqlFetch(con, "tbl_Events"); names(event)
 
  obs<-sqlFetch(con, "tbl_Observations");names(obs)
  
  
  odbcClose(con)
  
  ####### create new vectors to match field names for binding ########
  names(obs)
  obs$pk_EventID<-obs$fk_EventID 
  
  
  #############Join toegther various dataframes to create queries ##########
  
  ##### Boat-based Incubation surveys for nests and adults of target species groups  ###########
  # COTE, DCCO,  GBBG, HERG, LETE, SPSA, WILL 
  # returns df with the counts of adults and nests of target species observed during incubation surveys per island, segment, date and  species.
  # NOTE: When applicable this query retuns muultiple records of species counts per island/date combination when species was spotted at more than one time.
  # will need to sum the counts per island, date, species etc.
  
  # Add obs data to  incubation event data
  intersect(names(event),names(obs))
  
  temp.incub<-join(event,obs,  by="pk_EventID")
  #View(temp.incub)
  
  ## remove incidental obs
  temp.incub2<-temp.incub[temp.incub$Obs_Type %in% "Target",]
  
  temp.incub2<-droplevels(temp.incub2)# clean up factor levels
  #View(temp.incub2)
  
  # work with dates and time
  
  temp.incub2$Date<-as.Date(temp.incub2$Date, format= "%Y-%m-%d") #convert to date
  temp.incub2$year<-as.factor(format(temp.incub2$Date,"%Y")) #Create year variable
  temp.incub2$month<-as.factor(format(temp.incub2$Date,"%m")) #Create month variable
  
  # strip time out (this may need to be changed if number of digits varies)
  temp.incub2$Start_Time<-substr(temp.incub2$Start_Time,12,19)
  
  ## subset df to final columns
  #names(temp.incub2)
  incubation_raw<-temp.incub2[,c("Park","Island","Segment", "Survey_Class","Survey_Type","Obs_Type","Date" ,"year", "month","Start_Time" ,"Species_Code","Species_Unit", "Unit_Count", 
                                 
                                 "Obs_Notes" , "Recorder", "c_Observer","Data_Source", "Wind_Direction","Wind_Speed","Air_Temp_F","Cloud_Perc","Tide_Stage")]  
  # sort df
  incubation_raw<-incubation_raw[order(incubation_raw$Island,incubation_raw$Segment,incubation_raw$Date, incubation_raw$Species_Code),]
  
    ### export to use in R viz
  #write.table(incubation_raw, "./Data/incubation_raw.csv", sep=",", row.names= FALSE)
  
  incubation_raw
   
}