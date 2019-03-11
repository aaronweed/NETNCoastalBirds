#' @title Return creche surveys from database
#'
#' @importFrom plyr join
#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' and returns the raw creche survey data of COEI
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager. 
#' (If ODBC_connect = TRUE).
#' @param x Denote in parentheses to return df
#' @param ODBC_connect Should the function connect to the Access DB? The default (TRUE) is to 
#' try to connect using the Windows ODBC manager. If the connection is not available or not desired, 
#' the function can return the saved data from the package. 
#' Note the saved data may not be up-to-date.
#' @param export Should the incubation data be exported as a csv file and RData object?
#' (This argument is used to regenerate the RData for the package.)
#'
#' @details This function returns the raw AMOY survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples 
#' creche <- GetCrecheData(x)

#' @export


GetCrecheData <- function(x, ODBC_connect = TRUE, export= FALSE) {
  # Connect to database BE 
  if  (ODBC_connect == TRUE) {
  con <- odbcConnect("NETNCB")
  
  ###################### Import data and lookup tables used for the query   ################
  #"tbl_Events","tbl_Group","tbl_Group_Observations" ,"tbl_Nests","tbl_Observations"      
  
  # import dataframes of each tables within the DB
  group <- sqlFetch(con, "tbl_Group")
  event <- sqlFetch(con, "tbl_Events")
  group_obs<-sqlFetch(con, "tbl_Group_Observations")
  
  odbcClose(con)
  
  ####### create new vectors to match for joining ########
  #names(obs)
  
  group$pk_EventID<-group$fk_EventID
  group_obs$GroupID<-group_obs$fk_GroupID
  group$GroupID<-group$pk_GroupID
  
  
  #############Join toegther various dataframes to create queries ##########
  
  ##### Boat-based Creche surveys (COEI) ###########
  # returns df with the counts of COEI adults and ducklings observed during creche surveys per island,  Excludes incidental obs.
  # NOTE: When applicable this query retuns muultiple records of species counts per island/date combination when species was spotted at more than one time.
  
  # NOTE: As of 9/6/2017 query is returning NA's for some records under Unit_Count. This shoulnd't happen but may be occuring b/c recorder fails to enter in the group data
  # For now I have excluded these records 
  
  # will need to sum the counts per island, date, species, stage etc.  
  
  ## extract Creche surveys from "events"
  event.crec<-event[event$Survey_Type %in% "Creche",]
  
  event.crec<-droplevels(event.crec)
  
  ## extract COEI surveys from "groups" so that AMOY obs don't get pulled in
  group.crec<-group[group$Species_Code %in% "COEI",]
  
  group.crec<-droplevels(group.crec)
  
  
  # Add subsetted group data to subsetted creche event data (event.crec)
  #intersect(names(event.crec),names(group.crec))
  
  temp.crec<-join( event.crec,group.crec,  by="pk_EventID")
  #names(temp.crec)
  
  #count(unique(group_obs[,c("GroupID","Species_Unit")]))
  
  # add in group obs data for each event/group of COEI
  #intersect(names(temp.crec),names(group_obs))
  
  temp.crec2<-join( temp.crec,group_obs,  by="GroupID")
  #names(temp.crec2)
  
  
  # work with dates and time
  
  temp.crec2$Date<-as.Date(temp.crec2$Date, format= "%Y-%m-%d") #convert to date
  temp.crec2$year<-as.factor(format(temp.crec2$Date,"%Y")) #Create year variable
  temp.crec2$month<-as.factor(format(temp.crec2$Date,"%m")) #Create month variable
  
  # strip time out (this may need to be changed if number of digits varies)
  temp.crec2$Start_Time<-substr(temp.crec2$Start_Time,12,19)
  temp.crec2$Group_Time<-substr(temp.crec2$Group_Time,12,19)
  
  #names(temp.crec2)
 
  
  ## subset df to final 
  creche_raw<-temp.crec2[,c("Park", "Island","Segment", "Survey_Class" , "Survey_Type","Date","Start_Time", "year", "month", "Survey_MultiPart" , "Survey_Duplicate" ,
                            "Survey_Primary","Survey_Complete" ,"Obs_Type", "Recorder", "Observer","Species_Code" ,"Group_Count","Group_Time",
                            "Group_Repeat","Group_Notes", "Group_Coords", "Species_Unit","Unit_Count","Obs_Notes" ,"Wind_Direction",
                            "Wind_Speed","Air_Temp_F","Cloud_Perc","Tide_Stage")] 
  
  
  
  ## Get rid of blank group obs (Unit_Count == NA)
  
  creche_raw <- creche_raw[!is.na(creche_raw$Unit_Count),]
  
  ### export to use in R viz
  #write.table(creche_raw, "./Data/creche_raw.csv", sep=",", row.names= FALSE)
  if (export == TRUE) {
    write.table(creche_raw, "Data/creche_raw.csv", sep=",", row.names= FALSE)
    save(creche_raw, file = "Data/creche_raw.RData")
  }
  
  
  }
  if (ODBC_connect == FALSE) {
    data(creche_raw)
  }
  
  creche_raw
}