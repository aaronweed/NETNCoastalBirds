#' @title GetAMOYData
#'
#' @importFrom plyr join
#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB and returns the raw AMOY survey data
#'
#' @param x Denote in parentheses to return df
#'
#' @details This function returns the raw AMOY survey data as a \code{data.frame}.
#'
#' @export

GetAMOYData<-function(x){
  
  con <- odbcConnect("NETNCB")
  
  ###################### Import data and lookup tables used for the query   ################
  #"tbl_Events","tbl_Group","tbl_Group_Observations" ,"tbl_Nests","tbl_Observations"      
  
  # import dataframes of each tables within the DB
  group <- sqlFetch(con, "tbl_Group")
  event <- sqlFetch(con, "tbl_Events")
  group_obs<-sqlFetch(con, "tbl_Group_Observations")
  
  odbcClose(con)
  
  ####### create new vectors to match field names for binding ########
  #names(obs)
  
  group$pk_EventID<-group$fk_EventID
  group_obs$newID<-group_obs$fk_GroupID
  group$newID<-group$pk_GroupID
  
  #############Join toegther various dataframes to create queries ##########
  
  ####### AMOY surveys #######
  # # returns df with the counts of AMOY  observed during different surveys per island, 
  # NOTE: When applicable this query retuns multiple records of species counts per island/date combination when species was spotted at more than one time.
  # will need to sum the counts per island, date, species, stage etc.  
  
  ## all events potentially have AMOY data
 
  
  ### I think I need to subset groups by AMOY, bind this to group obs, and then bind these to events
  
  amoy.grp<-group[group$Species_Code %in% "AMOY",]
  
  # add the group obs data 
  
  #intersect(names(amoy.grp),names(group_obs))
  
  amoy.grp2<-join(amoy.grp, group_obs, by ="newID")
  
  ### Subset AMOY obs from Incubation and Creche survey events 
  #intersect(names(event),names(group))
  
  event.amoy<-join(event, group, by ="pk_EventID")
  
  # subet df with AMOY observations
  # all of these observations are events where AMOY was looked for during incubation, nest, creche, or flyover (target) surveys
  temp.amoy<-event.amoy[event.amoy$Species_Code %in% "AMOY",]
  temp.amoy<-droplevels(temp.amoy)
  
  # add the group data to the df
  #intersect(names(temp.amoy),names(group_obs))
  
  temp.amoy2<-join(temp.amoy, group_obs, by ="newID")
  
  # check to see the number of AMOY obs per event
  #count(unique(temp.amoy2[,c("pk_EventID","Species_Code")]))
  
  # work with dates and time
  
  temp.amoy2$Date<-as.Date(temp.amoy2$Date, format= "%Y-%m-%d") #convert to date
  temp.amoy2$year<-as.factor(format(temp.amoy2$Date,"%Y")) #Create year variable
  temp.amoy2$month<-as.factor(format(temp.amoy2$Date,"%m")) #Create month variable
  
  # strip time out (this may need to be changed if number of digits varies)
  temp.amoy2$Start_Time<-substr(temp.amoy2$Start_Time,12,19)
  temp.amoy2$Group_Time<-substr(temp.amoy2$Group_Time,12,19)
  
  #names(temp.amoy2)
  ## subset df to final 
  AMOY_raw<-temp.amoy2[,c("Park", "Island","Segment", "Survey_Class" , "Survey_Type","Date","Start_Time", "year", "month", "Obs_Type","Species_Code" ,"Group_Name","Group_Time",
                          "Group_Repeat","Group_Notes", "Group_Coords", "Species_Unit","Unit_Count","Obs_Notes" ,"Wind_Direction","Wind_Speed","Air_Temp_F","Cloud_Perc","Tide_Stage")] 
  
  ### export to use in R viz
  #write.table(AMOY_raw, "./Data/AMOY_raw.csv", sep=",", row.names= FALSE)
  
  AMOY_raw
}