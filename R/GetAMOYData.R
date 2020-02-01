#' @title Return in-season AMOY surveys from database
#'
#' @importFrom dplyr select left_join
#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#' @importFrom lubridate ymd year month date
#' @importFrom Hmisc mdb.get
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' (Access backend entered as 'NETNCB' in Windows ODBC manager) and returns the raw AMOY 
#' survey data. f the Access DB is not
#' accessible from the ODBC connection, one can try to connect via Hmisc, or
#' the function returns a saved image of the data.
#' 
#' @param connect Should the function connect to the Access DB? The default 
#' (\code{connect = `ODBC`}) is to try to connect using the Windows ODBC manager. 
#' If the connection is not available or not desired, one can use \code{connect = `Hmisc`}
#' and include a patch to a saved version of the database, or
#' the function can return the saved data from the package (\code{connect = `No`}). 
#' Note the saved data may not be up-to-date.
#' @param export Should the incubation data be exported as a csv file and RData object?
#' (This argument is used to regenerate the RData for the package.)
#' 
#' @return This function returns the raw AMOY survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' amoy <- GetAMOYdata()
#' @export

GetAMOYData <- function(connect = "ODBC", DBfile = NULL, export = FALSE){
  ## First, get the data depending on the connection option:
  if (connect == "ODBC") {
     con <- odbcConnect("NETNCB")
  
  ###################### Import data and lookup tables used for the query   ################
  #"tbl_Events","tbl_Group","tbl_Group_Observations" ,"tbl_Nests","tbl_Observations"      
  
  # import dataframes of each tables within the DB
  group <- sqlFetch(con, "tbl_Group")
  event <- sqlFetch(con, "tbl_Events")
  group_obs<-sqlFetch(con, "tbl_Group_Observations")
  
  odbcClose(con)
  
  ## If connection didn't work, try mdb.get() 
  } else if (connect == "Hmisc") {
    if (is.null(DBfile)) {
      stop("Please specify the database location for this connection option.")
    }
    group <- mdb.get(db_path, tables="tbl_Group", 
                     mdbexportArgs = '', stringsAsFactors = FALSE)
    event <- mdb.get(DBfile, tables = "tbl_Events", 
                     mdbexportArgs = '', stringsAsFactors = FALSE)
    group_obs <- mdb.get(db_path, tables="tbl_Group_Observations", 
                         mdbexportArgs = '', stringsAsFactors = FALSE)
    group <- clear.labels(group)
    event <- clear.labels(event)
    group_obs <- clear.labels(group_obs)
    
    ## The names are imported differently using mdb.get().
    ## Replace "." with "_"
    names(group) <- gsub("\\.", "_", names(group))
    names(event) <- gsub("\\.", "_", names(event))
    names(group_obs) <- gsub("\\.", "_", names(group_obs))
    
  } else if (connect == "No") {
    return(data(nest_surveys_raw))
  } else {
    stop("connect must be ODBC, Hmisc, or No.")
  }
  
  ## Keep organizing data from DB:
  
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
  
  # amoy.grp2<-join(amoy.grp, group_obs, by ="newID")
  # 
  # ### Subset AMOY obs from Incubation and Creche survey events 
  # #intersect(names(event),names(group))
  # 
  # event.amoy<-join(event, group, by ="pk_EventID")
  
  amoy.grp2 <- left_join(amoy.grp, group_obs, by = "newID")
  event.amoy <- left_join(event, group, by ="pk_EventID")

  
  # subet df with AMOY observations
  # all of these observations are events where AMOY was looked for during incubation, nest, creche, or flyover (target) surveys
  temp.amoy<-event.amoy[event.amoy$Species_Code %in% "AMOY",]
  temp.amoy<-droplevels(temp.amoy)
  
  # add the group data to the df
  #intersect(names(temp.amoy),names(group_obs))
  
  # temp.amoy2<-join(temp.amoy, group_obs, by ="newID")
  temp.amoy2 <- left_join(temp.amoy, group_obs, by ="newID")
  
  # check to see the number of AMOY obs per event
  #count(unique(temp.amoy2[,c("pk_EventID","Species_Code")]))
  
  # work with dates and time
  ## (different for odbcConnect and HMisc pacakge)
  if (connect == "ODBC") { 
  temp.amoy2$Date <- ymd(temp.amoy2$Date) #convert to date
  temp.amoy2$year <- year(temp.amoy2$Date) #Create year variable
  temp.amoy2$month <- month(temp.amoy2$Date) #Create month variable
  
  # strip time out (this may need to be changed if number of digits varies)
  temp.amoy2$Start_Time<-substr(temp.amoy2$Start_Time,12,19)
  temp.amoy2$Group_Time<-substr(temp.amoy2$Group_Time,12,19)
  } 
  if (connect == "Hmisc") {
    temp.amoy2$Date  <- date(mdy_hms(as.character(temp.amoy2$Date))) #convert to date
    temp.amoy2$year  <- year(temp.amoy2$Date) #Create year variable
    temp.amoy2$month <- month(temp.amoy2$Date) #Create month variable
    
    temp.amoy2$Start_Time <- substr(temp.amoy2$Start_Time, 10, 19)
    temp.amoy2$Group_Time <- substr(temp.amoy2$Group_Time, 10, 19)
  }
  
  #names(temp.amoy2)
  ## subset df to final 
  AMOY_raw <- dplyr::select(temp.amoy2, 
                            Park, Island, Segment, Survey_Class, Survey_Type, 
                            Date, Start_Time, year, month, Obs_Type,
                            Species_Code,Group_Time, Group_NewTerritory, 
                            Group_Notes, Group_Coords, Species_Unit, Unit_Count, 
                            Wind_Direction, Wind_Speed, Air_Temp_F, Cloud_Perc,
                            Tide_Stage) %>% 
    # refactor Survey_Type levels from  Creche and Surv. to Incubation
    mutate(Survey_Type= recode_factor(Survey_Type,Surveillance = "Incubation", Creche = "Incubation"))
  
  
  ### export to use in R viz and for R package
  if (export == TRUE) {
    write.table(AMOY_raw, "./Data/AMOY_raw.csv", sep=",", row.names= FALSE)
    save(AMOY_raw, file = "./Data/AMOY_raw.RData")
  }
  
  return(AMOY_raw)
}


## Need this help function to remove labels afer using HMisc package:
## (from: https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions)
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}
