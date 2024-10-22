#' @title Return creche surveys from database
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate year ymd month date
#'   
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' (Access backend entered as 'NETNCB' in Windows ODBC manager) and 
#' returns the raw creche survey data of COEI. If the Access DB is not
#' accessible from the ODBC connection, one can try to connect via Hmisc, or
#' the function returns a saved image of the data.
#' 
#' @param connect Should the function connect to the Access DB? The default 
#' (\code{connect = `ODBC`}) is to try to connect using the Windows ODBC manager. 
#' If the connection is not available or not desired, one can use \code{connect = `Hmisc`}
#' and include a patch to a saved version of the database, or
#' the function can return the saved data from the package (\code{connect = `No`}). 
#' Note the saved data may not be up-to-date.
#' @param DBfile Path to a specified database file. 
#' @param export Should the incubation data be exported as a csv file and RData object?
#' (This argument is used to regenerate the RData for the package.)
#'
#' @return This function returns the raw AMOY survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples 
#' creche <- GetCrecheData()

#' @export


GetCrecheData <- function(connect = "ODBC", DBfile = NULL, export= FALSE) {
 

  if(!requireNamespace("Hmisc", quietly = TRUE)){
    stop("Package 'Hmisc' is needed for this function to work. Please install it.", call. = FALSE)
  } 
  
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
   ## First, get the data depending on the connection option:
  if (connect == "ODBC") {
    con <- RODBC::odbcConnect("NETNCB")
  
 
  ###################### Import data and lookup tables used for the query   ################
  #"tbl_Events","tbl_Group","tbl_Group_Observations" ,"tbl_Nests","tbl_Observations"      
  
  # import dataframes of each tables within the DB
  group <- RODBC::sqlFetch(con, "tbl_Group")
  event <- RODBC::sqlFetch(con, "tbl_Events")
  group_obs<-RODBC::sqlFetch(con, "tbl_Group_Observations")
  species <- RODBC::sqlFetch(con, "tlu_Species")
 
  RODBC::odbcClose(con)
  
  ## If connection didn't work, try mdb.get() 
  } else if (connect == "Hmisc") {
    if (is.null(DBfile)) {
      stop("Please specify the database location for this connection option.")
    }
    
    group <- Hmisc::mdb.get(db_path, tables="tbl_Group", 
                     mdbexportArgs = '', stringsAsFactors = FALSE)
    event <- Hmisc::mdb.get(DBfile, tables = "tbl_Events", 
                     mdbexportArgs = '', stringsAsFactors = FALSE)
    group_obs <- Hmisc::mdb.get(db_path, tables="tbl_Group_Observations", 
                         mdbexportArgs = '', stringsAsFactors = FALSE)
    species <- Hmisc::mdb.get(db_path, tables="tlu_Species", 
                                mdbexportArgs = '', stringsAsFactors = FALSE)
    group <- clear.labels(group)
    event <- clear.labels(event)
    group_obs <- clear.labels(group_obs)
    species <- clear.labels(species)
    
    ## The names are imported differently using mdb.get().
    ## Replace "." with "_"
    names(group) <- gsub("\\.", "_", names(group))
    names(event) <- gsub("\\.", "_", names(event))
    names(group_obs) <- gsub("\\.", "_", names(group_obs))
    names(species) <- gsub("\\.", "_", names(species))
  } else if (connect == "No") {
    return(data(creche_raw))
  } else {
    stop("connect must be ODBC, Hmisc, or No.")
  }
  

  #############Join together various dataframes to create queries ##########
   ##### Boat-based Creche surveys (COEI) ###########
  # returns df with the counts of COEI adults and ducklings observed during creche surveys per island,  Excludes incidental obs.
  # NOTE: When applicable this query returns multiple records of species counts per island/date combination when species was spotted at more than one time.
  
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
  
  temp.crec<-left_join(event.crec, group.crec,  by= c("pk_EventID" = "fk_EventID"))
  #names(temp.crec)
  
  #count(unique(group_obs[,c("GroupID","Species_Unit")]))
  
  # add in group obs data for each event/group of COEI. Also add species information and filter cols
  #intersect(names(temp.crec),names(group_obs))
  
  temp.crec2<-left_join(temp.crec, group_obs, by = c("pk_GroupID" = "fk_GroupID"))%>%
    left_join(., species, by= 'Species_Code')%>%
    filter(., Obs_Type == 'Group')
  
  #names(temp.crec2)
  names(temp.crec2) <- gsub(x = names(temp.crec2), pattern = 'pk_', replacement = '')
 
  
  
  # work with dates and time
  ## (different for odbcConnect and HMisc pacakge)
  if (connect == "ODBC") { 
  temp.crec2$Date<-ymd(temp.crec2$Date) #convert to date
  temp.crec2$year<-year(temp.crec2$Date) #Create year variable
  temp.crec2$month<-month(temp.crec2$Date) #Create month variable
  
  # strip time out (this may need to be changed if number of digits varies)
  temp.crec2$Start_Time<-substr(temp.crec2$Start_Time,12,19)
  temp.crec2$Group_Time<-substr(temp.crec2$Group_Time,12,19)
  
  #names(temp.crec2)
  } 
  if (connect == "Hmisc") {
    temp.crec2$Date  <- date(mdy_hms(as.character(temp.crec2$Date))) #convert to date
    temp.crec2$year  <- year(temp.crec2$Date) #Create year variable
    temp.crec2$month <- month(temp.crec2$Date) #Create month variable
    
    temp.crec2$Start_Time <- substr(temp.crec2$Start_Time,10,19)
    temp.crec2$Group_Time <- substr(temp.crec2$Group_Time,10,19)
  }
  ## subset df to final 
  creche_raw<-select(temp.crec2, Park,	Survey_Agency,	Survey_Class,	Survey_Type,	
                     Date, Start_Time,	End_Time,	Island,	Segment,	Recorder,
                     Observer,	Wind_Direction,	Wind_Speed,	Air_Temp_F,	
                     Cloud_Perc, Tide_Stage,	Survey_Complete,	Survey_MultiPart,
                     Survey_Duplicate,	Survey_Primary, Survey_Notes,
                     c_TargetSpp_Group,	Checked,	DPL,	Data_Source, Obs_Type,
                     Species_Code,	CommonName,	Group_Count,	Group_Coords,
                     Group_Notes,	Group_Time,	Species_Unit,	Unit_Count,	EventID,
                     GroupID,	GroupObsID, month, year)
                     
  
  ### export to use in R viz
  #write.table(creche_raw, "./Data/creche_raw.csv", sep=",", row.names= FALSE)
  
  if (export == TRUE) {
    write.table(creche_raw, "Data/creche_raw.csv", sep=",", row.names= FALSE)
    save(creche_raw, file = "Data/creche_raw.RData")
  }
  
  creche_raw
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