#' @title Return incubation surveys from database
#'
#' @import dplyr
#' @importFrom plyr join
#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#'  
#' @description This function connects to the backend of NETN's Coastal Bird 
#' Access DB (Access backend entered as 'NETNCB' in Windows ODBC manager) and 
#' returns the boat-based incubation data. If the Access DB is not
#' accessible from the ODBC connection, the user may try to connect using the Hmisc
#' package. If neither connection works, the function returns a saved image of the data.
#' @param ODBC_connect Should the function connect to the Access DB? The default is to 
#' try to connect using the Windows ODBC manager. If the connection is not available or not desired, 
#' the function can return the saved data from the package. 
#' (Set \code{ODBC_connect} and \code{Hmisc_connect} both equal to \code{FALSE}.) 
#' Note the saved data may not be up-to-date.
#' @param Hmisc_connect If the Windows ODBC manager is unavaialble/ fails, the function
#' can try to connect using the Hmisc package. If the connection is not available or not desired, 
#' the function can return the saved data from the package. 
#' (Set \code{ODBC_connect} and \code{Hmisc_connect} both equal to \code{FALSE}.) 
#' Note the saved data may not be up-to-date.
#' @param export Should the incubation data be exported as a csv file and RData object?
#' @param DBfile What is the location and file name of the Access Database?  
#' This argument is required if the user wants to connect to the database using the 
#' Hmisc package.
#'
#' @return This function returns the raw boat-based incubation survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples 
#' GetIncubationData()
#' @export

### This script connects to the backend of NETN's Coastal Bird Access DB and 
### returns the raw incubation survey data 

## ODBC BE file is: NETN_CoastalBirds_BE_20180309.accdb

#https://science.nature.nps.gov/im/units/netn/monitor/vitalSigns/birds/coastalBirds.cfm for further details 


GetIncubationData <- function(ODBC_connect = TRUE, Hmisc_connect = FALSE, export = FALSE,
                              DBfile = '~/repos/NPS/trunk/Boston_birds/Data/NETN_CoastalBirds_BE_20181010.accdb') {
  if (ODBC_connect == TRUE) {
    # Connect to database BE using odbcConnect (default)
    con <- odbcConnect("NETNCB")

    ######### Import data and lookup tables used for the query   ##############
    #"tbl_Events","tbl_Group_Observations"    
    # import dataframes of each tables within the DB
    event <- sqlFetch(con, "tbl_Events"); names(event)
    obs <- sqlFetch(con, "tbl_Observations"); names(obs)
    
    odbcClose(con)
    
    
    ## If connection didn't work, try mdb.get() 
    if (Hmisc_connect == TRUE) {
      event <- mdb.get(DBfile, tables = "tbl_Events", stringsAsFactors = FALSE); names(event)
      obs <- mdb.get(DBfile, tables = "tbl_Observations", stringsAsFactors = FALSE); names(obs)   
      ## The names are imported differently using mdb.get().
      ## Replace "." with "_"
      names(event) <- gsub("\\.", "_", names(event))
      names(obs) <- gsub("\\.", "_", names(obs))
    }
    
    ####### create new vectors to match field names for binding ########
    # names(obs)
    obs$pk_EventID <- obs$fk_EventID 
    unique(event$pk_EventID)  # is NAs...
    unique(obs$pk_EventID)  # NAs...
    
    
    #############Join together various dataframes to create queries ##########
    
    ##### Boat-based Incubation surveys for nests and adults of target species groups  ###########
    # COTE, DCCO,  GBBG, HERG, LETE, SPSA, WILL 
    # returns df with the counts of adults and nests of target species 
    # observed during incubation surveys per island, segment, date and  species.
    # NOTE: When applicable this query retuns muultiple records of species 
    # counts per island/date combination when species was spotted at more than one time.
    # will need to sum the counts per island, date, species etc.
    
    # Add obs data to  incubation event data
    # intersect(names(event), names(obs))
    
    temp.incub <- plyr::join(event, obs, by = "pk_EventID")
    # View(temp.incub)
    
    ## remove incidental obs
    temp.incub2 <- temp.incub[temp.incub$Obs_Type %in% "Target", ]
    
    temp.incub2 <- droplevels(temp.incub2) # clean up factor levels
    # View(temp.incub2)
    
    # work with dates and time
    temp.incub2$Date  <- as.Date(temp.incub2$Date, format = "%Y-%m-%d") #convert to date
    temp.incub2$year  <- as.factor(format(temp.incub2$Date, "%Y")) #Create year variable
    temp.incub2$month <- as.factor(format(temp.incub2$Date, "%m")) #Create month variable
    
    # strip time out (this may need to be changed if number of digits varies)
    temp.incub2$Start_Time <- substr(temp.incub2$Start_Time, 12, 19)
    
    ## subset df to final columns
    #names(temp.incub2)
    incubation_raw <- temp.incub2[, c("Park", "Island", "Segment", "Survey_Class",
                                      "Survey_Type", "Obs_Type", "Survey_MultiPart",
                                      "Survey_Duplicate", "Survey_Primary", "Survey_Complete", 
                                      "Date", "year", "month", "Start_Time", "c_Observer", 
                                      "Species_Code", "Species_Unit", "Unit_Count", 
                                      "Obs_Notes", "Recorder", "Data_Source", "Wind_Direction",
                                      "Wind_Speed", "Air_Temp_F", "Cloud_Perc", "Tide_Stage")]  
    # sort df
    incubation_raw <- incubation_raw %>%
      dplyr::arrange(Island, Segment, Date, Species_Code, Observer)
    #[order(incubation_raw$Island,incubation_raw$Segment,incubation_raw$Date, incubation_raw$Species_Code, incubation_raw$c_Observer),]
    
    ### export to use in R viz and for R package
    if (export == TRUE) {
      write.table(incubation_raw, "../Data/incubation_raw.csv", sep=",", row.names= FALSE)
      save(incubation_raw, file = "../Data/incubation_raw.RData")
    }
  }
  
  if (ODBC_connect == FALSE & Hmisc_connect == FALSE) {
    incubation_raw <- data(incubation_raw)
  }
  
  incubation_raw
  
}