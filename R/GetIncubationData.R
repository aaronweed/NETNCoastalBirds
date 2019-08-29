#' @title Return incubation surveys from database
#'
#' @importFrom  dplyr select left_join
#' @importFrom lubridate ymd year month
#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#' @importFrom Hmisc mdb.get
#'  
#' @description This function connects to the backend of NETN's Coastal Bird 
#' Access DB (Access backend entered as 'NETNCB' in Windows ODBC manager) and 
#' returns the boat-based incubation data. If the Access DB is not
#' accessible from the ODBC connection, one can try to connect via Hmisc, or
#' the function returns a saved image of the data.
#' @param connect Should the function connect to the Access DB? The default 
#' (\code{connect = `ODBC`}) is to try to connect using the Windows ODBC manager. 
#' If the connection is not available or not desired, one can use \code{connect = `HMisc`}
#' and include a patch to a saved version of the database, or
#' the function can return the saved data from the package (\code{connect = `No`}). 
#' Note the saved data may not be up-to-date.
#' @param export Should the incubation data be exported as a csv file and RData object?
#' (This argument is used to regenerate the RData for the package.)
#'
#' @return The raw boat-based incubation survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples 
#' incubation <- GetIncubationData(x)
#' @export

### This script connects to the backend of NETN's Coastal Bird Access DB and returns 
### the raw incubation survey data 

## ODBC BE file is: NETN_CoastalBirds_Master_BE_20180826.accdb

#https://science.nature.nps.gov/im/units/netn/monitor/vitalSigns/birds/coastalBirds.cfm for further details 


GetIncubationData <- function(x, connect = "ODBC", DBfile = NULL, export = FALSE) {
  ## First, get the data depending on the connection option:
  if (connect == "ODBC") {
    # Connect to database BE using odbcConnect (default)
    con <- odbcConnect("NETNCB")
    
    ######### Import data and lookup tables used for the query   ##############
    #"tbl_Events","tbl_Group_Observations"    
    # import dataframes of each tables within the DB
    event <- sqlFetch(con, "tbl_Events"); names(event)
    obs <- sqlFetch(con, "tbl_Observations"); names(obs)
    odbcClose(con)
    
    
    ## If connection didn't work, try mdb.get() 
   } else if (connect == "Hmisc") {
     if (is.null(DBfile)) {
       stop("please specify a database location for this connection option.")
     }
      event <- mdb.get(DBfile, tables = "tbl_Events", stringsAsFactors = FALSE); 
      obs <- mdb.get(DBfile, tables = "tbl_Observations", stringsAsFactors = FALSE); 
      ## The names are imported differently using mdb.get().
      ## Replace "." with "_"
      names(event) <- gsub("\\.", "_", names(event))
      names(obs) <- gsub("\\.", "_", names(obs))
   } else if (connect == "No") {
       return(data(incubation_raw))
   } else {
     stop("connect must be ODBC, HMisc, or No.")
   }
    
  ## Keep organizing data from DB:
  
    ####### create new vectors to match field names for binding ########
    # names(obs)
    obs$pk_EventID <- obs$fk_EventID 
    #unique(event$pk_EventID)  # is NAs...
    #unique(obs$pk_EventID)  # NAs...
  
    
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
    temp.incub <- filter(event, Survey_Type == "Incubation") %>% 
      left_join(.,obs, by = c("pk_EventID")) %>% 
      filter(Obs_Type =="Target")
     
   # View(temp.incub)
    
    # work with dates and time
    temp.incub$Date  <- ymd(temp.incub$Date) #convert to date
    temp.incub$year  <- year(temp.incub$Date) #Create year variable
    temp.incub$month <- month(temp.incub$Date) #Create month variable
    
    # strip time out (this may need to be changed if number of digits varies)
    temp.incub$Start_Time <- substr(temp.incub$Start_Time, 12, 19)
    
    ## subset df to final columns
    #names(temp.incub)
    incubation_raw <-select(temp.incub ,Park, Island, Segment, Survey_Class,
                                      Survey_Type, Survey_MultiPart,
                                      Survey_Duplicate, Survey_Primary, Survey_Complete,
                                      Date, year, month, Start_Time, c_Observer, 
                                      Species_Code, Species_Unit, Unit_Count, 
                                      Obs_Notes, Recorder, Data_Source, Wind_Direction,
                                      Wind_Speed, Air_Temp_F, Cloud_Perc, Tide_Stage)  
    # sort df
    incubation_raw <- incubation_raw %>%
      dplyr::arrange(Island, Segment, Date, Species_Code, c_Observer)
    #[order(incubation_raw$Island,incubation_raw$Segment,incubation_raw$Date, incubation_raw$Species_Code, incubation_raw$Observer),]
    
    ### export to use in R viz and for R package
    if (export == TRUE) {
      write.table(incubation_raw, "Data/incubation_raw.csv", sep=",", row.names= FALSE)
      save(incubation_raw, file = "Data/incubation_raw.RData")
    }

      incubation_raw
}