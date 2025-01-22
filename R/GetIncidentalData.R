#' @title Return in-season observations of incidental (non-target) species from database
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' (Access backend entered as 'NETNCB' in Windows ODBC manager) and pulls observations of incidental (non-target) species detected during surveys. 
#' @param DBfile Path to a specified database file. 
#' @param connect Should the function connect to the Access DB? The default 
#' (\code{connect = `ODBC`}) is to try to connect using the Windows ODBC manager. 
#' If the connection is not available or not desired, one can use \code{connect = `Hmisc`}
#' and include a patch to a saved version of the database, or
#' the function can return the saved data from the package (\code{connect = `No`}). 
#' Note the saved data may not be up-to-date.
#' @param export Should the incubation data be exported as a csv file and RData object?
#' (This argument is used to regenerate the RData for the package.)
#' 
#' @return This function returns the raw incidental survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' # incidentals <- GetIncidentalData()
#' @export

GetIncidentalData <- function(connect = "ODBC", DBfile = NULL, export = FALSE){
  ## connect to DB:
  con <- RODBC::odbcConnect("NETNCB")
  
  ###################### Import data and lookup tables used for the query   
  #"tbl_Events","tlu_Species","tbl_Observations"      
  
  # import dataframes of each tables within the DB
  events <- RODBC::sqlFetch(con, "tbl_Events")
  species <- RODBC::sqlFetch(con, "tlu_Species")
  obs <- RODBC::sqlFetch(con, "tbl_Observations")
  
  # Close connection
  RODBC::odbcClose(con)
  
  # Bind tables, filter to Incidental observations, and remove Surveillance surveys
  temp.incid <- left_join(events, obs, by = c('pk_EventID' = 'fk_EventID'))%>%
    inner_join(., species, by = 'Species_Code')%>%
    filter(., Obs_Type == 'Incidental',
           Survey_Type != 'Surveillance')
  
  # Rename columns
  temp.incid <- rename(temp.incid, 'ScientificName' = 'FullLatinName')
  names(temp.incid) <- gsub(x = names(temp.incid), pattern = 'pk_', replacement = '')
  
  # Add time variables
  temp.incid$Date  <- ymd(temp.incid$Date) #convert to date
  temp.incid$year  <- year(temp.incid$Date) #Create year variable
  temp.incid$month <- month(temp.incid$Date) #Create month variable
  
  # Remove date from the time columns
  temp.incid$Obs_Time<-substr(temp.incid$Obs_Time,12,19)
  
  ## subset df to final columns for exporting
  raw.incid <- select(temp.incid,Park, Survey_Agency, Survey_Class,
                      Survey_Type, Date, year, month, Site, 
                      Segment, Observer, Obs_Type, Species_Code, 
                      CommonName, ScientificName, Species_Unit, Unit_Count, 
                      Latitude, Longitude, Datum, Obs_Notes, Obs_Time, 
                      EventID, ObservationID)
  # sort df
  raw.incid <- raw.incid %>%
    dplyr::arrange(Date, Site, Segment, Observer, Obs_Time)
  rownames(raw.incid) <- NULL
  
  ### export to use in R viz
  
  if (export == TRUE) {
    write.table(raw.incid, "Data/raw.incid.csv", sep=",", row.names= FALSE)
    save(raw.incid, file = "Data/raw.incid.RData")
  }
  
  raw.incid
  
}