#' @title Return in-season Surveillance Data from Coastal Bird MS Access database
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' (Access backend entered as 'NETNCB' in Windows ODBC manager) and pulls observations of target species detected 
#' outside of the protocols normal survey routes (e.g., Surveillance surveys) to provide further information about potential foraging or nesting areas.
#' @param DBfile Path to a specified database file. 
#' @param connect Should the function connect to the Access DB? The default 
#' (\code{connect = `ODBC`}) is to try to connect using the Windows ODBC manager. 
#' If the connection is not available or not desired, one can use \code{connect = `Hmisc`}
#' and include a patch to a saved version of the database, or
#' the function can return the saved data from the package (\code{connect = `No`}). 
#' Note the saved data may not be up-to-date.
#' @param export Should the surveillance data be exported as a csv file and RData object?
#' (This argument is used to regenerate the RData for the package.)
#' 
#' @return This function returns the raw surveillance data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' # surveil <- GetSurveillanceData()
#' @export

GetSurveillanceData <- function(connect = "ODBC", DBfile = NULL, export = FALSE){
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
  
  # Bind tables and filter to Surveillance surveys
  temp.surv <- left_join(events, obs, by = c('pk_EventID' = 'fk_EventID'))%>%
    inner_join(., species, by = 'Species_Code')%>%
    filter(., Survey_Type == 'Surveillance')
  
  # Rename columns to match Access col names
  temp.surv <- rename(temp.surv, 'ScientificName' = 'FullLatinName')
  names(temp.surv) <- gsub(x = names(temp.surv), pattern = 'pk_', replacement = '')
  
  # Add time variables
  temp.surv$Date  <- ymd(temp.surv$Date) #convert to date
  temp.surv$year  <- year(temp.surv$Date) #Create year variable
  temp.surv$month <- month(temp.surv$Date) #Create month variable
  
  # Remove date from the time columns
  temp.surv$Obs_Time<-substr(temp.surv$Obs_Time,12,19)
  
  ## subset df to final columns for exporting
  raw.surv <- select(temp.surv, Park, Survey_Agency, Survey_Class, 
                     Survey_Type, Date, year, month, Site, 
                     Segment, Observer, Obs_Type, Species_Code, 
                     CommonName, ScientificName, Species_Unit, Unit_Count, 
                     Latitude, Longitude, Datum, Obs_Notes, Obs_Time, EventID, 
                     ObservationID)
  
  # sort df
  raw.surv <- raw.surv %>%
    dplyr::arrange(Date, Site, Segment, Observer, Obs_Time)
  rownames(raw.surv) <- NULL
  
  ### export to use in R viz
  #write.table(raw.surv, "./Data/raw.surv.csv", sep=",", row.names= FALSE)
  
  if (export == TRUE) {
    write.table(raw.surv, "Data/raw.surv.csv", sep=",", row.names= FALSE)
    save(raw.surv, file = "Data/raw.surv.RData")
  }
  
  raw.surv
}