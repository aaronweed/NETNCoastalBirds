#' @title Return in-season survey events from database
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' (Access backend entered as 'NETNCB' in Windows ODBC manager) and returns summary information
#' on surveys
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
#' @return This function returns the raw AMOY survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' # amoy <- GetAMOYdata()
#' @export

GetEvents <- function(connect = "ODBC", DBfile = NULL, export = FALSE){
  
  con <- RODBC::odbcConnect("NETNCB")# establish connection to DB
  
  events <- RODBC::sqlFetch(con, "tbl_Events")
  
  RODBC::odbcClose(con)
  
  # drop extra columns
  events <- events[, !colnames(events)%in%c("Imported_By", "Imported_Time", "Imported_Notes")] 
  
  # rearrange columns
  events <- events[,c(3:26,2,1)]
  
  # Sort
  events <- events %>%
    dplyr::arrange(Date, Start_Time, Island, Segment, Observer)

  rownames(events) <- NULL
  
  ### export to use in R viz
  #write.table(events, "./Data/events.csv", sep=",", row.names= FALSE)
  
  if (export == TRUE) {
    write.table(events, "Data/events.csv", sep=",", row.names= FALSE)
    save(events, file = "Data/events.RData")
  }
  
  events
}