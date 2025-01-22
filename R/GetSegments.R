#' @title Returns information for segments for each sites surveyed from the Coastal birds Access database.
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' and returns information on the segments of each site that are surveyed for coastal breeding birds
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
#' @return This function returns all of segment information as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' # segments <- GetSegments()
#' @export

GetSegments <- function(connect = "ODBC", DBfile = NULL, export = FALSE){
  
  con <- RODBC::odbcConnect("NETNCB")# establish connection to DB
  
  segments <- RODBC::sqlFetch(con, "tbl_Survey_Segments")
  
  RODBC::odbcClose(con)
  
  
  rownames(segments) <- NULL
  
  # rearrange columns
  segments <- segments[,c(2:6,1)]
  
  # Sort
  segments <- segments %>%
    dplyr::arrange(Site, Segment)
  
  ### export to use in R viz
  #write.table(segments, "./Data/segments.csv", sep=",", row.names= FALSE)
  
  if (export == TRUE) {
    write.table(segments, "Data/segments.csv", sep=",", row.names= FALSE)
    save(segments, file = "Data/segments.RData")
  }
  
  segments
}