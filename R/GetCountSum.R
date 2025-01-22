#' @title Pulls summary of annual counts, including data not collected by NETN, from the coastal birds Access DB
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' and returns end of year counts for target species (including AMOY and PIPL). Includes
#' non-NPS collected data.
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
#' @return This function returns all of summariezd counts as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' # counts <- GetCountSum()
#' @export

GetCountSum <- function(connect = "ODBC", DBfile = NULL, export = FALSE){
  
  con <- RODBC::odbcConnect("NETNCB")# establish connection to DB
  
  counts <- RODBC::sqlFetch(con, "tbl_Summary_Counts")
  
  RODBC::odbcClose(con)
  
  
  rownames(counts) <- NULL
  
  # rearrange columns
  counts <- counts[,c(2:14,1)]
  
  # Sort
  counts <- counts %>%
    dplyr::arrange(Site_Name, Survey_Year, Survey_Date, Species_Code, Count_Unit, Count)
  
  ### export to use in R viz
  #write.table(counts, "./Data/counts.csv", sep=",", row.names= FALSE)
  
  if (export == TRUE) {
    write.table(counts, "Data/counts.csv", sep=",", row.names= FALSE)
    save(counts, file = "Data/counts.RData")
  }
  
  counts
}