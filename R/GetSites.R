#' @title Return information for sites surveyed for coastal breeding birds from the Coastal birds Access database.
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' (Access backend entered as 'NETNCB' in Windows ODBC manager) and returns summary information
#' on survey sites. 
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
#' @return This function returns all of the site data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' # sites <- GetSites()
#' @export

GetSites <- function(connect = "ODBC", DBfile = NULL, export = FALSE){
  
  con <- RODBC::odbcConnect("NETNCB")# establish connection to DB
  
  sites <- RODBC::sqlFetch(con, "tbl_Sites")
  
  RODBC::odbcClose(con)
  
  # Sort
  sites <- sites %>%
    dplyr::arrange(Site_Name)
  
  if (export == TRUE) {
    write.table(sites, "Data/sites.csv", sep=",", row.names= FALSE)
    save(sites, file = "Data/sites.RData")
  }
  
  sites
}