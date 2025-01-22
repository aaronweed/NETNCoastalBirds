#' @title Return end of season mating pair observations of AMOY and PIPL from Coastal Bird MS Access database
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' and returns summary data for AMOY and PIPL by reporting agency for BOHA islands per survey year. 
#' (Access backend entered as 'NETNCB' in Windows ODBC manager)
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
#' @return This function returns end of season mating pair observations of AMOY and PIPL as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' # amoy_pipl <- GetAMOY_PIPL()
#' @export

GetAMOY_PIPL_EOYSum <- function(connect = "ODBC", DBfile = NULL, export = FALSE){
  
  ## connect to DB:
  con <- RODBC::odbcConnect("NETNCB")
  
  # Pull tables
  
  summary <- RODBC::sqlFetch(con, "tbl_Summary_Spp_AMOY_PIPL")
  species <- RODBC::sqlFetch(con, "tlu_Species")
  
  # Close connection
  RODBC::odbcClose(con)
  
  # Bind tables, filter to desired species
  temp.AMOYPIPL <- full_join(species, summary, by = c('Species_Code' = 'Species'))%>%
    filter(., Species_Code == 'AMOY'| Species_Code == 'PIPL')
  
  # rename column
  names(temp.AMOYPIPL)[names(temp.AMOYPIPL) == 'FullLatinName'] <- "ScientificName"
  
  # subset df to final columns for exporting
  AMOYPIPL <- select(temp.AMOYPIPL, "Species_Code", "CommonName", "ScientificName", "Location",
                         "Survey_Year", "Pair_Count_MAwindow", "Reporting_Agency",
                         "DPL", "Notes")
  
  # sort df
  AMOYPIPL <- AMOYPIPL %>%
    dplyr::arrange(CommonName, Location, Survey_Year)
  rownames(AMOYPIPL) <- NULL
  
  ### export to use in R viz
  #write.table(AMOYPIPL, "./Data/AMOYPIPL.csv", sep=",", row.names= FALSE)
  
  if (export == TRUE) {
    write.table(AMOYPIPL, "Data/AMOYPIPL.csv", sep=",", row.names= FALSE)
    save(AMOYPIPL, file = "Data/AMOYPIPL.RData")
  }
  
  AMOYPIPL
}