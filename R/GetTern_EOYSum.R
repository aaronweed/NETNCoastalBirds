#' @title Return end of season mating pair observations of terns from Coastal Bird MS Access database
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' (Access backend entered as 'NETNCB' in Windows ODBC manager) and returns the end of year summary of tern 
#' surveys (COTE, LETE, ROST) by reporting agency for BOHA islands.
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
#' # terns <- GetTern_EOYSum()
#' @export

GetTern_EOYSum <- function(connect = "ODBC", DBfile = NULL, export = FALSE){
  ## connect to DB:
  con <- RODBC::odbcConnect("NETNCB")
  
  ###################### Import data and lookup tables used for the query   
  #"tbl_Summary_Spp_Terns","tlu_Species","tlu_Tern_Summary_Method", "tlu_Tern_Summary_Productivity"     
  
  # import dataframes of each tables within the DB
  summary <- RODBC::sqlFetch(con, "tbl_Summary_Spp_Terns")
  species <- RODBC::sqlFetch(con, "tlu_Species")
  method <- RODBC::sqlFetch(con, "tlu_Tern_Summary_Method")
  Productivity <- RODBC::sqlFetch(con, "tlu_Tern_Summary_Productivity")
  
  # Close connection
  RODBC::odbcClose(con)
  
  # Bind tables, filter to tern species, and remove NA's from DPL column
  temp.terns <- left_join(summary, species, by = c('Species' = 'Species_Code'))%>%
    full_join(., method, by =c('Method' = 'Code'))%>%
    full_join(., Productivity, by = c('Productivity' = 'Code'))%>%
    filter(., Species == 'COTE'| Species == 'LETE'| Species == 'ROTE')%>%
    filter(!is.na(.$DPL))
  
  # Rename some cols to match DB version
  temp.terns%>%
    rename(Method_Code = Method, Method_Desc = Description.x, 
           Productivity_Code = Productivity, 
           Productivity_Desc = Description.y,
           Species_Code = Species,
           ScientificName = FullLatinName) -> temp.terns
  
  # subset df to final columns for exporting
  raw.terns <- select(temp.terns, "Species_Code", "CommonName", "ScientificName",
                      "Location", "Survey_Year", "Count_MAwindow", "Method_Code",
                      "Method_Desc", "Productivity_Code", "Productivity_Desc",
                      "Reporting_Agency", "DPL", "Notes")
  # sort df
  raw.terns <- raw.terns %>%
    dplyr::arrange(Species_Code, Location, Survey_Year)
  rownames(raw.terns) <- NULL
  
  ### export to use in R viz
  #write.table(raw.terns, "./Data/raw.terns.csv", sep=",", row.names= FALSE)
  
  if (export == TRUE) {
    write.table(raw.terns, "Data/raw.terns.csv", sep=",", row.names= FALSE)
    save(raw.terns, file = "Data/raw.terns.RData")
  }
  
  raw.terns
}