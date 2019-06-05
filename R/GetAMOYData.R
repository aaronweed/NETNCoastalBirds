#' @title Return AMOY surveys from database
#'

#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB and returns the annual end of season estimates of
#' American Oystercatcher (AMOY) per Island.
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param x Denote "x" in parentheses to return a \code{data.frame} of all AMOY observations.
#'
#' @return This function returns the raw AMOY nesting pair survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' GetAMOYData(x, export= TRUE)
#' @export

GetAMOYData<-function(x,  ODBC_connect = TRUE, export = FALSE){

  
  if (ODBC_connect == TRUE) {
    
      con <- odbcConnect("NETNCB")
  
  ###################### Import data and lookup tables used for the query   ################
  
  # import dataframes of each tables within the DB
  AMOY <- sqlFetch(con, "tbl_Summary_AMOYpairs")
  
  odbcClose(con)
  
  if (export == TRUE) {
    write.table(AMOY, "Data/AMOY_Pairs.csv", sep=",", row.names= FALSE)
    save(AMOY, file = "Data/AMOY_Pairs.RData")
    }
  
  } 
  if (ODBC_connect == FALSE) {
    data(AMOY_Pairs)
  }
  
  return(AMOY)
}