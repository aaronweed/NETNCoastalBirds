#' @title Return end of season AMOY mating pair summary
#'
#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#' @importFrom magrittr %>% 
#' @importFrom dplyr rename
#' 
#' @description This function connects to the backend of NETN's Coastal Bird Access DB and returns the end of season AMOY mating pair summary.
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param x Denote "x" in parentheses to return a \code{data.frame} of all AMOY observations.
#'
#' @return This function returns the end of season AMOY mating pair summary as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples
#' AMOY_MatingPairSumm(x)
#' @export

AMOY_MatingPairSumm<-function(x,  ODBC_connect = TRUE, export = FALSE){
  
  if (ODBC_connect == TRUE) {
    
    con <- odbcConnect("NETNCB")
    
    ###################### Import data and lookup tables used for the query   ################
    
    # import dataframes of each tables within the DB
    AMOY <- sqlFetch(con, "tbl_Summary_AMOY")
    
    odbcClose(con)
    
     
      AMOY<-AMOY %>% dplyr::rename(time= Survey_Year, value = Pair_Count, Island= Location)
      AMOY$value[AMOY$value == 999] = NA  ### convert 999 counts for nest contents to NAs
      AMOY$Species_Code <- "AMOY"

    ### export to use in R viz and for R package
    if (export == TRUE) {
      write.table(AMOY, "./Data/AMOY_Pairs.csv", sep=",", row.names= FALSE)
      save(AMOY, file = "./Data/AMOY_Pairs.RData")
    }
    
  } 
  if (ODBC_connect == FALSE) {
    data(AMOY_Pairs)
  }

  
  return(AMOY)
}