#' @title Return in-season AMOY surveys from database
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access DB 
#' (Access backend entered as 'NETNCB' in Windows ODBC manager) and returns the raw AMOY 
#' survey data. Ff the Access DB is not
#' accessible from the ODBC connection, one can try to connect via Hmisc, or
#' the function returns a saved image of the data.
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

GetAMOYData <- function(connect = "ODBC", DBfile = NULL, export = FALSE){

     # pull tables from Access
     con <- RODBC::odbcConnect("NETNCB") # connect to DB
     
     event <- RODBC::sqlFetch(con, "tbl_Events")
     group <- RODBC::sqlFetch(con, "tbl_Group")
     group_obs <- RODBC::sqlFetch(con, "tbl_Group_Observations")
     species <- RODBC::sqlFetch(con, "tlu_species")
     
     RODBC::odbcClose(con)  # close connection

     # join tables and filter
     temp.amoy <- left_join(event, group, by = c("pk_EventID" = "fk_EventID"))%>%
       filter(., Species_Code == "AMOY" & Obs_Type == "Group")%>%
       left_join(., group_obs, by = c("pk_GroupID" = "fk_GroupID"))%>%
       inner_join(., species, by = "Species_Code")
     
     # Add date information
     temp.amoy$Date <- ymd(temp.amoy$Date) #convert to date
     temp.amoy$Year <- year(temp.amoy$Date) #Create year variable
     temp.amoy$Month <- month(temp.amoy$Date) #Create month variable
     
     # Remove date from the time columns
     temp.amoy$Start_Time<-substr(temp.amoy$Start_Time,12,19)
     temp.amoy$End_Time<-substr(temp.amoy$End_Time,12,19)
     temp.amoy$Group_Time<-substr(temp.amoy$Group_Time,12,19)
     
     # Edit column names
     names(temp.amoy) <- gsub(x = names(temp.amoy), pattern = 'pk_', replacement = '')
     
     # Subset columns for final df  
     AMOY_raw <- select(temp.amoy, Park, Survey_Agency, Survey_Class, Survey_Type, 
                        Date, Month, Year, Start_Time, End_Time, Island, Segment, 
                        Recorder, Observer, Wind_Direction, Wind_Speed, Air_Temp_F, 
                        Cloud_Perc, Tide_Stage, Survey_Complete, Survey_MultiPart,
                        Survey_Duplicate, Survey_Primary, Survey_Notes, 
                        c_TargetSpp_Group, Checked, DPL, Data_Source, Obs_Type, 
                        Species_Code, CommonName, Group_Count, Group_Coords, 
                        Group_Notes, Group_Time, Species_Unit, Unit_Count, 
                        EventID, GroupID, GroupObsID)
  # sort df
  AMOY_raw <- AMOY_raw %>%
   dplyr::arrange(Date, Start_Time, Island, Segment, Recorder, GroupID)
  rownames(AMOY_raw) <- NULL
  AMOY_raw <- rename(AMOY_raw,
                       Species_Name = CommonName)
  
  ### export to use in R viz and for R package
  if (export == TRUE) {
    write.table(AMOY_raw, "./Data/AMOY_raw.csv", sep=",", row.names= FALSE)
    save(AMOY_raw, file = "./Data/AMOY_raw.RData")
  }
  
  return(AMOY_raw)
}


## Need this help function to remove labels afer using HMisc package:
## (from: https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions)
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}
