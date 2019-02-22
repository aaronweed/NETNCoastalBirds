#' @title Return ground-based nest surveys from database
#'
#' @importFrom plyr join
#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access 
#' DB and returns the raw ground-based nest survey data for non-AMOY 
#' (BCNH COEI COTE DCCO GBBG GLIB GREG HERG LETE SNEG SPSA WILL).
#' @section Warning:
#' User should have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' (If ODBC_connect = TRUE).
#' 
#' @param x Denote in parentheses to return all ground-based nest surveys.
#' @param ODBC_connect Should the function connect to the Access DB? The default (TRUE) is to 
#' try to connect using the Windows ODBC manager. If the connection is not available or not desired, 
#' the function can return the saved data from the package. 
#' Note the saved data may not be up-to-date.
#' @param export Should the incubation data be exported as a csv file and RData object?
#' (This argument is used to regenerate the RData for the package.)
#' @return This function returns the raw nest survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#'@examples 
#' nests <- GetNestData(x)
#' @export


GetNestData <- function(x, ODBC_connect = TRUE, export = FALSE) {
  if (ODBC_connect == TRUE) {
    # Connect to database BE 
    
    con <- odbcConnect("NETNCB")
    
    ###################### Import data and lookup tables used for the query   ################
    #"tbl_Events","tbl_Group","tbl_Group_Observations" ,"tbl_Nests","tbl_Observations"      
    
    # import dataframes of each tables within the DB
    
    event <- sqlFetch(con, "tbl_Events")
    nests <- sqlFetch(con, "tbl_Nests")
    
    odbcClose(con)
  
  
  ####### create new vectors to match field names for joining ########
  
  nests$pk_EventID <- nests$fk_EventID
  
  
  #############Join together various dataframes to create queries ##########
  
  ###### Nest surveys ###########
  # query ground-based and boat-based (DCCO/gulls) nest surveys and return df 
  # with counts of chicks and eggs per species, island, and year. 
  ## BCNH COEI COTE DCCO GBBG GLIB GREG HERG LETE SNEG SPSA WILL 
  # NOTE: When applicable this query retuns multiple records of species counts 
  # per island/date combination when a species was spotted at more than one time per event.
  # will need to sum the number of nests and counts nest per island, date, species etc.
  
  # bind Events to Nests
  # intersect(names(event),names(nests))
  
  temp.nest <- join(nests, event, by="pk_EventID")
  #head(temp.nest)
  
  
  
  # Convert date to date object and create year and month vars
  temp.nest$Date  <- as.Date(temp.nest$Date, format = "%Y-%m-%d") #convert to date
  temp.nest$year  <- as.factor(format(temp.nest$Date, "%Y")) #Create year variable
  temp.nest$month <- as.factor(format(temp.nest$Date, "%m")) #Create month variable
  
  # strip off time (this may need to be changed if number of digits varies)
  temp.nest$Start_Time <- substr(temp.nest$Start_Time, 12, 19)
  #names(temp.nest)
  
  ### convert 999 counts for nest contents to NAs
  
  temp.nest$Unit_Count[temp.nest$Unit_Count == 999] = NA
  temp.nest$Egg_Count[temp.nest$Egg_Count == 999] = NA
  temp.nest$Chick_Count[temp.nest$Chick_Count == 999] = NA
  
  ## subset df to final columns for exporting
  nest_surveys_raw <- temp.nest[, c("Park", "Island", "Segment", "Survey_Class",
                                    "Survey_Type", "Survey_MultiPart",
                                    "Survey_Duplicate", "Survey_Primary", 
                                    "Survey_Complete", "Obs_Type", "Date", "year", 
                                    "month", "Start_Time", "Species_Code", "Species_Unit", "Nest_Status", 
                                    "Unit_Count", "Egg_Count", "Chick_Count", 
                                    "Obs_Coords", "Obs_Notes", "Wind_Direction", "Wind_Speed", "Air_Temp_F", "Cloud_Perc", "Tide_Stage")]  
  
  ### export to use in R viz
  #write.table(nest_surveys_raw, "./Data/nest_surveys_raw.csv", sep=",", row.names= FALSE)
  
  ## Events
<<<<<<< HEAD
  #nest_Events<-temp.nest[,c("Park","Island","Segment", "Survey_Class","Survey_Type","Obs_Type","Date" ,"year", "month","Start_Time","pk_EventID")]
  #write.table(nest_Events, "./Data/nestEvents.csv", sep=",", row.names= FALSE)
  
  ###Nest Data
  #nest_Data<-temp.nest[,c("Park","Island","Segment","Date" ,"year", "month","Start_Time","c_Observer", "Recorder", "pk_EventID","Species_Code","Species_Unit","Nest_Status","Unit_Count"  ,"Egg_Count","Chick_Count")]
=======
  nest_Events <- temp.nest[, c("Park", "Island", "Segment", "Survey_Class", "Survey_Type", 
                               "Obs_Type", "Date", "year", "month", "Start_Time", "pk_EventID")]
  #write.table(nest_Events, "./Data/nestEvents.csv", sep=",", row.names= FALSE)
  
  ###Nest Data
  nest_Data <- temp.nest[, c("Park", "Island", "Segment", "Date", "year", "month", "Start_Time", 
                             "pk_EventID", "Species_Code", "Species_Unit", "Nest_Status", "Unit_Count", "Egg_Count", "Chick_Count")]
>>>>>>> 975e16eeef5594e886f3415c986f92fb97654702
  #write.table(nest_Data, "./Data/nestData.csv", sep=",", row.names= FALSE)
  
  ### export to use in R viz and for R package
  if (export == TRUE) {
    write.table(nest_surveys_raw, "Data/nest_surveys_raw.csv", sep=",", row.names= FALSE)
    save(nest_surveys_raw, file = "Data/nest_surveys_raw.RData")
  }
  
  } 
  if (ODBC_connect == FALSE) {
     data(nest_surveys_raw)
  }
  
  return(nest_surveys_raw)
}