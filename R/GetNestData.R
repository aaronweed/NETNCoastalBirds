#' @title Return ground-based nest surveys from database
#'
#' @importFrom dplyr select left_join
#' @importFrom lubridate ymd year month date
#'  
#' @description This function connects to the backend of NETN's Coastal Bird Access 
#' DB (Access backend entered as 'NETNCB' in Windows ODBC manager) and 
#' returns the raw ground-based nest survey data for non-AMOY 
#' (BCNH COEI COTE DCCO GBBG GLIB GREG HERG LETE SNEG SPSA WILL). If the Access DB is not
#' accessible from the ODBC connection, one can try to connect via Hmisc, or
#' the function returns a saved image of the data.
#' 
#' 
#' @param connect Should the function connect to the Access DB? The default 
#' (\code{connect = `ODBC`}) is to try to connect using the Windows ODBC manager. 
#' If the connection is not available or not desired, one can use \code{connect = `Hmisc`}
#' and include a patch to a saved version of the database, or
#' the function can return the saved data from the package (\code{connect = `No`}). 
#' Note the saved data may not be up-to-date.
#' @param DBfile Path to a specified database file. 
#' @param export Should the incubation data be exported as a csv file and RData object?
#' (This argument is used to regenerate the RData for the package.)
#' @return This function returns the raw nest survey data as a \code{data.frame}.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#'@examples 
#' nests <- GetNestData()
#' @export


GetNestData <- function(connect = "ODBC", DBfile = NULL, export = FALSE) {
  
  if(!requireNamespace("Hmisc", quietly = TRUE)){
    stop("Package 'Hmisc' is needed for this function to work. Please install it.", call. = FALSE)
  } 
  
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  ## First, get the data depending on the connection option:
  if (connect == "ODBC") {
    
 
    # Connect to database BE ti bring in tables
    
    con <- RODBC::odbcConnect("NETNCB")
    
    ###################### Import data and lookup tables used for the query   ################
    #"tbl_Events","tbl_Group","tbl_Group_Observations" ,"tbl_Nests","tbl_Observations"      
    
    # import dataframes of each tables within the DB
    
    event <- RODBC::sqlFetch(con, "tbl_Events")
    nests <- RODBC::sqlFetch(con, "tbl_Nests")
    RODBC::odbcClose(con)
  
    ## If connection didn't work, try mdb.get() 
  } else if (connect == "Hmisc") {
    if (is.null(DBfile)) {
      stop("Please specify the database location for this connection option.")
    }
    event <- Hmisc::mdb.get(DBfile, tables = "tbl_Events", 
                     mdbexportArgs = '', stringsAsFactors = FALSE)
    nests <- Hmisc::mdb.get(DBfile, tables = "tbl_Nests", 
                   mdbexportArgs = '', stringsAsFactors = FALSE)
    event <- clear.labels(event)
    nests   <- clear.labels(nests)
    
    ## The names are imported differently using mdb.get().
    ## Replace "." with "_"
    names(event) <- gsub("\\.", "_", names(event))
    names(nests) <- gsub("\\.", "_", names(nests))
  } else if (connect == "No") {
    return(data(nest_surveys_raw))
  } else {
    stop("connect must be ODBC, Hmisc, or No.")
  }
  
  ## Keep organizing data from DB:
    
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
  
   temp.nest <- filter(event, Survey_Type =="Nest" & !c_TargetSpp_Group %in% "AMOY") %>% 
      left_join(nests, ., by="pk_EventID") %>% 
      filter(Obs_Type %in% "Target")
          
  #head(temp.nest)
  
  
  # Convert date to date object and create year and month vars
   ## (different for odbcConnect and HMisc pacakge)
   if (connect == "ODBC") {
  temp.nest$Date  <- ymd(temp.nest$Date) #convert to date
  temp.nest$year  <- year(temp.nest$Date) #Create year variable
  temp.nest$month <- month(temp.nest$Date) #Create month variable
  
  # strip off time (this may need to be changed if number of digits varies)
  temp.nest$Start_Time <- substr(temp.nest$Start_Time, 12, 19)
  #names(temp.nest)
   } 
   if (connect == "Hmisc") {
     temp.nest$Date  <- date(mdy_hms(as.character(temp.nest$Date))) #convert to date
     temp.nest$year  <- year(temp.nest$Date) #Create year variable
     temp.nest$month <- month(temp.nest$Date) #Create month variable
     
     temp.nest$Start_Time <- substr(temp.nest$Start_Time, 10, 19)
   }
   
  ### convert 999 counts for nest contents to NAs
  
  temp.nest$Unit_Count[temp.nest$Unit_Count == 999] = NA
  temp.nest$Egg_Count[temp.nest$Egg_Count == 999] = NA
  temp.nest$Chick_Count[temp.nest$Chick_Count == 999] = NA
  
  ## subset df to final columns for exporting
  nest_surveys_raw <- select(temp.nest, Park, Island, Segment, Survey_Class,
                                    Survey_Type, Survey_MultiPart,
                                    Survey_Duplicate, Survey_Primary, 
                                    Survey_Complete, Date, year, 
                                    month, Start_Time, Species_Code, Species_Unit, Nest_Status, 
                                    Unit_Count, Egg_Count, Chick_Count, 
                                    Obs_Coords, Obs_Notes, Survey_Notes, Wind_Direction, Wind_Speed, Air_Temp_F, Cloud_Perc, Tide_Stage)  
  
  ### export to use in R viz and for R package
  if (export == TRUE) {
    write.table(nest_surveys_raw, "Data/nest_surveys_raw.csv", sep=",", row.names= FALSE)
    save(nest_surveys_raw, file = "Data/nest_surveys_raw.RData")
  }
    
    
    #############Join together various dataframes to create queries ##########
    
    ###### Nest surveys ###########
    # query ground-based nest surveys and return df 
    # with counts of nests, chicks and eggs per species, island, and year. 
    ## BCNH COEI COTE DCCO GBBG GLIB GREG HERG LETE SNEG SPSA WILL 
    # NOTE: When applicable this query retuns multiple records of species counts 
    # per island/date combination when a species was spotted at more than one time per event.
    # will need to sum the number of nests and counts nest per island, date, species etc.
    
    # bind Events to Nests
    # intersect(names(event),names(nests))
    
    temp.nest <- filter(event, Survey_Type =="Nest" & !c_TargetSpp_Group %in% "AMOY") %>% 
      left_join(nests, ., by= c(fk_EventID = "pk_EventID")) %>% 
      filter(Obs_Type %in% "Target")
    
    #head(temp.nest)
    
    
    # Convert date to date object and create year and month vars
    temp.nest$Date  <- ymd(temp.nest$Date) #convert to date
    temp.nest$year  <- year(temp.nest$Date) #Create year variable
    temp.nest$month <- month(temp.nest$Date) #Create month variable
    
    # strip off time (this may need to be changed if number of digits varies)
    temp.nest$Start_Time <- substr(temp.nest$Start_Time, 12, 19)
    #names(temp.nest)
    
    ### convert 999 counts for nest contents to NAs
    
    temp.nest$Unit_Count[temp.nest$Unit_Count == 999] = NA
    temp.nest$Egg_Count[temp.nest$Egg_Count == 999] = NA
    temp.nest$Chick_Count[temp.nest$Chick_Count == 999] = NA
    
    ## subset df to final columns for exporting
    nest_surveys_raw <- select(temp.nest, Park, Island, Segment, Survey_Class,
                               Survey_Type, Survey_MultiPart,
                               Survey_Duplicate, Survey_Primary, 
                               Survey_Complete, Date, year, 
                               month, Start_Time, Species_Code, Species_Unit, Nest_Status, 
                               Unit_Count, Egg_Count, Chick_Count, Observer,
                               Obs_Coords, Obs_Notes, Survey_Notes, Wind_Direction, Wind_Speed, Air_Temp_F, Cloud_Perc, Tide_Stage)  
    
    ### export to use in R viz and for R package
    if (export == TRUE) {
      write.table(nest_surveys_raw, "Data/nest_surveys_raw.csv", sep=",", row.names= FALSE)
      save(nest_surveys_raw, file = "Data/nest_surveys_raw.RData")
    }
  
  return(nest_surveys_raw)
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
