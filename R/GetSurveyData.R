#' @title Get survey effort per species
#'

#' @importFrom dplyr left_join filter distinct
#' @importFrom lubridate month year
#' @importFrom tibble as_tibble
#' 
#' @description This function returns the survey effort per species for each island segment. 
#' This information can be filtered by species and survey type if desired.
#' 
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param x Denote in parentheses to return all surveys
#' @param species Enter species code as character if you would like to filter by species (e.g., "DCCO")
#' @param survey Enter survey type to filter for species detected within that survey type. Options are "Incubation","Nest", and "Creche".
#' @param DBfile Path to a specified database file. 
#' @param connect Should the function connect to the Access DB? The default 
#' (\code{connect = `ODBC`}) is to try to connect using the Windows ODBC manager. 
#' If the connection is not available or not desired, one can use \code{connect = `Hmisc`}
#' and include a patch to a saved version of the database, or
#' the function can return the saved data from the package (\code{connect = `No`}). 
#' Note the saved data may not be up-to-date.
#' @param DBfile If \code{connect = `Hmisc`}, user must provide a location of the DB from which
#' it will grab the data.
#' @return Returns a \code{data.frame} of survey effort (area and distance searched) by species for each survey event. 
#' 
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples  
#' GetSurveyData(survey= "Nest")
#' GetSurveyData(species ="DCCO")
#' @export

GetSurveyData<-function(x, species = NA, survey = NA, connect = "ODBC", DBfile = NULL) {
  
  if(!requireNamespace("fuzzyjoin", quietly = TRUE)){
    stop("Package 'fuzzyjoin' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("Hmisc", quietly = TRUE)){
    stop("Package 'Hmisc' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  #### Import dataframes from DB to access survey effort by event per species ----
  if  (connect == "ODBC") {
    
   
    con <- RODBC::odbcConnect("NETNCB")# establish connection to DB
    
    event <- RODBC::sqlFetch(con, "tbl_Events")
    Isl_Seg<- RODBC::sqlFetch(con, "tbl_Islands_Segments")
    Seg_Size<-RODBC::sqlFetch(con, "tbl_Segments_Size")
    species_tlu<-RODBC::sqlFetch(con,"tlu_Species")
    
    odbcClose(con)
    ## If connection didn't work, try mdb.get() 
  } else if (connect == "Hmisc") {
    if (is.null(DBfile)) {
      stop("Please specify the database location for this connection option.")
    }
    
    event <- Hmisc::mdb.get(DBfile, tables = "tbl_Events", 
                     mdbexportArgs = '', stringsAsFactors = FALSE)
    Isl_Seg <- Hmisc::mdb.get(db_path, tables="tbl_Islands_Segments", 
                         mdbexportArgs = '', stringsAsFactors = FALSE)
    Seg_Size <- Hmisc::mdb.get(db_path, tables="tbl_Segments_Size", 
                         mdbexportArgs = '', stringsAsFactors = FALSE)
    species_tlu <- Hmisc::mdb.get(db_path, tables="tlu_Species", 
                         mdbexportArgs = '', stringsAsFactors = FALSE)
    event <- clear.labels(event)
    Isl_Seg <- clear.labels(Isl_Seg)
    Seg_Size <- clear.labels(Seg_Size)
    species_tlu <- clear.labels(species_tlu)
    
    ## The names are imported differently using mdb.get().
    ## Replace "." with "_"
    names(event) <- gsub("\\.", "_", names(event))
    names(Isl_Seg) <- gsub("\\.", "_", names(Isl_Seg))
    names(Seg_Size) <- gsub("\\.", "_", names(Seg_Size))
    names(species_tlu) <- gsub("\\.", "_", names(species_tlu))
  } else if (connect == "No") {
    return(data(seg_event))
  } else {
    stop("connect must be ODBC, Hmisc, or No.")
  }
  
  
    
    # drop unneeded species
    species_tlu<-filter(species_tlu, !Species_Code %in% "ROTE" & !Species_Code %in% "PIPL") %>% 
      as_tibble()
    
    ##### Join together information on segments to bring in area/distance surveyed per island. ----
    Spec_Isl<- 
      left_join(Isl_Seg, Seg_Size, by= c(pk_SegmentID= "fk_SegmentID"))%>% # joins tables to create table of survey effort per segement
      select(Island, Segment, Survey_Class, Survey_Type, Target_Spp, Survey_Size, Size_Units, Active) %>% 
      left_join(event,., by=c("Survey_Class", "Survey_Type" , "Island","Segment")) %>% as_tibble() %>% # add on segment survey data
      fuzzyjoin::regex_inner_join(.,species_tlu, by=c(Target_Spp = "TargetSpp_Group")) %>%   #Add species codes to survey effort to remove target species grouping for later joining
      {if (!anyNA(species)) filter(.,Species_Code %in% species) else .} %>% # filter by selected species, defaults to all species
      {if (!anyNA(survey)) filter(.,Survey_Type %in% survey) else .} %>% # filter by selected survey, defaults to all 
      select(Species_Code, Island, Segment, Survey_Class, Survey_Type,Survey_Size, Size_Units) %>% 
      droplevels() %>% distinct() 
    
    ## sum survey sizes across BOHA to append to All island summaries
    
    All_Isl<- Spec_Isl %>%
      dplyr::group_by(Species_Code,Survey_Class,Survey_Type) %>% 
      dplyr::summarise(Survey_Size = sum(Survey_Size)) %>% 
      dplyr::mutate(Size_Units = ifelse(Survey_Type %in% "Creche","m","m2")) %>% 
      tibble::add_column(Island = "All Islands", Segment= "All")
      
    # bind together dfs
    
    seg_event <-bind_rows(Spec_Isl,All_Isl) %>% 
      select(Species_Code, Island, Segment, Survey_Class, Survey_Type,Survey_Size, Size_Units)
    
  return(seg_event)
}