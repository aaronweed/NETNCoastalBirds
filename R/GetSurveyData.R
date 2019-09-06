#' @title Get survey effort per species
#'

#' @importFrom dplyr left_join filter distinct
#' @importFrom lubridate month year
#' @importFrom fuzzyjoin regex_inner_join
#' 
#' @description This function returns the survey effort per species for each island segment. 
#' This information can be filtered by species and survey type if desired.
#' 
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param x Denote in parentheses to return all surveys
#' @param species Enter species code as character if you would like to filter by species (e.g., "DCCO")
#' @param survey Enter survey type to filter for species detected within that survey type. Options are "Incubation","Nest", and "Creche".
#' @param ODBC_connect Should the function connect to the Access DB? The default (TRUE) is to 
#' try to connect using the Windows ODBC manager to get survey data. If the connection is not available or not desired, 
#' the function can return the saved data from the package. 
#' @return Returns a \code{data.frame} of survey effort (area and distance searched) by species for each survey event. 
#' 
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples  
#' GetSurveyData(survey= "Nest")
#' GetSurveyData(species ="DCCO")
#' @export

GetSurveyData<-function(x,species = NA, survey = NA,ODBC_connect = TRUE){
  
  #### Import dataframes from DB to access survey effort by event per species ----
  if  (ODBC_connect == TRUE) {
    con <- odbcConnect("NETNCB")# establish connection to DB
    
    event <- sqlFetch(con, "tbl_Events")
    Isl_Seg<- sqlFetch(con, "tbl_Islands_Segments")
    Seg_Size<-sqlFetch(con, "tbl_Segments_Size")
    species_tlu<-sqlFetch(con,"tlu_Species")
    
    odbcClose(con)
    
    # drop unneeded species
    species_tlu<-filter(species_tlu, !Species_Code %in% "ROTE" & !Species_Code %in% "PIPL") %>% 
      tbl_df()
    
    ##### Join together information on segments to bring in area/distance surveyed per island. ----
    Spec_Isl<- 
      left_join(Isl_Seg, Seg_Size, by= c(pk_SegmentID= "fk_SegmentID"))%>% # joins tables to create table of survey effort per segement
      select(Island, Segment, Survey_Class, Survey_Type, Target_Spp, Survey_Size, Size_Units, Active) %>% 
      left_join(event,., by=c("Survey_Class", "Survey_Type" , "Island","Segment")) %>% tbl_df() %>% # add on segment survey data
      regex_inner_join(.,species_tlu, by=c(Target_Spp = "TargetSpp_Group")) %>%   #Add species codes to survey effort to remove target species grouping for later joining
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
    
  }
  
  if  (ODBC_connect == FALSE) { 
    
    data(seg_event)
  }
  return(seg_event)
}