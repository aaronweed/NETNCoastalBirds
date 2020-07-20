#' @include GetIncubationData.R GetSurveyData.R

#' @title sum incubation surveys
#'
#' @import dplyr 
#' @importFrom RODBC odbcConnect sqlFetch odbcClose
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' 
#' @description Brings in the raw incubation survey data from \code{\link{GetIncubationData}} and
#'  summarizes the data for plotting and analysis of DCCO and Gulls (no terns). Summarizes counts by day or by year 
#'  (sum, mean, max, and min of daily surveys) from the primary survey conducted by the lead biologist.  
#'  If you specify  \code{ByObserver}= \code{TRUE} and \code{time} = "date counts of all surveys will be summed by observer.
#' @section Warning:
#' Unless df or survey_date are specified, the user must have an Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param df Dataframe. Defaults to \code{NULL} which requires ODBC connection to generate df. 
#' Otherwise, provide a \code{data.frame} similar to \code{\link{GetIncubationData}}. 
#' @param time Character string equal to "date" or "year". Value must be provided; there is
#' no default. Choose to summarize counts by "date" or "year". Summing by date will sum counts across 
#' segments of each island for each date. When \code{time= year}, summarizes counts (mean, max, min, and sum) across all surveys conducted 
#' in that year. Note that some surveys were repeated in the same year so mean is best to use for reporting because it accounts for seasonal variation in repeat counts. 
#' @param species To subset data by species, use  "DCCO","GBBG","HERG". Defaults
#' to providing output for all species.
#' @param output Character string equal to "graph" or "table". 
#' Defaults to long format (output= "graph") ready for ggplot and the \code{\link{PlotBirds}}
#' function. For wide format use "table".
#' @param ByObserver Character string equal to \code{"yes"} or \code{"no"}.
#' If "yes" and \code{time} = "date", the function will output the survey data counted by each observer for 
#' each island segment on each date. Sums counts across multiple observations by same 
#' observer at each segment. Defaults to "no".
#' @param segment Logical. To summarize data at the survey (island-segment) scale (\code{TRUE}) or island-scale (\code{FALSE})
#'  Defaults to \code{FALSE}.
#' @param survey_data Path to data frame with survey data. Deafults to generating from MS Access database via RODBC.
#' @return Returns a \code{data.frame} with the raw and effort-adjusted counts of Gulls and DCCO incubating nests observed 
#' during boat-based incubation surveys per island, life stage, and time. 
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm} 
#' @examples  
#' SumIncubation(time= "year", species = "DCCO", output = "graph")
#' SumIncubation(time= "date", species = "DCCO", output = "graph")
#' SumIncubation(time= "date", ByObserver = "yes")
#' @export
#' 
#
SumIncubation <- function(df = NULL, time, survey_data = NULL,
                          species = NA, output = "graph", ByObserver = "no", segment= FALSE) {
  
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("Hmisc", quietly = TRUE)){
    stop("Package 'Hmisc' is needed for this function to work. Please install it.", call. = FALSE)
  } 
  
  # this function summarizes the number of adults on nests per island, year, and by observer
  
  if (is.null(df)) {
    df <- GetIncubationData() # import data from the database if needed
    #head(df)
  }
  ## if  Survey data aren't input by user, pull from database
  if (is.null(survey_data)){
    survey_data <- GetSurveyData(x, survey = "Incubation", 
                                 species = {if(!anyNA(species)) species else NA})
  } 
  
  
  # Setup and create molten dataframe
  #############################################################################
  # subset by species if provided 
  if(!anyNA(species)) df <- df[df$Species_Code %in% species, ] 
  
  ### Subset by Islands and by species
  
  Isls<-c ("Calf","Green","Little Calf","Middle Brewster",
           "Outer Brewster", "Shag Rocks", "Spinnaker")# removed Little Brewster and The Graves b/c 0 counts 
  
  # exclude Terns
  df<-df %>%  
    dplyr::filter(!Species_Code %in% c("LETE","COTE")) %>% 
    dplyr::filter(Island %in% Isls) %>% droplevels() # grab the Outer Island loop
  
  ### Sum data across each segement as raw and effort-adjusted numbers by observer
  
  if (time == "date" & ByObserver =="yes") {
    graph.final <- df %>%
      group_by(Island, Segment, Date,month, year, Species_Code, Survey_Type, Survey_Primary,
               Survey_Duplicate, Survey_Complete, Observer) %>% 
      dplyr::summarise(value = sum(Unit_Count, na.rm=TRUE)) %>% ## sum counts across observers
      dplyr::left_join(., survey_data, 
                       by=c("Species_Code","Island","Segment","Survey_Type")) %>% ## append survey effort per segment
      dplyr::mutate(valuePerSurveySize = round(value/(Survey_Size)*1000000,3)) %>% # standardize counts by survey effort
      dplyr::mutate(Survey_Size = Survey_Size/1000000) %>% # added in case I want to scale to other units
      tibble::add_column(Survey_Units = "km2") %>% # denote what survey effort units are reported
      dplyr::select(Species_Code, Island, Segment, time = Date, month, year, Survey_Type, Survey_Primary,
                    Survey_Duplicate, Survey_Complete, value, valuePerSurveySize,Survey_Size, Survey_Units, Observer)
    return(graph.final)
  }else{
    
    df.melt <- df %>%
      dplyr::select(Island, Segment, Date, year, month, Survey_Type, Survey_Primary,
                    Survey_Duplicate, Survey_Complete, Species_Code,Observer, Unit_Count) %>%
      dplyr::filter(Survey_Primary == "Yes" ) %>% # grab only the records from the primary survey to avoid counting multi-obs of same event
      #dplyr::filter(Survey_Duplicate == "No" ) %>% # grab only the records from the first survey if repeated
      tidyr::gather(variable, value, -Island, -Segment, -Date, -year, -month,-Survey_Type,  -Survey_Primary, 
                    -Survey_Duplicate, -Survey_Complete, -Observer, -Species_Code) %>% 
      dplyr::mutate(variable = NULL)
    # head(df.melt)
    
  }
  #######################################
  ## Sum the number of adults observed on nests
  #################################################
  
  ########### Sum counts per date for all surveys counted on the same island across segements  #######################################
  # Note that in some years (mainly 2007-2009) there can be more than one primary survey per day for a segment (Little Calf and Calf). This is because of how the data 
  # were translated from past recording at the island scale to how we handle data in the segments in the current database BE. 
  # These are NOT to be considerd duplicate surveys but complomete the survey for that island so they should be summed together. 
  #######################################
  if (time == "date") {
    
    # count the primary surveys per day
    df.melt %>% group_by(Species_Code, Island, Segment,Date,Survey_Primary, Observer) %>% tally() %>% View()
    
    SumBySegment <- df.melt %>% 
      group_by(Species_Code, Island, Segment, Date, month, year, Observer) %>% 
      dplyr::summarise(value = sum(value, na.rm = TRUE), surveys=n()) %>% # sum per segment when >1 primary surveys per date. 
      dplyr::rename(time = Date) %>% 
      dplyr::mutate(stat ="sum")
      
    
    ## Sum the number of adults on each date across all islands
    ### Calculate for all Islands
    SumByBOHA <- df.melt %>% 
      group_by(Species_Code,  Date, month, year) %>% 
      dplyr::summarise(value = sum(value, na.rm = TRUE), surveys= n()) %>% #  sum per segment when >1 primary surveys per date.
      tibble::add_column(Island = "All Islands", Segment="All") %>% 
      dplyr::rename(time = Date) %>% 
      dplyr::mutate(stat ="sum")
  }
  
  ############# Sum counts per year or across all surveys per island across segements
  # Note that there can be more than one primary survey per year
  # there are also >1 (mainly 2) surveys on the same day by same obs at Little Calf- All, so these need to be summed first prior to
  # summarizing per year so that daily mean/max is correctly calculated
  # there are also surveys with 0 obs on the first survey of the year but follow-up surveys have >1; taking the mean across successive dates
  # isn't appropriate because the 0 surveys were "too early", removing these 0 from mean calc
  # If CLT is on a survey, use her count. If there are >1 surveys per year by CLT take the maximum count from those surveys for the annual estimate. 
  # If CLT isn’t on the survey, take the average of the count from the skilled obs (skill level >3) in the boat or take the average of their maximum counts when >1 surveys per year
  
  #######################################
  # count the primary surveys per year
  df.melt %>% group_by(Species_Code, Island, Segment,year,Survey_Primary, Observer) %>% tally() %>% View()
  
  if (time == "year") {
    yrs_NoCLT<-c("2009","2010","2012")# 3 years Carol wasn't an observer b/c she was taking photos
    
    CLTByDay <- df.melt %>% filter( stringr::str_detect(Observer, 'CLT')) %>%  # extract Carol's surveys
      group_by(Species_Code, Island, Segment,year, Date, Observer) %>% ## first sum by date to account for multiple surveys per day (Little Calf- All)
      dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% # get daily totals by observer
      
      
    OthersByDay<- df.melt %>% filter(year %in% yrs_NoCLT) %>% 
      group_by(Species_Code, Island, Segment,year, Date, Observer) %>% ## first sum by date to account for multiple surveys per day (Little Calf- All)
      dplyr::summarise(value = sum(value, na.rm = TRUE))  # get daily totals by observer
      
    
      
      group_by(Species_Code, Island, Segment, year) %>% # now summarize daily data by year to calc daily mean/ max and overall sum
      dplyr::summarise(sum = sum(value, na.rm = TRUE), mean= round(mean(value[value!=0],na.rm = TRUE),2),
                       max=max(value, na.rm = TRUE), min= min(value, na.rm = TRUE), surveys=n()) %>% 
      dplyr::mutate(mean = if_else(is.nan(mean), 0,mean)) %>% # convert NaN to 0's for zero-only counts
      tidyr::gather(stat, value,-Species_Code, -Island, -Segment,-year, -surveys) %>% # make data long
      dplyr::rename(time = year)
    
    ## Summarize the number of incubating adults in each year across all islands
    ### Calculate for all Islands
    SumByBOHA <- df.melt %>% 
      group_by(Species_Code, Island, Segment,year, Date) %>% ## first sum by date and segment to account for multiple partial segment surveys per day (Little Calf- All)
      dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% # get daily totals
      group_by(Species_Code, year) %>% # now summarize data by year to calc daily mean/ max and overall sum
      dplyr::summarise(sum = sum(value, na.rm = TRUE), mean= round(mean(value[value!=0],na.rm = TRUE),2),
                       max=max(value, na.rm = TRUE), min= min(value, na.rm = TRUE), surveys=n()) %>% 
      dplyr::mutate(mean = if_else(is.nan(mean), 0,mean)) %>% # conver NaN to 0's for zero-only counts
      tidyr::gather(stat, value,-Species_Code, -year, -surveys) %>% # make data long
      tibble::add_column(Island = "All Islands", Segment="All")  %>% 
      dplyr::rename(time = year)
  }
  
    #################################
  # bind together the results aggregated by time AT the SEGMENT-SCALE
  ## CALCULATE  VALUES PER ISLAND or SEGMENT BASED ON SURVEY-ADJUSTED ESTIMATES 
  ## for islands with >1 segments, sums raw counts and survey area by island and then divides sum counts / sum survey area
  ## for many surveys there is only one segment surveys, but there are a few with >1 segement per island surevyed in a given year
  ## the final df calculates the sum of all counts per island(segment), which are inflated estimates because they include repeat surveys,
  # the sum of the mean, max, and minimum of mutiple counts per island(segment) and year (if time = year). 
  # When time= year, mean values are the best to report because they account for seasonal variation.
  #################################
  AllData <- bind_rows(SumBySegment, SumByBOHA) %>% 
    tibble::add_column(Survey_Type = "Incubation") %>% # add in for correct binding of survey effort
    dplyr::left_join(., survey_data,
                     by=c("Species_Code","Island","Segment","Survey_Type")) %>% ## append survey effort per segment
    {if(segment) group_by(.,Species_Code, Island, Segment, time, Size_Units, stat) else # sum by segment if needed
      group_by(.,Species_Code, Island, time, Size_Units, stat) } %>% ##  summarize data by Island
    dplyr::summarise(value= sum(value), # sum raw counts
                     Survey_Size = sum(Survey_Size)) %>% # , ## sum survey effort per island, 
    dplyr::mutate(valuePerSurveySize = round(value/(Survey_Size)*1000000,3)) %>% # standardize counts by survey effort
    dplyr::mutate(Survey_Size = Survey_Size/1000000) %>% # added in case I want to scale to other units
    tibble::add_column(Survey_Units = "km2", variable = "Incubating Adults") %>% # denote what survey effort units are reported  
    {if(segment) dplyr::select(.,Species_Code, Island, Segment, time, variable, stat, value, valuePerSurveySize,Survey_Size, Survey_Units) else
      dplyr::select(.,Species_Code, Island,  time, variable, stat,value, valuePerSurveySize,Survey_Size, Survey_Units)}%>% 
    inner_join(species_tlu, ., by = "Species_Code") # add species names to data
  
  graph.final <- AllData %>%
    mutate(FullLatinName = as.character(FullLatinName),
           CommonName = as.character(CommonName)) 
  
  if(time  == "year"){
    graph.final<-graph.final %>% 
      mutate(year= time)}else{
        
        graph.final<-graph.final %>% 
          mutate(year= year(time))
      }
  
  # output data for graphing
  
  if(output == "graph") {
    return(graph.final)
    #write.table(graph.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
  }
  
  ### make wide for tabular display
  if(output == "table") {
    table.final <- spread(graph.final, CommonName, value, drop = TRUE) %>% 
      ungroup() %>%  
      mutate(Species_Code = NULL, FullLatinName = NULL)
    #write.table(table.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
    return(table.final)
  }
}