#' @include GetCrecheData.R
#' @include GetNestData.R
#' @include GetIncubationData.R
#' @include GetAMOYData.R

#' @title View survey matrix
#'
#'  
#' @importFrom dplyr summarise mutate filter arrange group_by
#' @importFrom tidyr spread
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column as_tibble
#' 
#' @description Constructs matrix showing history of survey for particular surveys
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager if df = NULL.
#'
#' @param survey A character vector. Accepts "Nest", "Creche", or "Incubation". There is no default and this must be denoted.
#' 
#' @param df A data frame of the surveys. Supply argument when connection to database 
#' is not available. If survey is "creche", then must be data frame created from 
#' \code{GetCrecheData} function. If survey is "Incubation", then must be data frame created from the
#' \code{GetIncubationData} function. If survey is "Nest", then must be data frame created from the
#' \code{GetNestData} function.
#' @param island A  vector of island names. To view summaries across all islands, 
#' "All Islands"
#' @param species  A  vector of species name codes, e.g. "BCNH"
#' @param year Calendar year(s) to view effort by. Useful when wanting to view seasonal surveys with time = "Date".
#' survey data in a year.
#' @param time Select whether to show effort by year ("year) or for individual dates ("Date"). Defaults to "year". 
#'
#' @examples
#' GGetSurveyMat(survey ="Nests", species ="COEI", year = 2009, time="Date")
#' GetSurveyMat(survey ="Creche",  year = 2007:2011)
#' GetSurveyMat(survey ="Incubation",  year = 2007, time="Date")
#' 
#' 
#' @return This function returns a matrix showing dates of surveys for 
#' specified arguments indicated as "X".
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @export

GetSurveyMat <- function(survey = NA, df= NULL, island=NA, year= NA, species=NA, time = "year") {
  
  
   if(is.null(df)){
    
  if(species == "AMOY") 
    df <- GetAMOYData() 
  
  if(survey == "Nest") 
    df <- as.data.frame(GetNestData())
  
  if(survey == "Incubation") 
    df <- as.data.frame(as.data.frame(GetIncubationData()))
  
  if(survey == "Creche") df <- as.data.frame(GetCrecheData())
    }
  
  if(!anyNA(species)) df <- df[df$Species_Code %in% species, ]
  
  if(!anyNA(island)) df <- df[df$Island %in% island, ]
  
  if(!anyNA(year)) df <- df[df$year %in% year, ] # for subsetting df ByDate
  
  if(!anyNA(survey)) df <- df[df$Survey_Type %in% survey, ] # for subsetting df by survey type
  
    
  df.wide <- df %>%
    group_by(Species_Code, Island, Segment, Survey_Type, time= {if(time == "year") year else Date}) %>% 
    summarise(value = n())  %>%  # collapse by time
    rename(Survey = Survey_Type) %>% 
    mutate(value = "X") %>% # replace value with text
    spread(time,value,drop= TRUE, fill= "-") %>% 
    filter(!Island %in% "All Islands") %>% 
    arrange(Species_Code,Island, Segment)

  return(df.wide)
}

