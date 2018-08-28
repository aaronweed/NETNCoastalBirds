#' @include GetAMOYData.R
#' @include AMOYPairsByYear.R
#' @include AMOYPairsByDate.R
#' 
#' @title getSurveyMat
#'
#' @importFrom dplyr summarise mutate filter arrange group_by
#' @importFrom tidyr spread
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' 
#' @description Constructs survey matrix to show effort based on the inputs
#'
#' @param survey Dataframe contructed from Access BE. Arguments can be "nest", "creche", "incubation" or "AMOY". If AMOY only returns dates when AMOY mating paris were reported
#' @param island A  vector of island names. To view summariaes across all islands, "All Islands"
#' @param species  A  vector of species name codes, e.g. "BCNH"
#' @param year Calendar year(s) to view data by. Useful when wanting to view seasonal survey data in a year.
#'
#' @details This function produces a graph of species detections over time.
#'
#' @export

getSurveyMat<-function(survey, island=NA, year= NA, species=NA){
  # 
  # source("incub_survey_sumfuncs.R") # COTE, DCCO, GBBG, HERG, LETE
  # source("nest_summaryfunc.R") # summarizes nest survey data for #"BCNH" "COEI" "COTE" "DCCO" "GBBG" "GLIB" "GREG" "HERG" "LETE" "SNEG" "SPSA" "WILL"
  # source("COEI_crech_survey_sum_func.R") # summarizes COEI creche counts per island and all islands
  # 
  if(survey == "nest") df<-as.data.frame(NestSurveyCountsByDate(x)[1])
  
  if(survey == "incubation") df<-as.data.frame(as.data.frame(incub_DCCO_Gull_sumfunc(x)[1]))
  
  if(survey == "creche") df<-as.data.frame(COEI_creche_sumfunc(x)[2])
  
  if(survey == "AMOY") df<-as.data.frame(AMOYPairsByDate(x)[1])
  
  
  
  if(!anyNA(species)) df<-df[df$Species_Code %in% species,]
  
  if(!anyNA(island)) df<-df[df$Island %in% island,]
  
  if(!anyNA(year)) df<-df[df$year %in% year,] # for subsetting df ByDate
  
    
  df.wide<- group_by(df,  CommonName, FullLatinName,Species_Code,Island,time) %>% 
    summarise(value = n())  %>%  # collapse by time
    add_column(Survey = survey) %>% 
    mutate(value = "X") %>% # replace value with text
    spread(time,value,drop= TRUE, fill= "-") %>% 
    filter(!Island %in% "All Islands") %>% 
    arrange(CommonName, FullLatinName,Species_Code,Island, Survey)

  return(df.wide)
}

