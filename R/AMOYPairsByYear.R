#' @include GetAMOYData.R

#' @title AMOYPairsByYear
#'
#' @importFrom dplyr summarise mutate filter  group_by select  right_join bind_rows
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' 
#' @description This script brings in the raw AMOY survey data and summarizes the AMOY mating pairs data by Year for plotting and analysis.
#'
#' @param survey Dataframe contructed from Access BE. Arguments can be "nest", "creche", "incubation" or "AMOY". If AMOY only returns dates when AMOY mating paris were reported
#' @param island A  vector of island names. To view summariaes across all islands, "All Islands"
#' @param species  A  vector of species name codes, e.g. "BCNH"
#' @param year Calendar year(s) to view data by. Useful when wanting to view seasonal survey data in a year.
#'
#' @details # Return counts of the number of mating AMOY pairs per event
#'
#' @export


AMOYPairsByYear<-function(x){
  
  ## import lookup tables for labeling
  
  species <- read.csv("./Data/tlu_Species.csv")
  
  
  df<-GetAMOYData(x) # bring in raw data
  
  df<-droplevels(df)
  
  
  # create molten df
  df.melt<- dplyr::filter(df, Species_Unit == "Pair") %>% 
    dplyr::select(Island,Date,Species_Code, Unit_Count) %>%
    dplyr::mutate(year= year(Date), month= month(Date) ) %>% 
    tidyr::gather(variable, value, -Species_Code,-Island,-Date,-year,-month) %>% 
    dplyr::mutate(variable = NULL) 
  
  ### Sum the number of pairs obs per year per island
  SumByIsl<- dplyr::group_by(df.melt,Island,Species_Code,year) %>% 
    dplyr::summarise( value= sum(value, na.rm=TRUE))
  
  
  ### Sum the number of pairs obs per year across all islands
  SumByBOHA<- group_by(df.melt,Species_Code,year) %>% 
    dplyr::summarise( value= sum(value, na.rm=TRUE)) %>% 
    add_column(Island = "All Islands")
  
  SumByYear<-bind_rows(SumByIsl,SumByBOHA)%>% 
    dplyr::right_join(species, ., by= "Species_Code") %>% 
    add_column(variable = "Mating Pairs") %>% 
    dplyr::rename( time= year) %>% 
    dplyr::select(Species_Code,CommonName,FullLatinName,Island, time, variable, value) %>%  # add species names to data
    dplyr::mutate(FullLatinName=as.character(FullLatinName), time= as.numeric(as.character(time))) %>% # force as chr
    dplyr::mutate(CommonName=as.character(CommonName))
  
   # output graph data
  write.table(SumByYear, "./Data/AMOYPairs_GraphByYear.csv", sep=",", row.names= FALSE)
  
  ### make wide for tabular display
  
  table.final<-spread(SumByYear,time,value,drop= TRUE)
  
  
  # output tabular  data
  write.table(table.final, "./Data/AMOYPairs_TabularByYear.csv", sep=",", row.names= FALSE)
  
  
  return(list(SumByYear,table.final))
  
  
  
}


