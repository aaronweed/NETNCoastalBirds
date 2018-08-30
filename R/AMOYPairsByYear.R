#' @include GetAMOYData.R

#' @title AMOYPairsByYear
#'
#' @importFrom dplyr summarise mutate filter  group_by select  right_join bind_rows rename
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' 
#' @description Summarizes the AMOY mating pair survey data from \code{\link{GetAMOYData}} by Year for plotting and analysis.
#' @section Warning:
#'User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param x Denote "x" in parentheses to return a \code{data.frame} of a summary of AMOY mating pairs.
#'
#' @return Returns counts of the number of mating AMOY pairs per event as a \code{list}; the first element for plotting and the second for tabular display. 
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples  
#' AMOYPairsByYear(x)
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
  #write.table(table.final, "./Data/AMOYPairs_TabularByYear.csv", sep=",", row.names= FALSE)
  
  
  return(list(SumByYear,table.final))
  
  
  
}


