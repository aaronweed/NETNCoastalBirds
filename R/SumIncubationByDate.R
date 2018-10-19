#' @include GetIncubationData.R

#' @title SumInucubationbyDate
#'
#' @importFrom dplyr summarise mutate filter group_by select right_join bind_rows rename
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' @importFrom plyr mapvalues
#' 
#' @description Brings in the raw creche survey data from \code{\link{GetIncubationData}} and summarizes the data by year and date for plotting and analysis
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param x Denote "x" in parentheses to return a summary of all creche surveys by island, across all islands, and date.
#' 
#' @return Returns a \code{list} with the counts of Gulls, cormorant, and terns observed during boat-based surveys per island, and species. The first  \code{list}  element summarizes incubation surveys by Date for graphing. The second element for tabular summary.
#'@seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm} 
#' @examples  
#' SumInucubationbyDate(x)
#' @export
#' 
#


SumInucubationbyDate<-function(x){
  # this function summarizes the number of adults on nests per island, year, and by observer
  
  df<-GetIncubationData(x)
  #head(df)
  
  ## import lookup tables for labeling
  species <- read.csv("./Data/tlu_Species.csv")
  
  
  ########################################################################################################################################################
  
  # extract DCCO, GBBG, and HERG observations
  
  df.melt<-select(df, Island,Segment,Date,year,month, Survey_Primary,Survey_Duplicate, Survey_Complete,Species_Code, Unit_Count) %>%
    filter(Survey_Primary == "Yes") %>% # grab only the records from the primary survey to avoid counting multi-obs of same event
    gather(variable, value, -Island,-Segment,-Date,-year,-month,-Survey_Primary, -Survey_Duplicate, -Survey_Complete,-Species_Code) %>% 
    mutate(variable = NULL)
  
  #head(df.melt)
  
  #######################################
  ## Sum the number of adults each DATE at each island
  #################################################
  
  SumBySelect<- group_by(df.melt,Island,Species_Code,Date,year,month,Survey_Duplicate, Survey_Complete) %>% 
    dplyr::summarise( value= sum(value, na.rm=TRUE))%>% 
    dplyr::rename(time = Date)
  
  ## Sum the number of adults on each date across all islands
  ### Calculate for all Islands
  SumByBOHA<-group_by(df.melt, Species_Code, Date,year, month,Survey_Duplicate, Survey_Complete) %>% 
    dplyr::summarise( value= sum(value, na.rm=TRUE)) %>% 
    add_column(Island= "All Islands")  %>% 
    dplyr::rename(time = Date)
  
  # bind together
  SumBySelection<-bind_rows(SumBySelect,SumByBOHA) %>% 
    inner_join(species, SumBySelection, by= "Species_Code") # add species names to data
   
  graph.final<- mutate(SumBySelection,FullLatinName=as.character(FullLatinName)) %>% # force as chr
    mutate(CommonName=as.character(CommonName))
  
  
  # output graph data
  
  write.table(graph.final, "./Data/IncubationsurveysGraph_ByDate.csv", sep=",", row.names= FALSE)
  
  ### make wide for tabular display
  
  table.final<-spread(graph.final,CommonName,value,drop= TRUE) %>% ungroup() %>%  mutate(Species_Code= NULL, FullLatinName = NULL)
  
  
  # output tabular  data
  #write.table(table.final, "./Data/IncubationsurveysTable_ByDate.csv", sep=",", row.names= FALSE)
  
  return(list(graph.final,table.final))
  
}