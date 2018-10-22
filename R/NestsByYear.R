#' @include GetNestData.R

#' @title NestsByYear
#'
#' @importFrom RODBC odbcConnect sqlFetch odbcClose 
#' @importFrom dplyr summarise mutate filter  group_by select inner_join bind_rows rename
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' @importFrom plyr mapvalues
#' @importFrom forcats fct_collapse
#' 
#' @description Brings in the raw ground-based nest survey data from  \code{\link{GetNestData}} and summarizes it by Year for plotting and analysis.
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param x Denote in parentheses to return a summary of all ground-based surveys by species, island, across all islands, and year.
#'
#' @return Returns a \code{list}  with the counts of nests and thier contents (chicks or eggs). The first \code{list}  element summarizes nest surveys by Date for graphing. The second element for tabular summary.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples 
#' NestsByYear(x)
#' @export


NestsByYear<-function(x){
  # counts the number of nests, chicks or eggs (type) per factor level (species, island, segment, date)
  # type inputs are "Chicks", "Nests", "Eggs"
  
  ## import lookup tables for labeling
  species <- read.csv("./Data/tlu_Species.csv")
  
  df<-GetNestData(x) # bring in raw data
  
  df<-droplevels(df)
  
  
  #### Calculate the actual count totals from the raw data. 
  ## In order to get correct counts, need to multiple the nest contents by the number of nests. 
  #For example, Unit_Count= 20, Eggs_count = 2, Chicks_Count= 0 means that there were 20 nests surveyed EACH with 2 eggs and 0 chicks ...
  # NOT 20 nests with a total of 2 eggs.
  # Chick and egg counts ONLY calculated when Nest Status == Normal
  
  df$Nests<-df$Unit_Count# change Unit_Count to Nests to be more specific
  # calculate no. of life stages based on per nest counts
  df$Chicks<-df$Unit_Count* df$Chick_Count
  df$Eggs<-df$Unit_Count* df$Egg_Count
  
  # in dplyr
  
  # create molten df
  df.melt<-select(df,Island,Date, Species_Code, Nest_Status, Nests, Chicks,Eggs) %>%
    mutate(year= year(Date), month= month(Date) ) %>% 
    gather(variable, value, -Island,-Date,-year,-month, -Species_Code,-Nest_Status) 
  
  # filter out nest contents from Normal nests only; denote which nests were directly counted vs estimated
  
  eggs<-filter(df.melt, variable %in% "Eggs" & Nest_Status %in% "Normal") %>% add_column(Count_Method = "Direct Count")
  
  chicks<-filter(df.melt, variable %in% "Chicks" & Nest_Status %in% "Normal") %>% add_column(Count_Method = "Direct Count")
  
  nests<-  filter(df.melt, variable %in% "Nests") %>% # collapse nests status to denote nests counted directly vs estimated (e.g., by flushing adults)
     mutate(Count_Method = fct_collapse(Nest_Status, "Direct Count" = c("Abandoned" , "Depredated","Fledged","Normal","Other","Unknown" ), Estimated = "Estimate"))
    
  # bind together
  
  temp<-bind_rows(eggs,chicks, nests) %>% na.omit()# a few NAs in the chicks table for some reason
  
  #################################
  ## Sum the number of nests, eggs and chicks per YEAR  at each island
  # in dplyr
  ######################################
  SumBySelect<- 
    group_by(temp,Island,Species_Code,year,Count_Method, variable) %>% 
    dplyr::summarise( value= sum(value, na.rm=TRUE)) %>%
    dplyr::rename(time = year)
  
  ## Sum the number of nests, eggs and chicks for each date across all islands
  ### Calculate for all Islands
  SumByBOHA<-
    group_by(temp, Species_Code, year, Count_Method, variable) %>% 
    dplyr::summarise( value= sum(value, na.rm=TRUE)) %>% 
    add_column(Island= "All Islands")%>% 
    dplyr::rename(time = year)  
  
  # bind together rows and then species labeling info
  SumBySelection<-bind_rows(SumBySelect,SumByBOHA) %>% 
    inner_join(species, SumBySelection, by= "Species_Code") %>% # add species names to data
    filter(!Species_Code %in% "AMOY")# remove AMOY coming in for some reason
  
  graph.final<- mutate(SumBySelection,FullLatinName=as.character(FullLatinName)) %>% # force as chr
    mutate(CommonName=as.character(CommonName))
  
  # output graph data
  #write.table(graph.final, "./Data/GroundNestsurveysGraph_ByYear.csv", sep=",", row.names= FALSE)
  
  ### make wide for tabular display
  
  table.final<-spread(graph.final,variable,value,drop= TRUE, fill= 0)
  
  
  # output tabular  data
  #write.table(table.final, "./Data/GroundNestsurveysTable_ByYear.csv", sep=",", row.names= FALSE)
  #write.table(table.final, "~/R/NETN/Coastal Birds/CoastalBirdViz/Data/GroundNestsurveys_ByDate.csv", sep=",", row.names= FALSE)
  
  return(list(graph.final,table.final))
  
}
