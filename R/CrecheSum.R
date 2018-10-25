#' @include GetCrecheData.R

#' @title Sum creche surveys
#'
#' @importFrom dplyr summarise mutate filter  group_by select right_join left_join inner_join bind_rows rename
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' @importFrom plyr mapvalues
#' 
#' @description Brings in the raw creche survey data from \code{\link{GetCrecheData}} and summarizes the data by year or date for plotting and analysis.Currently only sums counts from the primary survey (Carol's) when repeated surveys were conducted. If you specify an argument to "ByObserver" this will return sum counts of all duplicate surveys by observer. 
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param time Choose to sum counts by "date" or "year". Summing by date will sum counts across segments of each island for each date. Summing by year sums counts across all surveys conducted in that year. Note that some surveys were repeated in the same year. 
#' @param stage To subset data by life stage; options are "Chick","F-Lone","F-Tend"
#' @param output Defaults to long format ready for ggplot. For wide format use "table"
#' @param ByObserver If "yes" will output the survey data counted by each observer for each island segment on each date. Only sums across multiple observations by same observer at each segment. Defaults to "no".
#'  
#' @return Returns a \code{list} with the counts of COEI life stages observed during boat-based creche surveys per island, species, and by life stage. The first two \code{list}  elements summarize creche surveys by Date for tabular and graphing display, respectively. The 3rd and 4th \code{list} elements summarize creche surveys by YEAR for tabular and graphing display, respectively.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples  
#' CrecheSum(time ="date")
#' CrecheSum(time ="date", stage= "Chicks")
#' CrecheSum(time= "date", ByObserver = "yes")
#' @export
#' 
#

CrecheSum<-function(time, stage=  NA, output= "graph", ByObserver = "no"){
  # this function summarizes the nymber of adults on nests per island, year, and by observer
  # the function will summarize the data by each island (returns all islands)
  # load in functions, look up tables, and R packages
  
  ## import lookup tables for labeling
  species <- read.csv("./Data/tlu_Species.csv")
  
  df<-GetCrecheData(x)
  #head(df)
  
  # Setup and create molten dataframe
  ########################################################################################################################################################
  
  if(!anyNA(stage)) df<-df[df$Species_Unit %in% stage,] #  subset by stage if provided argument
  
  df<-droplevels(df)# get rid of the unneeded Species_Unit levels (assoc with nest surveys too)
  
  
  ### Sum data across each segement as raw numbers by observer 
  
  if (time == "date" & ByObserver == "yes"){
    graph.final<- group_by(df,Island,Segment, c_Observer, Date,month, year,Survey_Duplicate, Survey_Complete,Group_Time, Group_Count,Species_Unit) %>% 
      dplyr::summarise( value= sum(Unit_Count, na.rm=TRUE))%>% 
      dplyr::rename(time = Date) %>% 
      inner_join(species_tlu,., by= "Species_Code") # add species names to data
    return(graph.final)
  }else{
  
  df.melt<-select(df,Island=Island,Segment, Date,year,month, Survey_Primary, Group_Count, Species_Unit, Unit_Count) %>% 
    dplyr::filter(Survey_Primary == "Yes") %>% # grab only the records from the primary survey to avoid counting multi-obs of same event
    gather(variable, value, -Island,-Segment,-Date,-year,-month, -Survey_Primary, -Group_Count, -Species_Unit) %>% 
    mutate(variable=NULL) %>% 
    mutate(Group_Count= ifelse(Group_Count == 999,NA,Group_Count )) %>% 
    mutate(ValuePerGroup= round(value/Group_Count,2)) # if needed, calc the no. of each life stage per group for aggregated counts
    
   df.melt$ValuePerGroup[is.nan(df.melt$ValuePerGroup)] = 0
   
   #View(df.melt)
  
  # change Species_Unit levels
  df.melt$Species_Unit<-mapvalues(df.melt$Species_Unit, from= c("Chick","F-Lone","F-Tend"), to =c("COEI Ducklings",  "Adult female COEI alone", "Adult female COEI tending"))
  #levels(df.melt$Species_Unit)
  }
  
  ########################################################################################################################################################
  ############################### Create tabular summary of counts per life stage PER DATE similar to Carol's report #####################
  ########################################################################################################################################################
  # this will only summarize results from the primary survey
  
  ### Sum by Island for each date
  if (time == "date"){
    StageSumByIsl<-
      group_by(df.melt, Island,Date,year,month,Species_Unit) %>% # summarize all life stages by Island, Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      dplyr::select(Island, Date,month, year, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByIsl<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%# only sum chicks to get creche size
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(Island,Date,year,month) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      dplyr::select(Island, Date,month, year,`Average creche size` )
  
    COEI_ByIsl<-left_join(StageSumByIsl,CrecheSizeByIsl, by= c("Island", "Date","month", "year")) %>% # bind tables 
      dplyr::select(Island, Date,month, year, 'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed')%>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = Date)
    
    
  ### Sum by Date across all islands
    
    StageSumByDate<-
      group_by(df.melt,Date,year,month,Species_Unit) %>% # summarize by Date and Species Unit across ALL islands
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      add_column(Island = "All Islands") %>%
      dplyr::select(Island, Date,month, year, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByDate<-filter(df.melt, Species_Unit == "COEI Ducklings") %>% # sum ducklings and no. of observations
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(Date,year,month) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      add_column(Island = "All Islands") %>%
      dplyr::select(Island, Date,month, year,`Average creche size` )
    
    COEI_ByDate<-left_join(StageSumByDate,CrecheSizeByDate, by= c("Island", "Date","month", "year")) %>% 
    dplyr::select(Island, Date,month, year, 'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed') %>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = Date)
  
  #### COMBINE DATA FRAMES  #################
    
    TEMP<-bind_rows(COEI_ByDate,COEI_ByIsl) # this is the final table output

    # add species names to data
    
    COEI_ByDate<-right_join(species_tlu, TEMP, by= "Species_Code") %>% 
      mutate(FullLatinName=as.character(FullLatinName)) %>% # force as chr
      mutate(CommonName=as.character(CommonName))
  
  ### Manip data for graphing (long format)
    
    graph.final<- gather(COEI_ByDate, variable, value, -Species_Code,-FullLatinName, -CommonName,-Island,-time,-year,-month) 
    
  }
    
    ### Sum by Island for each year
  if (time == "year"){  
    StageSumByIslYr<-
      group_by(df.melt, Island,year, Species_Unit) %>% # summarize by Island, Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      dplyr::select(Island, year, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByIslYr<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(Island,year) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      dplyr::select(Island, year,`Average creche size` )
    
    COEI_ByIslYr<-left_join(StageSumByIslYr,CrecheSizeByIslYr, by= c("Island", "year")) %>% 
      dplyr::select(Island, year, 'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed')%>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = year)
    
    
    ### Sum by year across all islands
    
    StageSumByYr<-
      group_by(df.melt,year,Species_Unit) %>% # summarize by Island, Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      add_column(Island = "All Islands") %>%
      dplyr::select(Island, year, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByYr<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(year) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      add_column(Island = "All Islands") %>%
      dplyr::select(Island,  year,`Average creche size` )
    
    COEI_ByYr<-left_join(StageSumByYr,CrecheSizeByYr, by= c("Island", "year")) %>% 
      dplyr::select(Island,  year, 'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed') %>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = year)
    
    #### COMBINE DATA FRAMES  #################
    
    TEMP2<-bind_rows(COEI_ByIslYr,COEI_ByYr) # this is the final table output
    
    # add species names to data
    
    table.final<-right_join(species_tlu, TEMP2, by= "Species_Code")%>% 
      mutate(FullLatinName=as.character(FullLatinName)) %>% # force as chr
      mutate(CommonName=as.character(CommonName)) %>% 
      mutate(time = as.numeric(as.character(time)))
    
    ### Manip data for graphing (long format)
    
    graph.final<- gather(COEI_ByYr, variable, value, -Species_Code,-FullLatinName, -CommonName,-Island,-time) 
    
    
  }
  
  ###### EXPORT DATA #################
  
  if(output == "graph"){
    return(graph.final)
    #write.table(graph.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
  }
  
  ### make wide for tabular display
  if(output == "table"){
    #write.table(table.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
    return(table.final)
  }
  
 
}
