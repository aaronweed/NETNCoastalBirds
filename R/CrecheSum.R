#' @include GetCrecheData.R

#' @title CrecheSum
#'
#' @importFrom dplyr summarise mutate filter  group_by select right_join left_join bind_rows rename
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' @importFrom plyr mapvalues
#' 
#' @description Brings in the raw creche survey data from \code{\link{GetCrecheData}} and summarizes the data by year and date for plotting and analysis
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param x Denote "x" in parentheses to return a summary of all creche surveys by island, across all islands, and date.
#' 
#' @return Returns a \code{list} with the counts of COEI life stages observed during boat-based creche surveys per island, species, and by life stage. The first two \code{list}  elements summarize creche surveys by Date for tabular and graphing display, respectively. The 3rd and 4th \code{list} elements summarize creche surveys by YEAR for tabular and graphing display, respectively.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples  
#' CrecheSum(x)
#' @export
#' 
#

CrecheSum<-function(x){
  # this function summarizes the nymber of adults on nests per island, year, and by observer
  # the function will summarize the data by each island (returns all islands)
  # load in functions, look up tables, and R packages
  
  ## import lookup tables for labeling
  species <- read.csv("./Data/tlu_Species.csv")
  
  df<-GetCrecheData(x)
  #head(df)
  df<-droplevels(df)# get rid of the unneeded Species_Unit levels assoc with nest surveys

  ################DATA MANIPULATION #############
  
  ## create molten data frame (all values are represented for each species_unit*site*time combination)
  # in dplyr
  
  df.melt<-select(df,Island=Island,Segment, Date,year,month, Survey_Primary, Group_Count, Species_Unit, Unit_Count) %>% 
    filter(Survey_Primary == "Yes") %>% # grab only the records from the primary survey to avoid counting multi-obs of same event
    gather(variable, value, -Island,-Segment,-Date,-year,-month, -Survey_Primary, -Group_Count, -Species_Unit) %>% 
    mutate(variable=NULL) %>% 
    mutate(Group_Count= ifelse(Group_Count == 999,NA,Group_Count )) %>% 
    mutate(ValuePerGroup= round(value/Group_Count,2)) # if needed, calc the no. of each life stage per group for aggregated counts
    
   df.melt$ValuePerGroup[is.nan(df.melt$ValuePerGroup)] = 0
   
   #View(df.melt)
  
  # change Species_Unit levels
  df.melt$Species_Unit<-mapvalues(df.melt$Species_Unit, from= c("Chick","F-Lone","F-Tend"), to =c("COEI Ducklings",  "Adult female COEI alone", "Adult female COEI tending"))
  #levels(df.melt$Species_Unit)
  
  
  ########################################################################################################################################################
  ############################### Create tabular summary of counts per life stage PER DATE similar to Carol's report #####################
  ########################################################################################################################################################
  
  
  ### Sum by Island for each date

    StageSumByIsl<-
      group_by(df.melt, Island,Date,year,month,Species_Unit) %>% # summarize by Island, Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      dplyr::select(Island, Date,month, year, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByIsl<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(Island,Date,year,month) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      dplyr::select(Island, Date,month, year,`Average creche size` )
  
    COEI_ByIsl<-left_join(StageSumByIsl,CrecheSizeByIsl, by= c("Island", "Date","month", "year")) %>% 
      dplyr::select(Island, Date,month, year, 'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed')%>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = Date)
    
    
  ### Sum by Date across all islands
    
    StageSumByDate<-
      group_by(df.melt,Date,year,month,Species_Unit) %>% # summarize by Island, Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      add_column(Island = "All Islands") %>%
      dplyr::select(Island, Date,month, year, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByDate<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
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
    
    COEI_ByDate<-right_join(species, TEMP, by= "Species_Code") %>% 
      mutate(FullLatinName=as.character(FullLatinName)) %>% # force as chr
      mutate(CommonName=as.character(CommonName))
  
  ### Manip data for graphing (long format)
    
    COEI_GraphByDate<- gather(COEI_ByDate, variable, value, -Species_Code,-FullLatinName, -CommonName,-Island,-time,-year,-month) 
    
    
    ###### EXPORT DATA #################
    # wide tabular
    
    write.table(COEI_ByDate, "./Data/COEI_Table_ByDate.csv", sep=",", row.names= FALSE)

    write.table(COEI_GraphByDate, "./Data/COEI_creche_GraphByDate.csv", sep=",", row.names= FALSE)
    
    
    
    ########################################################################################################################################################
    ############################### Create tabular summary of counts per life stage PER YEAR similar to Carol's report #####################
    ########################################################################################################################################################
    
    
    ### Sum by Island for each year
    
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
    
    COEI_ByYr<-right_join(species, TEMP2, by= "Species_Code")%>% 
      mutate(FullLatinName=as.character(FullLatinName)) %>% # force as chr
      mutate(CommonName=as.character(CommonName)) %>% 
      mutate(time = as.numeric(as.character(time)))
    
    ### Manip data for graphing (long format)
    
    COEI_GraphByYr<- gather(COEI_ByYr, variable, value, -Species_Code,-FullLatinName, -CommonName,-Island,-time) 
    
    
    ###### EXPORT DATA #################
    # wide tabular
    
    write.table(COEI_ByYr, "./Data/COEI_Table_ByYear.csv", sep=",", row.names= FALSE)
    
    write.table(COEI_GraphByYr, "./Data/COEI_creche_GraphByYear.csv", sep=",", row.names= FALSE) 
    
    
    
    
return(list(COEI_ByDate, COEI_GraphByDate, COEI_ByYr,COEI_GraphByYr ))
}
