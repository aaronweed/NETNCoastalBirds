#' @include GetCrecheData.R

#' @title Sum creche surveys
#'
#' @import dplyr 
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' @importFrom plyr mapvalues
#' 
#' @description Brings in the raw creche survey data from \code{\link{GetCrecheData}} 
#' and summarizes the data by year or date for plotting and analysis. Currently only 
#' sums counts from the primary survey (Carol's) when repeated surveys were conducted. 
#' If you specify an argument to "ByObserver" this will return sum counts of all 
#' duplicate surveys by observer. 
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param time Choose to sum counts by "date" or "year". Summing by date will sum 
#' counts across segments of each island for each date. Summing by year sums counts 
#' across all surveys conducted in that year. Note that some sites were surveyed repeated in 
#' the same year. 
#' @param output Character string equal to "graph" or "table". 
#' Defaults to long format (output= "graph") ready for ggplot and the \code{\link{PlotBirds}}
#' function. For wide format use "table".
#' @param ByObserver Character string equal to "yes" or "no".  
#' If "yes" will output the survey data counted by each observer for 
#' each island segment on each date. Only sums across multiple observations by same 
#' observer at each segment. Defaults to "no".
#' #' @param df  The user can optionally load the raw creche data from an R object or connect to the 
#' Access database to obtain it. Defaults to NULL, which means the Access database will
#' be used to obtain it.
#'  
#' @return Returns a \code{list} with the counts of COEI life stages observed 
#' during boat-based creche surveys per island, species, and by life stage. 
#' The first two \code{list}  elements summarize creche surveys by Date for tabular 
#' and graphing display, respectively. The 3rd and 4th \code{list} elements summarize 
#' creche surveys by YEAR for tabular and graphing display, respectively.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples  
#' CrecheSum(time ="date")
#' CrecheSum(time ="date", stage= "Chicks")
#' CrecheSum(time= "date", ByObserver = "yes")
#' @export
#' 
#

CrecheSum <- function(time, df = NULL, output= "graph", ByObserver = "no") {
  # function summarizes the nymber of adults on nests per island, year, and by observer
  # the function will summarize the data by each island (returns all islands)
  # load in functions, look up tables, and R packages
  
  ## import lookup tables for labeling
  ## (instead, dataframe of info is read as part of package)
  # species_tlu <- read.csv("./Data/tlu_Species.csv")
  species_tlu <- data("tlu_Species")
  
  ## if Creche data aren't inputted by user, pull from database.
  if (is.null(df)){
    df <- GetCrecheData(x)
    } 
  #head(df)
  
  # Setup and create molten dataframe
  ##################################################################################
  
  # get rid of unneeded Species_Unit levels (assoc with nest surveys too)
  df <- droplevels(df)
  
  
  ### Sum data across each segement as raw numbers by observer 
  
  if (time == "date" & ByObserver == "yes") {
    graph.final <- df %>%
      group_by(Island, Segment, c_Observer, Date, month, year, 
                            Survey_Duplicate, Survey_Complete, Group_Time, 
                            Group_Count, Species_Unit) %>% 
      dplyr::summarise(value = sum(Unit_Count, na.rm=TRUE)) %>% 
      dplyr::rename(time = Date) %>%
      add_column(Species_Code = "COEI") %>% 
      inner_join(species_tlu, ., by= "Species_Code") # add species names to data
    return(graph.final)
  } else {
  
    # Only sum observations made by Carol, excluding repeat counts
  df.melt <- df %>%
    dplyr::select(Island, Segment, Date, year, month, 
                    Survey_Primary, Survey_Duplicate, Group_Count, 
                    Species_Unit, Unit_Count) %>% 
    dplyr::filter(Survey_Primary == "Yes" ) %>% # grab only the records from the primary survey to avoid counting multi-obs of same event
    dplyr::filter(Survey_Duplicate == "No" ) %>% # grab only the records from the first survey if repeated
    gather(variable, value, -Island, -Segment, -Date, -year, -month, 
           -Survey_Primary, -Survey_Duplicate, -Group_Count, -Species_Unit) %>% 
    mutate(variable = NULL) %>% 
    mutate(Group_Count = ifelse(Group_Count == 999, NA, Group_Count )) %>% # rename missing/unknown observations
    mutate(ValuePerGroup = round(value/Group_Count, 2))  # if needed, calc the no. of each life stage per group for aggregated counts
    
    df.melt$ValuePerGroup[is.nan(df.melt$ValuePerGroup)] = 0 # force NaN's to 0
   
   #View(df.melt)
  
  # change Species_Unit levels
  df.melt$Species_Unit <- plyr::mapvalues(df.melt$Species_Unit, 
                                    from = c("Chick", "F-Lone", "F-Tend"), 
                                    to = c("COEI Ducklings", "Adult female COEI alone", 
                                           "Adult female COEI tending"))
  #levels(df.melt$Species_Unit)
  }
  
  
  #######################################################################################
  ### Create table summary of cts per life stage PER DATE (sim to Carol's report) #######
  #######################################################################################
  # this will only summarize results from the primary survey
  
  ### Sum by Island for each date
  if (time == "date") {
    StageSumByIsl <- df.melt %>%
      group_by(Island, Date, year, month, Species_Unit) %>% # summarize all life stages by Island, Date and Species Unit
      dplyr::summarise(sum = sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed` = `Adult female COEI alone` + `Adult female COEI tending` ) %>% 
      dplyr::select(Island, Date, month, year, 'Adult female COEI tending', 
                    'COEI Ducklings', 'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByIsl <- df.melt %>%
      filter(Species_Unit == "COEI Ducklings") %>%# only sum chicks to get creche size
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(Island, Date, year, month) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), 
                       No_groups = sum(Group_Count)) %>% # sum ducklings and no. of observations
      mutate(`Average creche size` = round(Total_Ducklings / No_groups, 2)) %>% 
      dplyr::select(Island, Date, month, year, `Average creche size`)
  
    COEI_ByIsl <- left_join(StageSumByIsl, CrecheSizeByIsl, 
                            by= c("Island", "Date", "month", "year")) %>% # bind tables 
      dplyr::select(Island, Date, month, year, 'Adult female COEI tending', 
                    'COEI Ducklings', `Average creche size`, 
                    'Total Number of Female COEI Observed') %>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = Date)
    
    
  ### Sum by Date across all islands
    StageSumByDate <- df.melt %>%
      group_by(Date, year, month, Species_Unit) %>% # summarize by Date and Species Unit across ALL islands
      dplyr::summarise(sum = sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed` = `Adult female COEI alone` + `Adult female COEI tending` ) %>% 
      add_column(Island = "All Islands") %>%
      dplyr::select(Island, Date, month, year, 'Adult female COEI tending', 
                    'COEI Ducklings',  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByDate <- df.melt %>% 
      filter(Species_Unit == "COEI Ducklings") %>% # sum ducklings and no. of observations
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(Date, year, month) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), 
                       No_groups = sum(Group_Count)) %>% # sum ducklings and no. of observations
      mutate(`Average creche size` = round(Total_Ducklings / No_groups, 2)) %>% 
      add_column(Island = "All Islands") %>%
      dplyr::select(Island, Date, month, year, `Average creche size`)
    
    COEI_ByDate <- left_join(StageSumByDate, CrecheSizeByDate, 
                             by = c("Island", "Date", "month", "year")) %>% 
    dplyr::select(Island, Date, month, year, 'Adult female COEI tending', 
                  'COEI Ducklings', `Average creche size`, 
                  'Total Number of Female COEI Observed') %>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = Date)
  
  #### COMBINE DATA FRAMES  #################
    
    TEMP <- bind_rows(COEI_ByDate, COEI_ByIsl) # this is the final table output

    # add species names to data
    
    table.final <- right_join(species_tlu, TEMP, by = "Species_Code") %>% 
      mutate(FullLatinName = as.character(FullLatinName),
             CommonName = as.character(CommonName))
  
  ### Manip data for graphing (long format)
    graph.final <- gather(table.final, variable, value, -Species_Code, 
                          -FullLatinName, -CommonName, -Island, -time, -year, -month) 
    
  }
    
    ### Sum by Island for each year
  if (time == "year") {  
    StageSumByIslYr <- df.melt %>%
      group_by(Island,year, Species_Unit) %>%  # summarize by Island, Date and Species Unit
      dplyr::summarise(sum = sum(value, na.rm=TRUE)) %>%  # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>%  # make wide 
      mutate(`Total Number of Female COEI Observed` = `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      dplyr::select(Island, year, 'Adult female COEI tending', 
                    'COEI Ducklings', 'Total Number of Female COEI Observed')  # grab final columns
    
    CrecheSizeByIslYr <- df.melt %>% 
      filter(Species_Unit == "COEI Ducklings") %>%
      na.omit() %>%  # remove any entries where Group_Count isn't specified
      group_by(Island, year) %>%  # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), 
                       No_groups = sum(Group_Count)) %>%  # sum ducklings and no. of observations
      mutate(`Average creche size` = round(Total_Ducklings / No_groups, 2)) %>% 
      dplyr::select(Island, year, `Average creche size`)
    
    COEI_ByIslYr <- left_join(StageSumByIslYr, CrecheSizeByIslYr, 
                              by= c("Island", "year")) %>% 
      dplyr::select(Island, year, 'Adult female COEI tending', 'COEI Ducklings', 
                    `Average creche size`, 'Total Number of Female COEI Observed') %>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = year)
    
    
    ### Sum by year across all islands
    
    StageSumByYr <- df.melt %>% 
      group_by(year, Species_Unit) %>%  # summarize by Island, Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>%  # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>%  # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      add_column(Island = "All Islands") %>%
      dplyr::select(Island, year, 'Adult female COEI tending','COEI Ducklings' ,  
                    'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByYr <- df.melt %>% 
      filter(Species_Unit == "COEI Ducklings") %>%
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(year) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>%  # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      add_column(Island = "All Islands") %>%
      dplyr::select(Island,  year,`Average creche size` )
    
    COEI_ByYr <- left_join(StageSumByYr, CrecheSizeByYr, by= c("Island", "year")) %>% 
      dplyr::select(Island,  year, 'Adult female COEI tending','COEI Ducklings' , 
                    `Average creche size`, 'Total Number of Female COEI Observed') %>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = year)
    
    #### COMBINE DATA FRAMES  #################
    
    TEMP2 <- bind_rows(COEI_ByIslYr, COEI_ByYr) # this is the final table output
    
    # add species names to data
    
    table.final <- right_join(species_tlu, TEMP2, by= "Species_Code") %>% 
      mutate(FullLatinName = as.character(FullLatinName),
             CommonName = as.character(CommonName),
             time = as.numeric(as.character(time)))
    
    ### Manip data for graphing (long format)
    
    graph.final <- gather(table.final, variable, value, 
                          -Species_Code, -FullLatinName, -CommonName, -Island, -time) 
  }
  
  ###### EXPORT DATA #################
  
  if(output == "graph") {
    return(graph.final)
    #write.table(graph.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
  }
  
  ### make wide for tabular display
  if(output == "table") {
    #write.table(table.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
    return(table.final)
  }
}
