#' @include GetCrecheData.R GetSurveyData.R

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
#' and summarizes the counts by year or date and life stage for plotting and analysis. Currently sums counts from the primary 
#'  survey conducted by the lead biologist when repeated surveys were conducted.  If you specify  \code{ByObserver}=\code{TRUE} 
#'   the  counts of all duplicate surveys will be summed by observer.
#' @section Warning:
#' Unless df is specified, the user must have an Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param time Choose to sum counts by "date" or "year". Summing by date will sum 
#' counts across segments of each island for each date. Summing by year sums counts 
#' across all surveys conducted in that year. Note that some sites were surveyed more than once in 
#' the same year (primarily terns COTE and LETE). 
#' @param output Character string equal to "graph" or "table". 
#' Defaults to long format (output= "graph") ready for ggplot and the \code{\link{PlotBirds}}
#' function. For wide format use "table".
#' @param ByObserver Character string equal to \code{"yes"} or \code{"no"}.
#' If "yes" and \code{time} = "date", the function will output the survey data counted by each observer for 
#' each island segment on each date. Sums counts across multiple observations by same 
#' observer at each segment. Defaults to "no".
#' @param df  The user can optionally load the raw creche data from an R object or connect to the 
#' Access database to obtain it. Defaults to NULL, which means the Access database will
#' be used to obtain it.
#' @param islands Defaults to summarizing counts only within the Outer Islands (Calf, Little Calf, Green,
#'   The Graves, Middle Brewster, Outer Brewster, Shag Rocks and Little Brewster).
#'  
#' @return Returns a \code{data.frame} with the raw and effort-adjusted counts of COEI life stages observed 
#' during boat-based creche surveys per island, life stage, and time. 
#
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples  
#' CrecheSum(time ="date")
#' CrecheSum(time ="year")
#' CrecheSum(time= "date", ByObserver = "yes")
#' @export
#' 
#

CrecheSum<-function(time, df = NULL, survey_data = NULL,
                    output = "graph", ByObserver = "no", islands = "outer") {
  # this function summarizes the nymber of adults on nests per island, year, and by observer
  # the function will summarize the data by each island (returns all islands)
  # load in functions, look up tables, and R packages
  
  ## if Creche data aren't inputted by user, pull from database.
  if (is.null(df)){
    df <- GetCrecheData(x)
  } 
  #head(df)
  ## if Creche Survey data aren't inputted by user, pull from database
  if (is.null(survey_data)){
    survey_data <- GetSurveyData(x, survey = "Creche")
  } 
  
  # Setup and create molten dataframe
  #############################################################################
  
  ### Handle island naming and denote outer island loop
  
  # concatenate Roaring Bulls to The Graves
  df$Island <- plyr::mapvalues(df$Island, 
                               from = c("Roaring Bulls"), 
                               to = c("The Graves"))
  
  # Sum data to the outer Islands
  
  if(islands == "outer"){ 
    
    out<- c("Calf", "Little Calf", "Green", "The Graves", "Middle Brewster", 
            "Outer Brewster", "Shag Rocks","Little Brewster")
    
    df<-df[df$Island %in% out,]
    
    df<-droplevels(df)# get rid of the unneeded Species_Unit levels (assoc with nest surveys too)
    
  }else{
    
    df<-droplevels(df)
  }
  
  
  ### Sum data across each segement as raw and effort-adjusted numbers by observer 
  
  if (time == "date" & ByObserver =="yes") {
    graph.final <- df %>%
      group_by(Island, Segment,  Date,month, year, Species_Code, Survey_Type, Survey_Primary,
               Survey_Duplicate, Survey_Complete, Species_Unit,Observer) %>%
      dplyr::filter(Species_Unit %in% c("F-Lone", "Chick","F-Tend" )) %>% droplevels() %>% 
      dplyr::summarise(value = sum(Unit_Count, na.rm=TRUE)) %>% ## Sum counts per Island, Segment and Date
      dplyr::inner_join(., survey_data, 
                        by = c("Species_Code", "Island", "Segment", "Survey_Type")) %>% ## append survey effort per segment
      dplyr::mutate(valuePerSurveySize = round(value/(Survey_Size/1000),2)) %>% # standardize counts by survey effort
      dplyr::mutate(Survey_Size = Survey_Size/1000) %>% 
      tibble::add_column(Survey_Units = "km") %>% # denote what survey effort units are
      dplyr::select(Species_Code, Island, Segment, Survey_Type, time = Date, month, year, Survey_Primary,
                    Survey_Duplicate, Survey_Complete, Species_Unit, value, valuePerSurveySize,Survey_Size, Survey_Units,Observer)
    
    return(graph.final)
    
  } else {
    
    
    # Only sum observations made by Carol, excluding repeat counts
    df.melt <- df %>%
      dplyr::select(Island, Segment, Date, year, month, 
                    Survey_Primary, Group_Count, 
                    Species_Unit, Unit_Count) %>% 
      dplyr::filter(Survey_Primary == "Yes" ) %>% # grab only the records from the primary survey to avoid counting multi-obs of same event
      gather(variable, value, -Island, -Segment, -Date, -year, -month, 
             -Survey_Primary, -Group_Count, -Species_Unit) %>% 
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
  
  
  ########################################################################################################################################################
  ############################### Create tabular summary of counts per life stage PER DATE similar to Carol's report #####################
  ########################################################################################################################################################
  # this will only summarize results from the primary survey
  
  ### Sum by Island for each date
  if (time == "date") {
    StageSumBySegment<-
      group_by(df.melt, Island,Segment, Date,Species_Unit) %>% # summarize all life stages by Island, Segment,Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      dplyr::select(Island, Segment,Date,'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    # determine creche size: note that in some surveys we can't estimate creche size because of the way ducklings were tallied.
    CrecheSizeBySegment<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%# only sum chicks to get creche size
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(Island,Segment, Date) %>% # summarize by Island, Segment and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      dplyr::select(Island, Segment,Date,`Average creche size` )
  
    COEI_BySegment<-left_join(StageSumBySegment,CrecheSizeBySegment, by= c("Island","Segment", "Date")) %>% # bind tables 
      dplyr::select(Island,Segment, Date, 'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed')%>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = Date)
    
    
  ### Sum by Date across all islands
    
    StageSumByTime<-
      group_by(df.melt,Date,Species_Unit) %>% # summarize by Date and Species Unit across ALL islands
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      add_column(Island = "All Islands", Segment ="All") %>%
      dplyr::select(Island,Segment, Date, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByTime<-filter(df.melt, Species_Unit == "COEI Ducklings") %>% # sum ducklings and no. of observations
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(Date,year,month) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      add_column(Island = "All Islands", Segment ="All") %>%
      dplyr::select(Island,Segment, Date,`Average creche size` )
    
    COEI_ByTime<-left_join(StageSumByTime,CrecheSizeByTime, by= c("Island","Segment", "Date")) %>% 
    dplyr::select(Island,Segment, Date,'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed') %>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = Date)
  }
    
##### Sum by Island for each year##########
  
  if (time == "year"){  
    StageSumBySegment<-
      group_by(df.melt, Island,Segment,year, Species_Unit) %>% # summarize by Island, Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      dplyr::select(Island, Segment,year, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeBySegment<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(Island,Segment,year) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      dplyr::select(Island, Segment,year,`Average creche size` )
    
    COEI_BySegment<-left_join(StageSumBySegment,CrecheSizeBySegment, by= c("Island","Segment", "year")) %>% 
      dplyr::select(Island,Segment, year, 'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed')%>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = year)
    
    
    ### Sum by year across all islands
    
    StageSumByTime<-
      group_by(df.melt,year,Species_Unit) %>% # summarize by Island, Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      add_column(Island = "All Islands", Segment ="All") %>%
      dplyr::select(Island,Segment, year, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByTime<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
      na.omit() %>% # remove any entries where Group_Count isn't specified
      group_by(year) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      add_column(Island = "All Islands", Segment ="All") %>%
      dplyr::select(Island,Segment,  year,`Average creche size` )
    
    COEI_ByTime<-left_join(StageSumByTime,CrecheSizeByTime, by= c("Island", "Segment","year")) %>% 
      dplyr::select(Island, Segment, year, 'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed') %>% 
      add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = year)
    
  }
    
  ##################### CALC ISLAND WIDE COUNT SUMMARY PER LIFE STAGE ###### #################
  #### COMBINE Per Island and COUNTS ACROSS ALL ISLANDS AND DETERMINE EFFORT-ADJUSTED COUNTS FOR ALL TIME PERIODS (BY DATE OR YEAR)
    
    graph.final<-bind_rows(COEI_BySegment,COEI_ByTime)%>% # this is the final table output for a selected time period (date or year)
      tibble::add_column(Survey_Type = "Creche") %>% # add in for correct binding of survey effort 
      dplyr::left_join(., survey_data, 
                       by = c("Species_Code", "Island", "Segment", "Survey_Type")) %>% ## append survey effort per segment
      tidyr::gather( variable, value, -Species_Code, -Island,-Segment,-time, -Survey_Type,-Survey_Class, -Survey_Size,-Size_Units) %>% # bring data back together
      group_by(Species_Code, Island, time, variable) %>% ## first summarize data by Island
      dplyr::summarise(value= sum(value, na.rm = TRUE), # sum raw counts per island
                       Survey_Size = sum(Survey_Size, na.rm = TRUE), n=n()) %>% # , ## sum survey effort per island,
      dplyr::mutate(valuePerSurveySize = round(value/(Survey_Size/1000),2)) %>% # standardize counts by survey effort
      dplyr::mutate(Survey_Size = Survey_Size/1000) %>% 
      tibble::add_column(Survey_Units = "km") %>% # denote what survey effort units are
      dplyr::select(Species_Code, Island, time,  variable, value, valuePerSurveySize,Survey_Size, Survey_Units) %>% 
      inner_join(species_tlu, ., by = "Species_Code") %>% # add species names to data
      mutate(FullLatinName=as.character(FullLatinName),CommonName=as.character(CommonName)) # force as chr
 
  
    if(time  == "year"){
      graph.final<-graph.final %>% 
        mutate(year= time)}else{
          
          graph.final<-graph.final %>% 
            mutate(year= year(time), month=month(time))
        }
    
    # output data for graphing
    
    if(output == "graph") {
      return(graph.final)
      #write.table(graph.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
    }
    
    ### make wide for tabular display
    if(output == "table") {
      table.final <- graph.final %>% 
        select(-Species_Code, -CommonName, -FullLatinName) %>% 
        group_by(Island, )
        spread( CommonName, value, drop = TRUE) %>% 
        ungroup() %>%  
        mutate(Species_Code = NULL, FullLatinName = NULL)
      #write.table(table.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
      return(table.final)
    }
  }
