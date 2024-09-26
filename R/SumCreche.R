#' @include GetCrecheData.R GetSurveyData.R

#' @title Sum Common Eider creche surveys
#'
#' @import dplyr 
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' @importFrom plyr mapvalues
#' 
#' @description Imports raw creche survey data and summarizes life stage counts by year or date. Raw counts are also converted to
#' densities.This function currently only sums counts from the primary surveyor conducted by the lead biologist when multiple observer surveys were conducted.
#'  When you specify \code{ByObserver = TRUE} and \code{time = "date"} the  raw counts and associated densities of all surveys are summed by observer.
#' @section Warning:
#' Unless \code{df} is specified, the user must have an Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param time Character string equal to "date" or "year". Summing by date will sum 
#' counts (and associated densities) across segments of each island for each date. Summing by year sums counts 
#' across all surveys conducted in that year. Note that some sites were surveyed more than once in 
#' the same year (primarily terns COTE and LETE). 
#' @param output Character string equal to "graph" or "table". 
#' Defaults to exporting a data frame in long format (output= "graph") ready for ggplot and \code{\link{PlotBirds}}
#'For wide format enter "table" but note this currently just returns raw count values per life stage, not density.
#' @param ByObserver Character string equal to \code{"yes"} or \code{"no"}.
#' If "yes" and \code{time} = "date", the function will output the survey data counted by each observer for 
#' each island segment on each date. Sums counts across multiple observations by same 
#' observer at each segment. Defaults to "no".
#' @param df  Requires dataframe exported from NETN's data package imported via \code{\link{importCBBData}} from view 'qry_Dataset_4_Survey_Creche'. If \code{df} 
#' is \code{NULL}, the user must have an Access backend entered as 'NETNCB' in Windows ODBC manager in order to import from \code{\link{GetCrecheData}}.
#' @param islands Character. Defaults to summarizing counts only within the Outer Islands (Calf, Little Calf, Green,
#'   The Graves, Middle Brewster, Outer Brewster, Shag Rocks and Little Brewster) or a vector of island names can be provided.
#' @param segment Logical. To summarize data at the survey (island-segment) scale (\code{TRUE}) or island-scale (\code{FALSE})
#'  Defaults to \code{FALSE}.
#' @return Returns a \code{data.frame} with the raw and effort-adjusted counts of COEI life stages observed 
#' during boat-based creche surveys per island, life stage, and time. 
#
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples 
#' \dontrun{ 
#' importCBBData(path, zip_name, new_env= TRUE) #creates CBB_TABLES object
#' # CrecheSum(df=CBB_TABLES$qry_Dataset_4_Survey_Creche, time ="date")
#' # CrecheSum(df=CBB_TABLES$qry_Dataset_4_Survey_Creche, time ="year")
#' # CrecheSum(df=CBB_TABLES$qry_Dataset_4_Survey_Creche, time= "date", ByObserver = "yes")
#' }
#' @export

SumCreche<-function(df = NULL, time, survey_data = SurveyEffortBySpecies, segment= FALSE,
                    output = "graph", ByObserver = "no", islands = "outer") {
  # this function summarizes the nymber of adults on nests per island, year, and by observer
  # the function will summarize the data by each island (returns all islands)
  # load in functions, look up tables, and R packages
  
  ## if Creche data aren't inputted by user, pull from database.
  if (is.null(df)){
    df <- GetCrecheData()
  } 
  
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("Hmisc", quietly = TRUE)){
    stop("Package 'Hmisc' is needed for this function to work. Please install it.", call. = FALSE)
  } 
  
  #head(df)
  ## if Creche Survey data aren't input by user, pull from database
  if (is.null(survey_data)){
    survey_data <- GetSurveyData(x, survey = "Creche")
  } 
  
  # Setup and create molten dataframe
  #############################################################################
  
  
  # derive month, year and day columns if direct import of csvs

  df$Date  <- ymd(df$Date) #convert to date
  df$year  <- year(df$Date) #Create year variable
  df$month <- month(df$Date) #Create month variable
  
  
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
    
    df<-df[df$Island %in% islands,] 
    droplevels(df)
  }
  
  ### Sum data across each segment as raw and effort-adjusted numbers by observer 
  
  if (time == "date" & ByObserver =="yes") {
    graph.final <- df %>%
      group_by(Island, Segment,  Date,month, year, Species_Code, Survey_Type, Survey_Primary,
               Survey_Duplicate, Survey_Complete, Species_Unit,Observer) %>%
      dplyr::filter(Species_Unit %in% c("F-Lone", "Chick","F-Tend" )) %>% droplevels() %>% 
      dplyr::summarise(value = sum(Unit_Count, na.rm=TRUE)) %>% ## Sum counts per Island, Segment and Date
      dplyr::inner_join(., SurveyEffortBySpecies, 
                        by = c("Species_Code", "Island", "Segment", "Survey_Type")) %>% ## append survey effort per segment
      dplyr::mutate(valuePerSurveySize = round(value/(Survey_Size/1000),2)) %>% # standardize counts by survey effort
      dplyr::mutate(Survey_Size = Survey_Size/1000) %>% 
      tibble::add_column(Survey_Units = "km") %>% # denote what survey effort units are
      dplyr::select(Species_Code, Island, Segment, Survey_Type, time = Date, month, year, Survey_Primary,
                    Survey_Duplicate, Survey_Complete, Species_Unit, value, valuePerSurveySize,Survey_Size, Survey_Units,Observer)
    
    return(graph.final)
    
  } else {
    
    
    # Only sum observations made by lead biologist, excluding repeat counts
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
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum of each life stage by island segment
      spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide to add female COEI vectors
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
      tibble::add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = Date)%>% 
      dplyr::mutate(stat ="sum")
    
    
  ### Sum by Date across all islands
    
    StageSumByTime<-
      dplyr::group_by(df.melt,Date,Species_Unit) %>% # summarize by Date and Species Unit across ALL islands
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) %>% # calc sum per life stage
      tidyr::spread(Species_Unit, sum, drop= TRUE, fill= 0) %>% # make wide 
      dplyr::mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )%>% 
      tibble::add_column(Island = "All Islands", Segment ="All") %>%
      dplyr::select(Island,Segment, Date, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByTime<-filter(df.melt, Species_Unit == "COEI Ducklings") %>% # sum ducklings and no. of observations
      na.omit() %>% # remove any entries where Group_Count isn't specified
      dplyr::group_by(Date,year,month) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      dplyr::mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) %>% 
      tibble::add_column(Island = "All Islands", Segment ="All") %>%
      dplyr::select(Island,Segment, Date,`Average creche size` )
    
    COEI_ByTime<-left_join(StageSumByTime,CrecheSizeByTime, by= c("Island","Segment", "Date")) %>% 
      dplyr::select(Island,Segment, Date,'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed') %>% 
      tibble::add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = Date)%>% 
      dplyr::mutate(stat ="sum")
  }
    
##### Sum by Island for each year##########
  
  if (time == "year"){  
   
     # summarize counts by Date first for each Island and Species Unit
    StageSumByDay<-
      dplyr::group_by(df.melt, Island,Segment,year, Date, Species_Unit) %>% 
      dplyr::summarise( value= sum(value, na.rm=TRUE)) # calc sum per stage per day
      
      # now create new variable corresponding to the total females (lone + tending)  
    TotFemales<-StageSumByDay %>% 
      filter( Species_Unit == "Adult female COEI alone" | Species_Unit == "Adult female COEI tending")%>% 
      group_by(Island, Segment, year, Date) %>%
      summarise(value= sum(value)) %>% # sum by date
      add_column(Species_Unit = "Total Number of Female COEI Observed")# create new life stage variable name
    
    # bind daily summaries from above and summarize data by year to calc daily mean/max and overall sum
    StageSumBySegment<- bind_rows(StageSumByDay, TotFemales) %>%      
      dplyr::group_by(Island, Segment, year, Species_Unit) %>% 
      dplyr::summarise(sum = sum(value, na.rm = TRUE), mean= round(mean(value,na.rm = TRUE),2),
                       max=max(value, na.rm = TRUE), min= min(value, na.rm = TRUE), surveys=n()) %>% # calc summary stats based on indi. daily surveys
      tidyr::gather(stat, value, -Island, -Segment,-year,-Species_Unit, -surveys) # gather data into one value vector for spread
      
      
    # calculate the average creche size (groupings of ducklings) across all survey dates when counted in discrete groupings
    CrecheSizeBySegment<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
      na.omit() %>% # remove any entries where Group_Count isn't specified
      dplyr::group_by(Island,Segment,year, Date) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      dplyr::mutate(value=  round(Total_Ducklings/No_groups,2)) %>% # calc mean creche size for each date
      group_by(Island, Segment, year) %>% 
      dplyr::summarise(value= round(mean(value, na.rm = TRUE),2), No_groups = sum(No_groups)) %>% #calc daily avg creche size across surveys
      tibble::add_column(Species_Unit="Average creche size", stat= "max") %>% 
      dplyr::select(Island, Segment,year,stat,surveys= No_groups,Species_Unit,value )
    
    # bind life stage counts and creche size calc together
    COEI_BySegment<-bind_rows(StageSumBySegment,CrecheSizeBySegment) %>% select(-surveys) %>% 
      group_by(Island, Segment, year, stat) %>% 
      tidyr::spread(Species_Unit, value,fill=0, drop=T) %>% # make data wide
      dplyr::select(Island,Segment, year, stat, 'Adult female COEI tending','COEI Ducklings' , 'Average creche size', 'Total Number of Female COEI Observed')%>% 
      tibble::add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = year)
    
    
    ### Sum by year across all islands
    
    StageSumByTime<- bind_rows(StageSumByDay, TotFemales) %>%     # bind daily summaries for each Island--Segment from above 
      dplyr::group_by(year, Island, Species_Unit, Date) %>% 
      dplyr::summarise(sum = sum(value, na.rm = TRUE)) %>% # sum counts for each island and life stage by day
      dplyr::group_by(year, Species_Unit, Date) %>% 
      dplyr::summarise(sum2 = sum(sum, na.rm = TRUE)) %>% # sum count across all islands per life stage by day
      dplyr::group_by(year, Species_Unit) %>% 
      dplyr::summarise(sum = sum(sum2, na.rm = TRUE), mean= round(mean(sum2,na.rm = TRUE),2), max = max(sum2, na.rm = TRUE) , min= min(sum2, na.rm = TRUE), surveys=n()) %>% # extract max, min and mean annual counts from daily surveys across  all islands per life stage
      tidyr::gather(stat, value, -year,-Species_Unit, -surveys) %>% # gather data into one value vector for spread
      add_column(Island = "All Islands", Segment ="All")
    
    CrecheSizeByTime<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
      na.omit() %>% # remove any entries where Group_Count isn't specified
      dplyr::group_by(Island,Segment,year, Date) %>% # summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))%>% # sum ducklings and no. of observations
      dplyr::mutate(value=  round(Total_Ducklings/No_groups,2)) %>% # calc mean creche size for each date
      group_by(year) %>% # summarize by year
      dplyr::summarise(value= round(mean(value, na.rm = TRUE),2), No_groups = sum(No_groups)) %>% #calc daily avg creche size across surveys
      tibble::add_column(Species_Unit="Average creche size", stat= "max", Island="All Islands", Segment= "All") %>% 
      dplyr::select(Island, Segment,year,stat,surveys= No_groups,Species_Unit,value )
    
    COEI_ByTime<-bind_rows(StageSumByTime,CrecheSizeByTime) %>% select(-surveys) %>% 
      group_by(Island, Segment, year, stat) %>% 
      tidyr::spread(Species_Unit, value, fill=0, drop=T) %>% # make data wide
      dplyr::select(Island,Segment, year, stat, 'Adult female COEI tending','COEI Ducklings' , 'Average creche size', 'Total Number of Female COEI Observed') %>% 
      tibble::add_column(Species_Code = "COEI") %>% 
      dplyr::rename(time = year)
    
  }
    
  ##################### CALC ISLAND WIDE COUNT SUMMARY PER LIFE STAGE ###### #################
  #### COMBINE Per Island and COUNTS ACROSS ALL ISLANDS AND DETERMINE EFFORT-ADJUSTED COUNTS FOR ALL TIME PERIODS (BY DATE OR YEAR)
    
    graph.final<-bind_rows(COEI_BySegment,COEI_ByTime)%>% # this is the final table output for a selected time period (date or year)
      tibble::add_column(Survey_Type = "Creche") %>% # add in for correct binding of survey effort 
      dplyr::left_join(., SurveyEffortBySpecies, 
                       by = c("Species_Code", "Island", "Segment", "Survey_Type")) %>% ## append survey effort per segment
      tidyr::gather( variable, value, -Species_Code, -Island,-Segment,-time, -Survey_Type,-Survey_Class, -Survey_Size,-Size_Units,-stat) %>% # bring data back together
      {if(segment) group_by(.,Species_Code, Island, Segment, time, Size_Units, stat) else # sum by segment if needed
          group_by(.,Species_Code, Island, time, stat, variable)} %>% ## first summarize data by Island
      dplyr::summarise(value= sum(value, na.rm = TRUE), # sum raw counts per island
                       Survey_Size = sum(Survey_Size, na.rm = TRUE)) %>% # , ## sum survey effort per island,
      dplyr::mutate(valuePerSurveySize = round(value/(Survey_Size/1000),2)) %>% # standardize counts by survey effort
      dplyr::mutate(Survey_Size = Survey_Size/1000) %>% 
      tibble::add_column(Survey_Units = "km") %>% # denote what survey effort units are
      {if(segment) dplyr::select(.,Species_Code, Island, Segment, time, variable, stat, value, valuePerSurveySize,Survey_Size, Survey_Units) else
      dplyr::select(.,Species_Code, Island, time,  variable,stat, value, valuePerSurveySize,Survey_Size, Survey_Units)} %>%
      inner_join(tlu_Species, ., by = "Species_Code") %>% # add species names to data
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
        { if(segment) select(.,Island, Segment,time, variable, stat, value) else
          select(.,Island, time, variable, stat, value)} %>% 
        spread( variable, value, drop = TRUE)
      #write.table(table.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
      return(table.final)
    }
  }
