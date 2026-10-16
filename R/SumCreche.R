#' @include GetCrecheData.R GetSurveyData.R
#'
#' @title Summarize Common Eider creche surveys
#'
#' @description
#' Imports and summarizes Common Eider (*Somateria mollissima*) creche survey
#' data by date or year. Counts are summarized by life stage and may be
#' converted to effort-adjusted densities using survey effort data.
#'
#' By default, summaries are calculated at the island scale. Setting
#' \code{segment = TRUE} produces summaries at the island-segment scale.
#' When \code{ByObserver = "yes"} and \code{time = "date"}, counts are
#' summarized separately for each observer and island segment on each date.
#'
#' When \code{df} is not supplied, raw data are retrieved using
#' \code{\link{GetCrecheData}}, which requires access to the NETNCB database
#' through a configured Windows ODBC connection named "NETNCB".
#'
#' @param df Data frame containing raw creche survey data, typically imported
#'   from the NETN Coastal Birds data package using
#'   \code{\link{importCBBData}}. The data should correspond to
#'   \code{qry_Dataset_4_Survey_Creche}. If \code{NULL}, data are retrieved
#'   using \code{\link{GetCrecheData}}.
#'
#' @param time Character string specifying the temporal summary: \code{"date"}
#'   or \code{"year"}. With \code{"date"}, counts are summarized across
#'   segments of each island for each survey date (or separately by segment
#'   when \code{segment = TRUE}). With \code{"year"}, counts are summarized
#'   across surveys conducted in each year. Some sites may be surveyed more
#'   than once in a year.
#'
#' @param survey_effort Required data frame containing survey-effort
#'   information used to calculate effort-adjusted densities. It is imported
#'   from the NETN Coastal Birds data package using
#'   \code{\link{importCBBData}}. The data should correspond to
#'   \code{qry_Dataset_#_Segments}.
#'
#' @param segment Logical. If \code{TRUE}, summarize at the island-segment
#'   scale; if \code{FALSE}, summarize at the island scale. Defaults to
#'   \code{FALSE}.
#'
#' @param output Character string specifying the output format:
#'   \code{"graph"} or \code{"table"}. The \code{"graph"} format returns
#'   long-format data suitable for \code{ggplot2} and \code{\link{PlotBirds}}.
#'   The \code{"table"} format returns wide-format data, with life stages
#'   represented as columns. Check the function implementation to confirm
#'   whether density values are included in the table output.
#'
#' @param ByObserver Character string specifying whether to summarize by
#'   observer. Use \code{"yes"} to summarize by observer or \code{"no"} to
#'   combine observers, where supported by the selected \code{time} setting.
#'   Defaults to \code{"no"}. Observer-specific summaries are documented for
#'   \code{time = "date"}.
#'
#' @param islands Character vector specifying the islands to include.
#'   Defaults to \code{"outer"}, representing the Outer Islands: Calf,
#'   Little Calf, Green, The Graves, Middle Brewster, Outer Brewster,
#'   Shag Rocks, and Little Brewster. A vector of island names may be
#'   supplied instead.
#'
#' @return A data frame containing summarized Common Eider creche survey
#'   counts and, where calculated, effort-adjusted densities by island or
#'   island segment, life stage, and time.
#'
#' @seealso
#' \code{\link{GetCrecheData}},
#' \code{\link{importCBBData}},
#' \code{\link{PlotBirds}},
#' \url{https://www.nps.gov/im/netn/coastal-birds.htm}
#'
#' @examples
#' \dontrun{
#' # Import the Coastal Birds dataset from a ZIP archive
#' cbb <- importCBBData("data/NETN_CoastalBirds_Dataset_YYYYMMDD.zip")
#'
#' # Summarize creche surveys by date
#' SumCreche(
#'   df = cbb$qry_Dataset_4_Survey_Creche,
#'   time = "date"
#' )
#'
#' # Summarize by year
#' SumCreche(
#'   df = cbb$qry_Dataset_4_Survey_Creche,
#'   time = "year"
#' )
#'
#' # Summarize each observer's surveys by date
#' SumCreche(
#'   df = cbb$qry_Dataset_4_Survey_Creche,
#'   time = "date",
#'   ByObserver = "yes"
#' )
#' }
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' @importFrom plyr mapvalues
#' @export

SumCreche<-function(
    df = NULL, 
    time = c("date", "year"), 
    survey_effort = NULL,
    segment= FALSE,
    output = c("graph", "table"), 
    ByObserver = c("no", "yes"), 
    islands = "outer") {
  
  time <- match.arg(time)
  output <- match.arg(output)
  ByObserver <- match.arg(ByObserver)
  
  if (is.null(survey_effort)) {
    stop("Please provide the segment-event survey effort data.")
  }
  
  #------------------------------------------------------------
  # 1. Get and validate data
  #------------------------------------------------------------
  
  ## if Creche data aren't input by user, pull from database.
  if (is.null(df)){
    df <- GetCrecheData() # import via ODBC connection to MS Access database if df not specified
  } |>
  
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if (is.null(survey_effort)) {
    stop("Please provide the segment-event survey effort data.")
  }
  
   #------------------------------------------------------------
  # 2. Filter and prepare
  #------------------------------------------------------------
  
  # derive month, year and day columns if direct import of csvs

  out<- c("Calf", "Little Calf", "Green", "The Graves", "Middle Brewster", 
          "Outer Brewster", "Shag Rocks","Little Brewster")
  
  df<- df |>
    dplyr::filter(Island %in% out) |> # Sum data to the outer Islands
    dplyr::mutate(
      Island = dplyr::case_when(
        Island == "Roaring Bulls" ~ "The Graves", ### Handle island naming and denote outer island loop
        
        .default = Island ), 
      Date = lubridate::ymd(Date),
      year = lubridate::year(Date),
      month = lubridate::month(Date)
    ) %>% droplevels()
  
  #------------------------------------------------------------
  # 3. Sum multiple daily observations of COEI by different groupings: 
  #    island/segment/date/observer
  #------------------------------------------------------------
  
  if (time == "date" & ByObserver == "yes") {
    graph.final <- df %>%
      group_by(Island, Segment,  Date,month, year, Species_Code, Survey_Type, Survey_Primary,
               Survey_Duplicate, Survey_Complete, Species_Unit,Observer) |>
      dplyr::filter(Species_Unit %in% c("F-Lone", "Chick","F-Tend" )) |> droplevels() |>
      dplyr::summarise(value = sum(Unit_Count, na.rm=TRUE)) |> ## Sum counts per Island, Segment and Date
      dplyr::inner_join(survey_effort, 
                        by = c("Species_Code", "Island", "Segment", "Survey_Type")) |>## append survey effort per segment
      dplyr::mutate(valuePerSurveySize = round(value/(Survey_Size/1000),2)) |># standardize counts by survey effort
      dplyr::mutate(Survey_Size = Survey_Size/1000) |>
      tibble::add_column(Survey_Units = "km") |># denote what survey effort units are
      dplyr::select(Species_Code, Island, Segment, Survey_Type, time = Date, month, year, Survey_Primary,
                    Survey_Duplicate, Survey_Complete, Species_Unit, value, valuePerSurveySize,Survey_Size, Survey_Units,Observer) |>
      # change Species_Unit levels
      dplyr::mutate(Species_Unit = case_when(Species_Unit == "Chick" ~ "COEI Ducklings", Species_Unit == "F-Lone" ~ "Adult female COEI alone",
                                      Species_Unit == "F-Tend" ~ "Adult female COEI tending"))
                                        
    return(graph.final)
    
  } else {
    
        # Only sum observations made by lead birders, excluding repeat counts
    
    df.melt <- df %>%
      dplyr::select(Island, Segment, Date, year, month, 
                    Survey_Primary, Group_Count, 
                    Species_Unit, value= Unit_Count) |>
      dplyr::filter(Survey_Primary == "Yes" ) |> # grab only the records from the primary survey to avoid counting multi-obs of same event
      mutate(variable = NULL,
      Group_Count = ifelse(Group_Count == 999, NA, Group_Count), # rename missing/unknown observations
      ValuePerGroup = round(value/Group_Count, 2),  # if needed, calc the no. of each life stage per group for aggregated counts
      Species_Unit= dplyr::case_when(
        Species_Unit == "Chick"   ~ "COEI Ducklings",
        Species_Unit == "F-Lone"  ~ "Adult female COEI alone",
        Species_Unit == "F-Tend"  ~ "Adult female COEI tending",
        .default = Species_Unit))
    
    df.melt$ValuePerGroup[is.nan(df.melt$ValuePerGroup)] = 0 # force NaN's to 0
    
  }
  
  ########################################################################################################################################################
  ############################### Create tabular summary of counts per life stage PER DATE #####################
  ########################################################################################################################################################
  # this will only summarize results from the primary survey
  
  ### Sum by Island for each date
  if (time == "date") {
    StageSumBySegment<-
      group_by(df.melt, Island,Segment, Date,Species_Unit) |># summarize all life stages by Island, Segment,Date and Species Unit
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) |># calc sum of each life stage by island segment
      tidyr::pivot_wider(names_from = Species_Unit, values_from= sum, values_fill = 0) |> # make wide to add female COEI vectors
      dplyr::mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )|>
      dplyr::select(Island, Segment,Date,'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    # determine creche size: note that in some surveys we can't estimate creche size because of the way ducklings were tallied.
    CrecheSizeBySegment<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%# only sum chicks to get creche size
      na.omit() |># remove any entries where Group_Count isn't specified
      group_by(Island,Segment, Date) |># summarize by Island, Segment and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))|># sum ducklings and no. of observations
      dplyr::mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) |>
      dplyr::select(Island, Segment,Date,`Average creche size` )
  
    COEI_BySegment<-left_join(StageSumBySegment,CrecheSizeBySegment, by= c("Island","Segment", "Date")) |># bind tables 
      dplyr::select(Island,Segment, Date, 'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed')|>
      tibble::add_column(Species_Code = "COEI") |>
      dplyr::rename(time = Date)|>
      dplyr::mutate(stat ="sum")
    
    
  ### Sum by Date across all islands
    
    StageSumByTime<-
      dplyr::group_by(df.melt,Date,Species_Unit) |># summarize by Date and Species Unit across ALL islands
      dplyr::summarise( sum= sum(value, na.rm=TRUE)) |># calc sum per life stage
      # tidyr::spread(Species_Unit, sum, drop= TRUE, fill= 0) |># make wide 
      tidyr::pivot_wider(names_from = Species_Unit, values_from= sum, values_fill = 0) |> # make wide to add female COEI vectors
      dplyr::mutate(`Total Number of Female COEI Observed`= `Adult female COEI alone` + `Adult female COEI tending` )|>
      tibble::add_column(Island = "All Islands", Segment ="All") %>%
      dplyr::select(Island,Segment, Date, 'Adult female COEI tending','COEI Ducklings' ,  'Total Number of Female COEI Observed') # grab final columns
    
    CrecheSizeByTime<-filter(df.melt, Species_Unit == "COEI Ducklings") |># sum ducklings and no. of observations
      na.omit() |># remove any entries where Group_Count isn't specified
      dplyr::group_by(Date,year,month) |># summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))|># sum ducklings and no. of observations
      dplyr::mutate(`Average creche size`=  round(Total_Ducklings/No_groups,2)) |>
      tibble::add_column(Island = "All Islands", Segment ="All") %>%
      dplyr::select(Island,Segment, Date,`Average creche size` )
    
    COEI_ByTime<-left_join(StageSumByTime,CrecheSizeByTime, by= c("Island","Segment", "Date")) |>
      dplyr::select(Island,Segment, Date,'Adult female COEI tending','COEI Ducklings' , `Average creche size`, 'Total Number of Female COEI Observed') |>
      tibble::add_column(Species_Code = "COEI") |>
      dplyr::rename(time = Date)|>
      dplyr::mutate(stat ="sum")
  }
    
##### Sum by Island for each year##########
  
  if (time == "year"){  
   
     # summarize counts by Date first for each Island and Species Unit
    StageSumByDay<-
      dplyr::group_by(df.melt, Island,Segment,year, Date, Species_Unit) |>
      dplyr::summarise( value= sum(value, na.rm=TRUE)) # calc sum per stage per day
      
      # now create new variable corresponding to the total females (lone + tending)  
    TotFemales<-StageSumByDay |>
      filter( Species_Unit == "Adult female COEI alone" | Species_Unit == "Adult female COEI tending")|>
      group_by(Island, Segment, year, Date) %>%
      summarise(value= sum(value)) |># sum by date
      add_column(Species_Unit = "Total Number of Female COEI Observed")# create new life stage variable name
    
    # bind daily summaries from above and summarize data by year to calc daily mean/max and overall sum
    StageSumBySegment<- bind_rows(StageSumByDay, TotFemales) |>     
      dplyr::group_by(Island, Segment, year, Species_Unit) |>
      dplyr::summarise(sum = sum(value, na.rm = TRUE), mean= round(mean(value,na.rm = TRUE),2),
                       max=max(value, na.rm = TRUE), min= min(value, na.rm = TRUE), surveys=n()) |># calc summary stats based on indi. daily surveys
      tidyr::gather(stat, value, -Island, -Segment,-year,-Species_Unit, -surveys) # gather data into one value vector for spread
      
      
    # calculate the average creche size (groupings of ducklings) across all survey dates when counted in discrete groupings
    CrecheSizeBySegment<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
      na.omit() |># remove any entries where Group_Count isn't specified
      dplyr::group_by(Island,Segment,year, Date) |># summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))|># sum ducklings and no. of observations
      dplyr::mutate(value=  round(Total_Ducklings/No_groups,2)) |># calc mean creche size for each date
      group_by(Island, Segment, year) |>
      dplyr::summarise(value= round(mean(value, na.rm = TRUE),2), No_groups = sum(No_groups)) |>#calc daily avg creche size across surveys
      tibble::add_column(Species_Unit="Average creche size", stat= "max") |>
      dplyr::select(Island, Segment,year,stat,surveys= No_groups,Species_Unit,value )
    
    # bind life stage counts and creche size calc together
    COEI_BySegment<-bind_rows(StageSumBySegment,CrecheSizeBySegment) |>select(-surveys) |>
      group_by(Island, Segment, year, stat) |>
      tidyr::spread(Species_Unit, value,fill=0, drop=T) |># make data wide
      dplyr::select(Island,Segment, year, stat, 'Adult female COEI tending','COEI Ducklings' , 'Average creche size', 'Total Number of Female COEI Observed')|>
      tibble::add_column(Species_Code = "COEI") |>
      dplyr::rename(time = year)
    
    
    ### Sum by year across all islands
    
    StageSumByTime<- bind_rows(StageSumByDay, TotFemales) |>    # bind daily summaries for each Island--Segment from above 
      dplyr::group_by(year, Island, Species_Unit, Date) |>
      dplyr::summarise(sum = sum(value, na.rm = TRUE)) |># sum counts for each island and life stage by day
      dplyr::group_by(year, Species_Unit, Date) |>
      dplyr::summarise(sum2 = sum(sum, na.rm = TRUE)) |># sum count across all islands per life stage by day
      dplyr::group_by(year, Species_Unit) |>
      dplyr::summarise(sum = sum(sum2, na.rm = TRUE), mean= round(mean(sum2,na.rm = TRUE),2), max = max(sum2, na.rm = TRUE) , min= min(sum2, na.rm = TRUE), surveys=n()) |># extract max, min and mean annual counts from daily surveys across  all islands per life stage
      tidyr::gather(stat, value, -year,-Species_Unit, -surveys) |># gather data into one value vector for spread
      add_column(Island = "All Islands", Segment ="All")
    
    CrecheSizeByTime<-filter(df.melt, Species_Unit == "COEI Ducklings") %>%
      na.omit() |># remove any entries where Group_Count isn't specified
      dplyr::group_by(Island,Segment,year, Date) |># summarize by Island and date
      dplyr::summarise(Total_Ducklings = sum(value), No_groups = sum(Group_Count))|># sum ducklings and no. of observations
      dplyr::mutate(value=  round(Total_Ducklings/No_groups,2)) |># calc mean creche size for each date
      group_by(year) |># summarize by year
      dplyr::summarise(value= round(mean(value, na.rm = TRUE),2), No_groups = sum(No_groups)) |>#calc daily avg creche size across surveys
      tibble::add_column(Species_Unit="Average creche size", stat= "max", Island="All Islands", Segment= "All") |>
      dplyr::select(Island, Segment,year,stat,surveys= No_groups,Species_Unit,value )
    
    COEI_ByTime<-bind_rows(StageSumByTime,CrecheSizeByTime) |>select(-surveys) |>
      group_by(Island, Segment, year, stat) |>
      tidyr::spread(Species_Unit, value, fill=0, drop=T) |># make data wide
      dplyr::select(Island,Segment, year, stat, 'Adult female COEI tending','COEI Ducklings' , 'Average creche size', 'Total Number of Female COEI Observed') |>
      tibble::add_column(Species_Code = "COEI") |>
      dplyr::rename(time = year)
    
  }
    
  ##################### CALC ISLAND WIDE COUNT SUMMARY PER LIFE STAGE ###### #################
  #### COMBINE Per Island and COUNTS ACROSS ALL ISLANDS AND DETERMINE EFFORT-ADJUSTED COUNTS FOR ALL TIME PERIODS (BY DATE OR YEAR)
    
    graph.final<-bind_rows(COEI_BySegment,COEI_ByTime)|># this is the final table output for a selected time period (date or year)
      tibble::add_column(Survey_Type = "Creche") |># add in for correct binding of survey effort 
      dplyr::left_join(., survey_effort, 
                       by = c("Species_Code", "Island", "Segment", "Survey_Type")) |>## append survey effort per segment
      tidyr::gather( variable, value, -Species_Code, -Island,-Segment,-time, -Survey_Type,-Survey_Class, -Survey_Size,-Size_Units,-stat) |># bring data back together
      {if(segment) group_by(.,Species_Code, Island, Segment, time, Size_Units, stat) else # sum by segment if needed
          group_by(.,Species_Code, Island, time, stat, variable)} |>## first summarize data by Island
      dplyr::summarise(value= sum(value, na.rm = TRUE), # sum raw counts per island
                       Survey_Size = sum(Survey_Size, na.rm = TRUE)) |># , ## sum survey effort per island,
      dplyr::mutate(valuePerSurveySize = round(value/(Survey_Size/1000),2)) |># standardize counts by survey effort
      dplyr::mutate(Survey_Size = Survey_Size/1000) |>
      tibble::add_column(Survey_Units = "km") |># denote what survey effort units are
      {if(segment) dplyr::select(.,Species_Code, Island, Segment, time, variable, stat, value, valuePerSurveySize,Survey_Size, Survey_Units) else
      dplyr::select(.,Species_Code, Island, time,  variable,stat, value, valuePerSurveySize,Survey_Size, Survey_Units)} %>%
      inner_join(tlu_Species, ., by = "Species_Code") |># add species names to data
      mutate(FullLatinName=as.character(FullLatinName),CommonName=as.character(CommonName)) # force as chr
 
  
    if(time  == "year"){
      graph.final<-graph.final |>
        mutate(year= time)}else{
          
          graph.final<-graph.final |>
            mutate(year= year(time), month=month(time))
        }
    
    # output data for graphing
    
    if(output == "graph") {
      return(graph.final)
      #write.table(graph.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
    }
    
    ### make wide for tabular display
    if(output == "table") {
      table.final <- graph.final |>
        { if(segment) select(.,Island, Segment,time, variable, stat, value) else
          select(.,Island, time, variable, stat, value)} |>
        spread( variable, value, drop = TRUE)
      #write.table(table.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
      return(table.final)
    }
  }
