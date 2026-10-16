#' @include GetIncubationData.R GetSurveyData.R

#' @title Sum Incubation Surveys
#'
#' @import dplyr 
#' @importFrom tidyr spread gather
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' 
#' @description Imports raw incubation survey data and summarizes the data for plotting and analysis of DCCO and Gulls (no terns). Summarizes counts by day or by year 
#'  (sum, mean, max, and min of daily surveys) from the primary survey conducted by the lead biologist.  
#'  If you specify  \code{ByObserver}= \code{TRUE} and \code{time} = "date", nests will be summed by each observer.
#' @section Warning:
#' Unless \code{df} is specified, the user must have an Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param df Requires dataframe exported from NETN's data package imported via \code{\link{importCBBData}} from view "qry_Dataset_2_Survey_Incubation". If \code{df} 
#' is \code{NULL}, the user must have an Access backend entered as 'NETNCB' in Windows ODBC manager in order to import from \code{\link{GetIncubationData}}.
#' @param time Character string equal to "date" or "year". Value must be provided; there is
#' no default. Choose to summarize counts by "date" or "year". Summing by date will sum counts across 
#' segments of each island for each date. When \code{time = year}, summarizes counts (mean, max, min, and sum) across all surveys conducted 
#' in that year. Note that some surveys were repeated in the same year so mean is best to use for reporting because it accounts for seasonal variation in repeat counts. 
#' @param species To subset data by species, use  "DCCO","GBBG","HERG". Defaults
#' to providing output for all species.
#' @param output Character string equal to "graph" or "table". 
#' Defaults to long format (output= "graph") ready for ggplot and the \code{\link{PlotBirds}}
#' function. For wide format use "table".
#' @param ByObserver Character string equal to \code{"yes"} or \code{"no"}.
#' If "yes" and \code{time} = "date", the function will output the survey data counted by each observer for 
#' each island segment on each date. Sums counts across multiple observations by same 
#' observer at each segment. Defaults to "no".
#' @param segment Logical. To summarize data at the survey (island-segment) scale (\code{TRUE}) or island-scale (\code{FALSE})
#'  Defaults to \code{FALSE}.
#' @return Returns a \code{data.frame} with the raw and effort-adjusted counts of Gulls and DCCO incubating nests observed 
#' during boat-based incubation surveys per island, life stage, and time. 
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm} 
#' @examples 
#' \dontrun{ 
#' importCBBData(path, zip_name, new_env= TRUE) #creates CBB_TABLES object
#' SumIncubation(df= CBB_TABLES$qry_Dataset_2_Survey_Incubation, time= "year", species = "DCCO", output = "graph")
#' SumIncubation(df= CBB_TABLES$qry_Dataset_2_Survey_Incubation, time= "date", species = "DCCO", output = "graph")
#' SumIncubation(df= CBB_TABLES$qry_Dataset_2_Survey_Incubation, time= "date", ByObserver = "yes")
#' }
#' @export
#' 

SumIncubation <- function(
    df = NULL,
    time = c("date", "year"),
    species = NULL,
    output = c("graph", "table"),
    survey_effort = NULL,
    ByObserver = c("no", "yes"),
    segment = FALSE
) {
  
  time <- match.arg(time)
  output <- match.arg(output)
  ByObserver <- match.arg(ByObserver)
  
  if (is.null(survey_effort)) {
    stop("Please provide the segment-event survey effort data.")
  }
  
  #------------------------------------------------------------
  # 1. Get and validate data
  #------------------------------------------------------------
  
  if (is.null(df)) {
    df <- GetIncubationData() # import via ODBC connection to MS Access database if df not specified
  }
  
  required_cols <- c(
    "Species_Code", "Island", "Segment", "Date",
    "Survey_Type", "Survey_Primary", "Survey_Duplicate",
    "Survey_Complete", "Observer", "Unit_Count"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  
  #------------------------------------------------------------
  # 2. Filter and prepare
  #------------------------------------------------------------
  
  islands <- c(
    "Calf", "Green", "Little Calf", "Middle Brewster",
    "Outer Brewster", "Shag Rocks", "Spinnaker"
  )
  
  df <- df |> ### Subset by Islands and by species
    dplyr::filter(
      !Species_Code %in% c("LETE", "COTE"), # remove terns from the dataset
      Island %in% islands,
      Survey_Primary == "Yes"
    ) |>
    dplyr::mutate(
      Date = lubridate::ymd(Date),
      year = lubridate::year(Date),
      month = lubridate::month(Date)
    )
  
  if (!is.null(species)) {
    df <- df |>
      dplyr::filter(Species_Code %in% species)
  }
  
  #------------------------------------------------------------
  # 3. Sum multiple daily observations of incubating adults on nest on the same
  #    island/segment/date for each observer
  #------------------------------------------------------------
 
  
  ### Sum data across each segment as raw and effort-adjusted numbers by observer
  
  if (time == "date" & ByObserver =="Yes") {
    graph.final <- df |> 
      filter(Survey_Primary %in% "Yes") |>
      group_by(Island, Segment, Date,month, year, Species_Code, Survey_Type, Survey_Primary,
               Survey_Duplicate, Survey_Complete, Observer) |> 
      dplyr::summarise(value = sum(Unit_Count, na.rm=TRUE)) |> ## sum counts across observers
      dplyr::left_join(., survey_effort, 
                       by=c("Species_Code","Island","Segment","Survey_Type")) |> ## append survey effort per segment
      dplyr::mutate(valuePerSurveySize = round(value/(Survey_Size)*1000000,3)) |> # standardize counts by survey effort
      dplyr::mutate(Survey_Size = Survey_Size/1000000) |> # added in case I want to scale to other units
      dplyr::mutate(Survey_Units = "km2") |> # denote what survey effort units are reported
      dplyr::select(Species_Code, Island, Segment, time = Date, month, year, Survey_Type, Survey_Primary,
                    Survey_Duplicate, Survey_Complete, value, valuePerSurveySize,Survey_Size, Survey_Units, Observer)
    return(graph.final)
  }else{
    
    df.melt <- df |>
      dplyr::select(Island, Segment, Date, year, month, Survey_Type, Survey_Primary,
                    Survey_Duplicate, Survey_Complete, Species_Code,Observer, Unit_Count) |>
      dplyr::filter(Survey_Primary == "Yes" ) |> # grab only the records from the primary survey to avoid counting multi-obs of same event
      #dplyr::filter(Survey_Duplicate == "No" ) |> # grab only the records from the first survey if repeated
      tidyr::gather(variable, value, -Island, -Segment, -Date, -year, -month,-Survey_Type,  -Survey_Primary, 
                    -Survey_Duplicate, -Survey_Complete, -Observer, -Species_Code) |> 
      dplyr::mutate(variable = NULL)
    
  }
  #######################################
  ## Sum the number of adults observed on nests
  #################################################
  
  ########### Sum counts per date for all surveys counted on the same island across segments  #######################################
  # Note that in some years (mainly 2007-2009) there can be more than one primary survey per day for a segment (Little Calf and Calf). 
  # This is because of how the data were translated from past recording at the island scale to how we handle data in the segments in the current database BE. 
  # These are NOT to be considered duplicate surveys but complete the survey for that island so they should be summed together. 
  #######################################
  if (time == "date") {
    
    # count the primary surveys per day
    #df.melt |> group_by(Species_Code, Island, Segment,Date,Survey_Primary, Observer, ObsSkillLevel) |> tally() |> View()
    
    # sumarize the number of nests counted per segment each day
    SumBySegment <- df.melt |> 
      group_by(Species_Code, Island, Segment, Date, month, year, Observer) |> 
      dplyr::summarise(value = sum(value, na.rm = TRUE), surveys=n()) |> # sum per segment when >1 primary surveys per date. 
      dplyr::rename(time = Date) |> 
      dplyr::mutate(stat ="sum")
    
    
    no_clt_years <- c(2009:2010, 2012, 2024:2040) # years Carol wasn't considered the primary and only expert observer
    
    ## Sum the number of adults on each date across all islands from specific observers
    ### Calculate for all Islands
    SumByBOHA <- df.melt |> 
      group_by(Species_Code,  Date, month, year, Observer) |> 
      dplyr::summarise(value = sum(value, na.rm = TRUE), surveys= n()) |> #  sum per segment when >1 primary surveys per date.
      tibble::add_column(Island = "All Islands", Segment="All") |> 
      dplyr::rename(time = Date) |> 
      dplyr::mutate(stat ="sum")
  }
  
  
  ############# Sum counts per year or across all surveys per island across segments
  # Note that there can be more than one primary survey per year
  # there are also >1 (mainly 2) surveys on the same day by same obs at Little Calf- All, so these need to be summed first prior to
  # summarizing per year so that daily mean/max is correctly calculated
  # there are also surveys with 0 obs on the first survey of the year but follow-up surveys have >1; taking the mean across successive dates
  # isn't appropriate because the 0 surveys were "too early", removing these 0 from mean calc
  # If CLT is on a survey, use her count. If there are >1 surveys per year by CLT take the maximum count from those surveys for the annual estimate. 
  # If CLT isn’t on the survey, take the average of the count from the skilled obs (skill level >3) in the boat or take the average of their maximum counts when >1 surveys per year
  
  #######################################
  # count the primary surveys per year
  #df.melt |> group_by(Species_Code, Island, Segment,year,Survey_Primary, Observer) |> tally() |> View()
  
  if (time == "year") {
    yrs_NoCLT<- c(2009:2010, 2012, 2024:2040) # years Carol wasn't considered the primary and only expert observer
    
    # first, extract the Primary PI's surveys to calc max count per year
    CLTByDay <- df.melt |> 
      dplyr::filter(stringr::str_detect(Observer, stringr::regex("CLT", ignore_case = TRUE))) |>  # extract records where Carol Trocki's an observer (2007 to 2023)
      group_by(Species_Code, Island, Segment,year, Date, Observer) |> ## first sum by date to account for multiple counts per segement per day 
      dplyr::summarise(value = sum(value, na.rm = TRUE)) |> # get daily segment totals
      group_by(Species_Code, Island, Segment,year,Observer) |> 
      dplyr::summarise(value = max(value, na.rm = TRUE), survey_days = n()) |>   # get annual max count
      dplyr::mutate(stat ="max")
    
    OthersByDay<- df.melt |> filter(year %in% yrs_NoCLT) |> # extract records from other observers 
      group_by(Species_Code, Island, Segment,year, Date, Observer) |> ## first sum by observer to account for multiple surveys per day 
      dplyr::summarise(value = sum(value, na.rm = TRUE)) |>   # get daily totals by observer for each segment
      group_by(Species_Code, Island, Segment,year,Observer) |> 
      dplyr::summarise(value= max(value, na.rm = TRUE), survey_days=n()) |> # calculate the max count from each observer among dates 
      group_by(Species_Code, Island, Segment,year) |>
      dplyr::summarise(value= round(mean(value, na.rm = TRUE),0), survey_days = round(mean(survey_days),0)) |> # calculate the mean max count among observers per year 
      dplyr::mutate(stat ="max", Observer = "Multiple") 
    
    SumBySegment<-bind_rows(CLTByDay, OthersByDay) |> 
      arrange(Species_Code, Island, year)|> 
      dplyr::rename(time = year)
    
    ## Summarize the number of incubating adults in each year across all islands
    ### Calculate for all Islands
    SumByBOHA <- SumBySegment |> # use annual segment totals from specific obs from above
      group_by(Species_Code, time) |> # now summarize data by year to calc daily mean/ max and overall sum
      dplyr::summarise(value = sum(value, na.rm = TRUE)) |> 
      tibble::add_column(Island = "All Islands", Segment="All", stat = "max")  
  }
  
  #################################
  # bind together the results aggregated by time AT the SEGMENT-SCALE
  ## CALCULATE  VALUES PER ISLAND or SEGMENT BASED ON SURVEY-ADJUSTED ESTIMATES 
  ## for islands with >1 segments, sums raw counts and survey area by island and then divides sum counts / sum survey area
  ## for many surveys there is only one segment surveys, but there are a few with >1 segement per island surevyed in a given year
  ## the final df calculates the sum of all counts per island(segment), which are inflated estimates because they include repeat surveys,
  # the sum of the mean, max, and minimum of mutiple counts per island(segment) and year (if time = year). 
  # When time= year, mean values are the best to report because they account for seasonal variation.
  #################################
 
  # Define grouping columns based on segment
  group_cols <- if (segment) {
    c("Species_Code", "Island", "Segment", "time", "Size_Units", "stat")
  } else {
    c("Species_Code", "Island", "time", "Size_Units", "stat")
  }
  
  # Define output columns based on segment
  select_cols <- if (segment) {
    c("Species_Code", "Island", "Segment", "time", "variable", "stat",
      "value", "valuePerSurveySize", "Survey_Size", "Survey_Units")
  } else {
    c("Species_Code", "Island", "time", "variable", "stat",
      "value", "valuePerSurveySize", "Survey_Size", "Survey_Units")
  }
  
  AllData <- dplyr::bind_rows(SumBySegment, SumByBOHA) |>
    tibble::add_column(Survey_Type = "Incubation") |> # add in for correct binding of survey effort
    dplyr::left_join(
      survey_effort, 
      by = c("Species_Code", "Island", "Segment", "Survey_Type") # append survey effort per segment
    ) |>
    dplyr::group_by(!!!rlang::syms(group_cols)) |> # choose correct grouping variables (segment or all BOHA)
    dplyr::summarise(
      value = sum(value, na.rm = TRUE), # sum counts per grouping variable
      Survey_Size = sum(Survey_Size, na.rm = TRUE), # sum survey effort per island 
      .groups = "drop"
    ) |>
    dplyr::mutate(
      valuePerSurveySize = dplyr::if_else( # standardize counts by survey effort
        Survey_Size > 0,
        round(value / Survey_Size * 1e6, 3),
        NA_real_
      ),
      Survey_Size = Survey_Size / 1e6,
      Survey_Units = "km2",
      variable = "Incubating Adults"
    ) |>
    dplyr::select(dplyr::all_of(select_cols))
  
  AllData <- dplyr::inner_join(
    tlu_Species,
    AllData,
    by = "Species_Code"
  )
  
  graph.final <- AllData |>
    mutate(FullLatinName = as.character(FullLatinName),
           CommonName = as.character(CommonName)) 
  
  if(time  == "year"){
    graph.final<-graph.final |> 
      mutate(year= time)}else{
        
        graph.final<-graph.final |> 
          mutate(year= year(time))
      }
  
  # output data for graphing
  
  if(output == "graph") {
    return(graph.final)
    #write.table(graph.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
  }
  
  ### make wide for tabular display
  if(output == "table") {
    table.final <- spread(graph.final, CommonName, value, drop = TRUE) |> 
      ungroup() |>  
      mutate(Species_Code = NULL, FullLatinName = NULL)
    #write.table(table.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
    return(table.final)
  }
}