#' @include GetIncubationData.R GetSurveyData.R

#' @title Sum Incubation Surveys
#'
#' @import dplyr mutate
#' @importFrom magrittr %>% 
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
#' @param survey_effort Requires seg_event dataframe created from NETN's data package imported via \code{\link{importCBBData}}. E.g. ccb$seg_event<- importCBBData()
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
#' importCBBData(new_env= TRUE) #creates CBB_TABLES object
#' SumIncubation(df= CBB_TABLES$qry_Dataset_2_Survey_Incubation, time= "year",  survey_effort = cbb$seg_event, species = "DCCO", output = "graph")
#' SumIncubation(df= CBB_TABLES$qry_Dataset_2_Survey_Incubation, time= "date",  survey_effort = cbb$seg_event, species = "DCCO", output = "graph")
#' SumIncubation(df= CBB_TABLES$qry_Dataset_2_Survey_Incubation, time= "date",  survey_effort = cbb$seg_event, ByObserver = "yes")
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
    df <- GetIncubationData()
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
  
  df <- df |>
    dplyr::filter(
      !Species_Code %in% c("LETE", "COTE"),
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
  # 3. Sum multiple observations of incubating adults on nest on the same
  #    island/segment/date for each observer
  #------------------------------------------------------------
  
  ########### Sum counts per date for all surveys counted on the same island across segments  #######################################
  # Note that in some years (mainly 2007-2009) there can be more than one primary survey per day for a segment (Little Calf and Calf). 
  # This is because of how the data were translated from past recording at the island scale to how we handle data in the segments in the current database BE. 
  # These are NOT to be considered duplicate surveys so they should be summed together as a complete  survey for that island .
  #######################################

  
  daily <- df |>
    dplyr::group_by(
      Species_Code,
      Island,
      Segment,
      Survey_Primary,
      Date,
      year,
      month,
      Observer
    ) |>
    dplyr::summarise(
      value = sum(Unit_Count, na.rm = TRUE),
      .groups = "drop"
    )
  
  #------------------------------------------------------------
  # 4. Apply requested temporal summary
  #------------------------------------------------------------
  
  if (time == "date") {
    
    by_segment <- daily |>
      dplyr::group_by(
        Species_Code,
        Island,
        Segment,
        Date,
        month,
        year,
        Observer
      ) |>
      dplyr::summarise(
        value = sum(value, na.rm = TRUE),
        surveys = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::rename(time = Date) |>
      dplyr::mutate(stat = "sum")
    
  } else { # time = "year"
    
############# Sum counts per year or across all surveys per island and across segments ############# 
# 1. Note that there can be more than one primary survey per year for each segment
# 2. there are also >1 (mainly 2) surveys on the same day by same obs at Little Calf- All, so these need to be summed first prior to summarizing per year so that daily mean/max is correctly calculated
# 3. there are also surveys with 0 obs on the first survey of the year but follow-up surveys have >1 events per segment; taking the mean across successive dates
# isn't appropriate because the 0 surveys were "too early", removing these 0 from mean calc
# 4. If CLT is on a survey (2007 to 2023), use her count since there were no other observers with comparable skill.  
# 5. From 2007 to 2023: if CLT isn’t on the survey, take the average of the count from the skilled obs (skill level >3) in the boat or take the average of their maximum counts when >1 surveys per year
# 6. After 2024 most of the surveys should be duplicated, take the average of the count from the Primary Surveys
#######################################
# scripts first pull out observations by CLT and calculates daily mean counts per island per year and the max count per year among dates
    # script then pulls out observations from other observers and calculates (1) daily mean counts per island per year per observer and the annual max count among dates per observer
    
    no_clt_years <- c(2009:2010, 2012, 2024:2040) # years Carol wasn't considered the primary and only expert observer
    
    clt <- daily |>
      dplyr::filter(Observer == "CLT") |> # filter CLT surveys only
      dplyr::group_by(
        Species_Code,
        Island,
        Segment,
        year,
        Date
      ) |>
      dplyr::summarise(
        value = sum(value, na.rm = TRUE), # daily nest count
        .groups = "drop"
      ) |>
      dplyr::group_by(
        Species_Code,
        Island,
        Segment,
        year
      ) |>
      dplyr::summarise(
        mean_value = mean(value, na.rm = TRUE), # average daily nest count
        max_value = max(value, na.rm = TRUE), # max of the daily nest counts
        sd_value = sd(value, na.rm = TRUE), # sd of the daily nest counts
        daily_surveys = dplyr::n(), # the number of daily surveys
        n_observers = 1,
        daily_surveys_obs = daily_surveys/n_observers, # calc the no. of daily surveys per observer
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = c(mean_value, max_value,sd_value),
        names_to = "stat",
        values_to = "value"
      ) |>
      dplyr::mutate(
        stat = dplyr::recode(
          stat,
          mean_value = "mean",
          max_value = "max",
          sd_value = "sd",
        ),
        Observer = "CLT"
      )
    
    # calculate annual stats from the other observers
    
    others <- daily |>
      #dplyr::filter(year %in% no_clt_years) |> # filter out surveys without CLT
      
      # 1. Calculate each observer's daily total
      dplyr::group_by(
        Species_Code,
        Island,
        Segment,
        year,
        Date,
        Observer # daily total by observer
      ) |>
      dplyr::summarise(
        value = sum(value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      
      # 2. Calculate each observer's maximum daily count for the year
      dplyr::group_by(
        Species_Code,
        Island,
        Segment,
        year,
        Observer
      ) |>
      dplyr::summarise(
        max_value = max(value, na.rm = TRUE), # max count per observer per year
        mean_value = mean(value, na.rm = TRUE), # daily mean count per observer per year
        sd_daily_value = sd(value, na.rm = TRUE), # sd of the daily nest counts per observer per year
        surveys = dplyr::n(),
        .groups = "drop"
      ) |>
      
      # 3. Average daily observer-specific estimates per year
      dplyr::group_by(
        Species_Code,
        Island,
        Segment,
        year
      ) |>
      dplyr::summarise(
        mean_max = round(mean(max_value, na.rm = TRUE), 1), # mean of the max daily counts among observers per year
        mean_daily = round(mean(mean_value, na.rm = TRUE), 1), # mean daily count among observers per year
        sd_obs = round(sd(mean_value, na.rm = TRUE),1), # sd of the mean daily nest counts among observers per year
        daily_surveys = sum(surveys), # sum the number of daily surveys
        n_observers = dplyr::n(),
        daily_surveys_obs = daily_surveys/n_observers, # calc the no. of daily surveys per observer
        .groups = "drop"
      ) |>
      
      # 4. Put both statistics into the same format
      tidyr::pivot_longer(
        cols = c(mean_max, mean_daily, sd_obs),
        names_to = "stat",
        values_to = "value"
      ) |>
      dplyr::mutate(
        stat = dplyr::recode(
          stat,
          mean_max = "max", # this is technically the mean of the daily max counts 
          mean_daily = "mean",
          sd_obs = "sd_obs"
        ),
        Observer = "Multiple"
      )
  
    by_segment <- dplyr::bind_rows(clt, others) |>
      dplyr::rename(time = year)
    
  }
  
  #------------------------------------------------------------
  # 5. Create all-island totals
  #------------------------------------------------------------
  # add up the per segment mean daily and max daily counts per day for all island segments
  
  all_islands <- by_segment |>
    dplyr::group_by(
      Species_Code,
      time,
      stat
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      #daily_surveys = sum(daily_surveys/n(), na.rm = TRUE), # sum the no. daily surveys per segment per year divided by the no. of sgements
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Island = "All Islands",
      Segment = "All"
    )
  
  #------------------------------------------------------------
  # 6. Combine and join survey effort
  #------------------------------------------------------------
  
  all_data <- dplyr::bind_rows(
    by_segment,
    all_islands
  ) |>
    dplyr::mutate(
      Survey_Type = "Incubation"
    ) |>
    dplyr::left_join(
      survey_effort,
      by = c(
        "Species_Code",
        "Island",
        "Segment",
        "Survey_Type"
      )
    )
  
  #------------------------------------------------------------
  # 7. Island vs segment summary
  #------------------------------------------------------------
  
  if (segment) {
    
    all_data <- all_data |>
      dplyr::group_by(
        Species_Code,
        Island,
        Segment,
        time,
        stat,
        Size_Units
      )
    
  } else {
    
    all_data <- all_data |>
      dplyr::group_by(
        Species_Code,
        Island,
        time,
        stat,
        Size_Units
      )
    
  }
  
  all_data <- all_data |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      Survey_Size = sum(Survey_Size, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      valuePerSurveySize = dplyr::if_else(
        Survey_Size > 0,
        round(value / Survey_Size * 1e6, 3),
        NA_real_
      ),
      Survey_Size = Survey_Size / 1e6,
      Survey_Units = "km2",
      variable = "Incubating Adults"
    )
  
  #------------------------------------------------------------
  # 8. Add species names and rename time variable
  #------------------------------------------------------------
  
  graph.final <- all_data |>
    dplyr::left_join(
      tlu_Species,
      by = "Species_Code"
    ) 
  
  if(time  == "year"){
    graph.final<-graph.final %>% 
      dplyr::mutate(year = time)}else{
        
        graph.final<-graph.final %>% 
          mutate(year= lubridate::year(time))
      }
  
  #------------------------------------------------------------
  # 9. Return requested format
  #------------------------------------------------------------
  
  if (output == "graph") {
    return(graph.final)
  }
  
  ### make wide for tabular display
  if(output == "table") {
  graph.final |>
    tidyr::pivot_wider(
      names_from = CommonName,
      values_from = value
    ) |>
    dplyr::select(
      -dplyr::any_of(c("Species_Code", "FullLatinName"))
    )
    return(table.final)
  }
}