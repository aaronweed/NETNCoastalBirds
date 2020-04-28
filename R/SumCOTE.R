#' @include GetIncubationData.R

#' @title sum COTE incubation surveys
#'
#' @import dplyr 
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' @importFrom lubridate year month

#' @description Brings in the raw incubation survey data from \code{\link{GetIncubationData}} and
#'  summarizes the data for plotting and analysis for Common Tern. Currently only returns counts from Spinnaker Platform.
#'
#' @param time Character string equal to "date" or "year". Value must be provided; there is
#' no default. Choose to return raw counts by "date" or max counts per "year". 
#' @param output Character string equal to "graph" or "table". 
#' Defaults to long format (output= "graph") ready for ggplot and the \code{\link{PlotBirds}}
#' function. For wide format use "table".
#' @param ByObserver Character string equal to "yes" or "no".  If "yes" will output the 
#' survey data counted by each observer for each island 
#' segment on each date. Only sums across multiple observations by same observer at each segment. 
#' Defaults to "no".
#' @param df  The user can optionally load the raw incubation data from an R object or connect to the 
#' Access database to obtain it. Defaults to NULL, which means the Access database will
#' be used to obtain it.
#' 
#' @return Returns a \code{data.frame} with the counts of Common terns (COTE) observed during 
#' boat-based surveys 
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm} 
#' @examples  
#' SumCOTE(time= "year", output = "graph")
#' SumCOTE(time= "date", output = "table")
#' @export
#' 

SumCOTE <- function(df = NULL, time, output = "graph", ByObserver = "no") {
  # this function summarizes the number of adults on nests per island, year, and by observer
 
  
  if(!requireNamespace("Hmisc", quietly = TRUE)){
    stop("Package 'Hmisc' is needed for this function to work. Please install it.", call. = FALSE)
  } 
  
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if (is.null(df)) {
    df <- GetIncubationData(x) # import data from the database if needed
    #head(df)
  }
  
  # Subset to only counts on Spinnaker
  
  COTE<- df %>% 
    filter(Species_Code == "COTE" & Island =="Spinnaker") %>% 
    droplevels(.)
  
  # Setup and create molten dataframe
  #############################################################################
  
  
  ### Sum data across each segement as raw numbers by observer 
  
  if (time == "date" & ByObserver == "yes") {
    graph.final <- COTE %>%
      group_by(Island, Segment, c_Observer, Species_Code, 
               Date, month, year, Survey_Duplicate, Survey_Complete) %>% 
      dplyr::summarise(value = sum(Unit_Count, na.rm=TRUE)) %>% 
      dplyr::rename(time = year) %>% 
      inner_join(species_tlu, ., by= "Species_Code") # add species names to data
    return(graph.final)
  }else{
    
    df.melt <- COTE %>%
      dplyr::select(Island, Segment, Date, year, month, Survey_Primary,
                    Survey_Duplicate, Survey_Complete, Species_Code, Unit_Count) %>%
      dplyr::filter(Survey_Primary == "Yes" ) %>% # grab only the records from the primary survey to avoid counting multi-obs of same event
      dplyr::filter(Survey_Duplicate == "No" ) %>% # grab only the records from the first survey if repeated
      tidyr::gather(variable, value, -Island, -Segment, -Date, -year, -month, -Survey_Primary, 
                    -Survey_Duplicate, -Survey_Complete, -Species_Code) %>% 
      dplyr::mutate(variable = NULL)
    # head(df.melt)
    
  }
  
  #######################################
  ## Sum the number of adults observed on nests
  #################################################
  
  ### Get Max counts per year
  
  if (time == "year") {
    SumBySelect <- df.melt %>% 
      group_by(Island, Species_Code, year) %>%
      dplyr::summarise(value = max(value, na.rm = TRUE)) %>% 
      dplyr::rename(time = year)
    
  }
  
  ### Get Max counts per date for all surveys counted on the same island across segements
  
  if (time == "date") {
    SumBySelect <- df.melt %>% 
      group_by(Island, Species_Code, Date) %>%
      dplyr::summarise(value = max(value, na.rm = TRUE)) %>% 
      dplyr::rename(time = Date)
    
  }
  
  # bind species names
  SumBySelection <-  
    inner_join(species_tlu, SumBySelect, by = "Species_Code") # add species names to data
  
  graph.final <- SumBySelection %>%
    mutate(FullLatinName = as.character(FullLatinName),
           CommonName = as.character(CommonName),
           variable = "Incubating adults")
  
  # output data for graphing
  
  if(output == "graph") {
    return(graph.final)
    #write.table(graph.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
  }
  
  ### make wide for tabular display
  if(output == "table") {
    table.final <- spread(graph.final, CommonName, value, drop = TRUE) %>% 
      ungroup() %>%  
      mutate(Species_Code = NULL, FullLatinName = NULL)
    #write.table(table.final, "./Data/Incubationsurveys.csv", sep=",", row.names= FALSE)
    return(table.final)
  }
}