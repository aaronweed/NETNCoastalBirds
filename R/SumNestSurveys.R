#' @include GetNestData.R

#' @title Sum nest surveys
#'
#' @import dplyr 
#' @importFrom RODBC odbcConnect sqlFetch odbcClose 
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% %<>%
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' @importFrom plyr mapvalues
#' @importFrom forcats fct_collapse
#' 
#' @description Brings in the raw ground-based nest survey data from \code{\link{GetNestData}} 
#' and summarizes it by date for plotting and analysis.
#' @param time Character string equal to "date" or "year". Value must be provided; there is
#' no default. Choose to sum counts by "date" or "year". Summing by date will sum counts 
#' across segments of each island for each date. Summing by year sums counts across all surveys 
#' conducted in that year. Note that some surveys were repeated in the same year. 
#' @param species To subset data by species, use "BCNH" ,"COEI", "GLIB", "GREG", 
#' "SNEG", "DCCO", "GBBG", "HERG", "COTE" or "LETE". Defaults
#' to providing output for all species.
#' @param output Character string equal to "graph" or "table". 
#' Defaults to long format (output= "graph") ready for ggplot and the \code{\link{PlotBirds}}
#' function. For wide format use "table".
#' @param df  The user can optionally load the raw nest data from an R object or connect to the 
#' Access database to obtain it. Defaults to NULL, which means the Access database will
#' be used to obtain it.
#'
#' @return Returns a \code{list}  with the counts of nests and their contents (chicks or eggs). 
#' The first  \code{list}  element summarizes nest surveys by Date for graphing. The second 
#' element for tabular summary.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm} 
#' @examples 
#' SumNestSurveys(time= "date", species = "BCNH")
#' SumNestSurveys(time= "year", species = "BCNH")
#' SumNestSurveys(time= "year", output = "table")
#' @export


SumNestSurveys <- function(time, species=  NA, output= "graph", df = NULL) {
  # counts the num. of nests, chicks or eggs (type) per factor level (species, island, segment, date)
  # type inputs are "Chicks", "Nests", "Eggs"
  
  
  if (is.null(df))
    df <- GetNestData(x) # bring in raw data
  
  df <- droplevels(df)
  
  # subset by species if provided argument
  if(!anyNA(species)) df<-df[df$Species_Code %in% species, ] 
  
  
  # Setup and create molten dataframe
  ################################################################################
  
  #### Calculate the actual count totals from the raw data. 
  ## **In order to get correct counts, need to multiply the nest contents by the num of nests.**
  ## For example, Unit_Count= 20, Eggs_count = 2, Chicks_Count= 0 means 
  ## that there were 20 nests surveyed EACH with 2 eggs and 0 chicks ...
  ## NOT 20 nests with a total of 2 eggs.
  ## Chick and egg counts ONLY calculated when Nest Status == Normal
  
  df %<>% 
    dplyr::rename(Nests = Unit_Count) %>% # change Unit_Count to Nests to be more specific
    mutate(Chicks = Nests * Chick_Count,
           Eggs = Nests * Egg_Count)
  # calculate no. of life stages based on per nest counts
  
  # create molten df
  df.melt <- df %>%
    dplyr::select(Island, Date, Survey_Primary, Survey_Duplicate, 
                    Species_Code, Nest_Status, Nests, Chicks, Eggs) %>%
    dplyr::filter(Survey_Primary == "Yes" ) %>%  # grab only records from the primary survey to avoid counting multi-obs of same event
    dplyr::filter(Survey_Duplicate == "No" ) %>%  # grab only records from the first survey if repeated
    mutate(year = year(Date), 
           month = month(Date) ) %>% 
    gather(variable, value, -Island,-Date,-Survey_Primary, -Survey_Duplicate, 
           -year, -month, -Species_Code, -Nest_Status) 
  
  # filter out nest contents for "Normal" nests only; denote which nests were directly counted vs estimated
  
  eggs <- df.melt %>% 
    filter(variable %in% "Eggs" & Nest_Status %in% "Normal") %>% 
    add_column(Count_Method = "Direct Count")
  
  chicks <- df.melt %>% 
    filter(variable %in% "Chicks" & Nest_Status %in% "Normal") %>% 
    add_column(Count_Method = "Direct Count")
  
  nests <- df.melt %>% 
    filter(variable %in% "Nests") %>%  # collapse nests status to denote nests counted directly vs estimated (e.g., by flushing adults)
    mutate(Count_Method = 
             fct_collapse(Nest_Status, 
                          "Direct Count" = c("Abandoned" , "Depredated", "Fledged", "Normal", 
                                             "Other", "Unknown" ), 
                          Estimated = "Estimate"))
  
  # bind together
  
  temp <- bind_rows(eggs,chicks, nests) %>% 
    na.omit() # a few NAs in the chicks table for some reason
  
  #################################
  ## Sum the number of nests, eggs and chicks per YEAR  at each island
  # in dplyr
  ######################################
 
  if (time == "year") {
   TernMax <- temp %>% 
     filter(Species_Code %in% "COTE" | Species_Code %in% "LETE") %>% 
     group_by(Island, Species_Code, Count_Method, variable,year) %>% 
     dplyr::summarise(value = max(value, na.rm=TRUE))    # calc annual  max for terns
     
     SumBySelect <- temp %>% 
       filter(!Species_Code == "COTE" & !Species_Code == "LETE") %>%  # calc sum for others
       group_by(Island, Species_Code, year, Count_Method, variable) %>% 
       dplyr::summarise(value = sum(value, na.rm=TRUE)) %>%
      dplyr::bind_rows(.,TernMax) %>% 
     dplyr::rename(time = year)
  
  ## Sum the number of nests, eggs and chicks for each date across all islands
  ### Calculate for all Islands
     TernMax <- temp %>% 
       filter(Species_Code %in% "COTE" | Species_Code %in% "LETE") %>% 
       group_by(Species_Code, Count_Method, variable,year) %>% 
       dplyr::summarise(value = max(value, na.rm=TRUE))    # calc annual  max for terns
     
     SumByBOHA <- temp %>% 
       filter(!Species_Code == "COTE" & !Species_Code == "LETE") %>%  # calc sum for others
       group_by(Species_Code, year, Count_Method, variable) %>% 
       dplyr::summarise(value = sum(value, na.rm=TRUE)) %>%
       dplyr::bind_rows(.,TernMax) %>% 
       dplyr::rename(time = year) %>% 
       add_column(Island = "All Islands")
  }
  
  #######################################
  ## Sum the number of nests, eggs and chicks for each DATE at each island
  #################################################
  if (time == "date") {
  SumBySelect <- temp %>% 
    group_by(Island, Species_Code, Date, year, month, Count_Method, variable) %>% 
    dplyr::summarise(value = sum(value, na.rm=TRUE)) %>% 
    dplyr::rename(time = Date)
  
  ## Sum the number of nests, eggs and chicks for each date across all islands
  ### Calculate for all Islands
  SumByBOHA <- temp %>% 
    group_by(Species_Code, Date, year, month, Count_Method, variable) %>% 
    dplyr::summarise(value = sum(value, na.rm=TRUE)) %>% 
    add_column(Island = "All Islands")  %>% 
    dplyr::rename(time = Date)
  }
  
  # bind together
  SumBySelection <- bind_rows(SumBySelect, SumByBOHA) %>% 
    inner_join(species_tlu, ., by= "Species_Code") %>%  # add species names to data
    filter(!Species_Code %in% "AMOY")  # remove AMOY coming in for some reason
  
  graph.final <- SumBySelection %>%
    mutate(FullLatinName = as.character(FullLatinName),
           CommonName = as.character(CommonName))
      
  # output data for graphing
  
  if (output == "graph") {
    return(graph.final)
    #write.table(graph.final, "./Data/GroundNestsurveys.csv", sep=",", row.names= FALSE)
  }
  
  
  ### make wide for tabular display
  if (output == "table") {
    table.final <- spread(graph.final, variable, value, drop = TRUE, fill = 0)
    return(table.final)
    #write.table(table.final, "./Data/GroundNestsurveys.csv", sep=",", row.names= FALSE)
  }
  
  # output tabular  data
  #write.table(table.final, "./Data/GroundNestsurveysTable_ByDate.csv", sep=",", row.names= FALSE)
  #write.table(table.final, "~/R/NETN/Coastal Birds/CoastalBirdViz/Data/GroundNestsurveys_ByDate.csv", sep=",", row.names= FALSE)
  
}

