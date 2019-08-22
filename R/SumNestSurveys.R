#' @include GetNestData.R GetSurveyData.R

#' @title Sum nest surveys
#'
#' @import dplyr
#' @importFrom tidyr spread gather
#' @importFrom magrittr %>% %<>%
#' @importFrom tibble add_column
#' @importFrom lubridate year month
#' @importFrom plyr mapvalues
#' @importFrom forcats fct_collapse
#' 
#' @description Brings in the raw ground-based nest survey data from \code{\link{GetNestData}} 
#' and summarizes it by date or year for plotting and analysis.
#' @param time Character string equal to "date" or "year". Defaults to "date".
#' Choose to sum effort-adjusted counts (no. / km2) by "date" or "year". Summing by date will sum counts 
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
#' @return Returns a \code{data.frame} with the counts of nests and chicks or eggs adjusted for survey-area (e.g. nests per km2) per Island and time
#' and estimates the Eggs and Chicks per Nest per island and time. Life stages are denoted under 'variable' eg., variable == "Nests","Chicks","Eggs", ChicksPerNest", "EggsPerNest".
#'  
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm} 
#' @examples 
#' SumNestSurveys(time= "date", species = "BCNH")
#' SumNestSurveys(time= "year", species = "BCNH")
#' SumNestSurveys(time= "year", output = "table")
#' @export


SumNestSurveys <- function(time ="date", species=  NA, output= "graph", df = NULL) {
  # counts the num. of nests, chicks or eggs (type) per factor level (species, island, segment, date)
  # type inputs are "Chicks", "Nests", "Eggs"
  
  
  if (is.null(df))
    df <- GetNestData(x) %>%  # bring in raw data
          droplevels()
  

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
    dplyr::select(Island, Segment,Date, Survey_Primary, Survey_Duplicate, 
                  Species_Code, Nest_Status, Nests, Chicks, Eggs, Observer) %>%
    dplyr::filter(Survey_Primary == "Yes" ) %>%  # grab only records from the primary survey to avoid counting multi-obs of same event
    dplyr::filter(Survey_Duplicate == "No" ) %>%  # grab only records from the first survey if repeated
    mutate(year = year(Date), 
           month = month(Date) ) %>% 
    gather(variable, value, -Island,-Segment,-Date,-month, -year, -Survey_Primary, -Survey_Duplicate, 
          -Species_Code, -Nest_Status,-Observer) 
  
  # Set up variable naming to denote counting method per life stage and  
  # filter out nest contents for "Normal" nests only; denote which nests were directly counted vs estimated
  
  eggs <- df.melt %>% 
    filter(variable %in% "Eggs" & Nest_Status %in% "Normal") %>% droplevels() %>% 
    add_column(Count_Method = "Direct Count")
  
  
  chicks <- df.melt %>% 
    filter(variable %in% "Chicks" & Nest_Status %in% "Normal") %>% droplevels() %>% 
    add_column(Count_Method = "Direct Count")
  
  nests <- df.melt %>% 
    filter(variable %in% "Nests") %>%  droplevels() %>% # collapse nests status to denote nests counted directly vs estimated (e.g., by flushing adults)
    mutate(Count_Method = 
             fct_collapse(Nest_Status, 
                          "Direct Count" = c("Abandoned" , "Depredated", "Fledged", "Normal", 
                                             "Other", "Unknown" ), 
                          Estimated = "Estimate"))
  
  # bind togetherand exclude nests that were estimated vs directly counted
  
  temp <- bind_rows(eggs,chicks, nests) %>% 
    filter(Count_Method == "Direct Count") %>% 
    na.omit() # a few NAs in the chicks table for some reason
  
  #######################################
  ## Sum the number of nests, eggs and chicks for each DATE at each island
  #################################################
  if (time == "date") {
    SumBySegment <- temp %>% 
      group_by(Island, Segment, Species_Code, Date, month, Count_Method, variable) %>% 
      dplyr::summarise(value = sum(value, na.rm=TRUE)) %>% 
      dplyr::rename(time = Date)
    
    ## Sum the number of nests, eggs and chicks for each date across all islands
    ### Calculate for all Islands (note, not all segments may have been surveyed in a year)
    SumByBOHA <- temp %>% 
      group_by(Species_Code, Date,  month, Count_Method, variable) %>% 
      dplyr::summarise(value = sum(value, na.rm=TRUE)) %>% 
      add_column(Island = "All Islands", Segment = "All")  %>% 
      dplyr::rename(time = Date)
  }
  
  #################################
  ## Sum the number of nests, eggs and chicks per YEAR  at each island segment
  # in dplyr
  ######################################
  
  if (time == "year") {
    TernMax <- temp %>% # First calculate the max tern counts among surveys
      filter(Species_Code %in% "COTE" | Species_Code %in% "LETE") %>% 
      group_by(Island, Segment, Species_Code, Count_Method, variable,year, month) %>% 
      dplyr::summarise(value = max(value, na.rm=TRUE))    # calc annual  max for terns
    
    SumBySegment <- temp %>%  # calc sum for other species
      filter(!Species_Code == "COTE" & !Species_Code == "LETE") %>% 
      group_by(Island, Segment,Species_Code, year, month, Count_Method, variable) %>% 
      dplyr::summarise(value = sum(value, na.rm=TRUE)) %>% # sum stages
      dplyr::bind_rows(.,TernMax) %>% # bind back in tern max counts
      dplyr::rename(time = year)
    
    ## Sum the number of nests, eggs and chicks for each date across all islands
    ### Calculate for all Islands (note, not all segments may have been surveyed in a year)
    TernMax <- temp %>% 
      filter(Species_Code %in% "COTE" | Species_Code %in% "LETE") %>% 
      group_by(Species_Code, Count_Method, variable,year, month) %>% 
      dplyr::summarise(value = max(value, na.rm=TRUE))    # calc annual  max for terns
    
    SumByBOHA <- temp %>% 
      filter(!Species_Code == "COTE" & !Species_Code == "LETE") %>%  # calc sum for others
      group_by(Species_Code, year, month, Count_Method, variable) %>% 
      dplyr::summarise(value = sum(value, na.rm=TRUE)) %>%
      dplyr::bind_rows(.,TernMax) %>% 
      dplyr::rename(time = year) %>% 
      add_column(Island = "All Islands", Segment = "All")
  }
  
  #################################
  # bind together the results aggregated by time AT the SEGMENT-SCALE, then calc mean eggs and chicks per nest
  #################################
  
  temp2 <- bind_rows(SumBySegment, SumByBOHA) %>%  ### bind together island-segment and all island data
    spread(variable, value, drop=TRUE) %>%  # make wide to divide chicks and eggs by nest count
    mutate(EggsPerNest = round(Eggs/Nests,2), ChicksPerNest = round(Chicks/Nests,2)) %>%  # calc avg chicks or eggs per nest
    gather(variable, value, -Island,-Segment, -Species_Code,-time,-month,-Count_Method) %>% 
    na.omit()  # remove NAs added when no chicks or eggs found and Nests =0
    
  
  ## add survey effort to nest, chick and egg data to estimate # per survey size
  # this provides the option to view the sum total of eggs and chicks per area; the next code chunk sets up per nest estimates
   SumNests <- temp2 %>% 
    dplyr::filter(variable %in% c("Nests","Chicks","Eggs")) %>% 
    dplyr::inner_join(.,GetSurveyData(x, survey="Nest", species = {if(!anyNA(species)) species else NA}),# bind on survey effort #
    by=c("Species_Code","Island","Segment")) %>% ## append survey effort per segment
    dplyr::mutate(valuePerSurveySize = round((value/Survey_Size)*1000000,3)) %>% # standardize counts by survey effort
    dplyr::mutate(Survey_Size = Survey_Size/1000000) %>% # added in case I want to scale to other units
    tibble::add_column(Survey_Units = "km2") # denote what survey effort units are reported
  
  # setup eggs per nest and chicks per nest data and bind to nest data
  
   temp3<- temp2 %>% 
     dplyr::filter(variable %in% c("EggsPerNest","ChicksPerNest")) %>%
     mutate(valuePerSurveySize= value) %>% 
     tibble::add_column(Survey_Class = "Ground",Survey_Type = "Nest",Survey_Size = 1, Survey_Units = "per Nest") %>% 
     bind_rows(SumNests,.)  #add on nest survey data
    
  ## CALCULATE MEAN VALUES PER ISLAND BASED ON SURVEY-ADJUSTED ESTIMATES for final table
   ## for many surveys there is only one segment surveys, but there are a few with >1 segement per island surevyed in a given year
     
    graph.final<- temp3 %>% 
      group_by(Species_Code,Island,time, month, Count_Method, variable, Survey_Size,Survey_Units) %>% 
    dplyr::summarise(value = round(mean(valuePerSurveySize, na.rm= TRUE),2),
                     se= round(sd(valuePerSurveySize, na.rm= TRUE)/sqrt(n()),2)) %>% 
    inner_join(species_tlu, ., by= "Species_Code") %>%   # add species names to data
    mutate(FullLatinName = as.character(FullLatinName),
           CommonName = as.character(CommonName))
    
    # subset by species if provided argument
    if(!anyNA(species)) graph.final<-graph.final %>% 
      filter(Species_Code %in% species) %>% droplevels() 
    
    if(time  == "year"){
      graph.final<-graph.final %>% 
      mutate(year= time)}else{
      
        graph.final<-graph.final %>% 
          mutate(year= year(time))
      }
  
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

