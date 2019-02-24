#' @include SumNestSurveys.R
#' @include SumIncubation.R

#' @title sum Gull and DCCO surveys
#'
#' @import dplyr
#' @importFrom magrittr %>% 
#' @importFrom tibble add_column
#' 
#' @description Brings in the raw incubation and Nest survey data from \code{\link{GetIncubationData}} 
#' and \code{\link{GetNestData}}, respectively, and summarizes the data for plotting and analysis. 
#' Currently only sums counts from the primary survey when repeated surveys were conducted. 
#' If you specify an argument to "ByObserver" this will return sum counts of all duplicate surveys by observer.

#' @param time Character string equal to "date" or "year". Value must be provided; there is
#' no default. Choose to sum counts by "date" or "year". Summing by date will sum counts across 
#' segments of each island for each date. Summing by year sums counts across all surveys conducted 
#' in that year. Note that some surveys were repeated in the same year. 
#' @param species To subset data by species, use "DCCO","GBBG","HERG". Defaults
#' to providing output for all species.
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
#'@param islands Defaults to summarizing counts only within the Outer Islands (Calf, Little Calf, Green,
#'   The Graves, Middle Brewster, Outer Brewster, Shag Rocks and Little Brewster).
#' @return Returns a \code{list} with the counts of Gulls, cormorant, and terns observed during 
#' boat-based surveys per island, species, and date/year. The first  \code{list}  element summarizes 
#' incubation surveys by Year for graphing. The second element for tabular summary.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm} 
#' @examples  
#' SumGulls_DCCO(time= "year", species = "DCCO", output = "graph")
#' SumGulls_DCCO(time= "date", species = "DCCO", output = "graph")
#' SumGulls_DCCO(time= "date", ByObserver = "yes")
#' 
#' DCCO<-SumGulls_DCCO(time= "year", species= "DCCO")
#' PlotBirds(DCCO, var= "Incubating adults")
#' PlotBirds(DCCO, var= "Nests")
#' @export
#' 
#
SumGulls_DCCO <- function(time = "year", species = c("DCCO","GBBG","HERG"), output = "graph", ByObserver = "no", islands =c("Calf", "Little Calf", "Green", "The Graves", "Middle Brewster", "Outer Brewster", "Shag Rocks","Little Brewster")) {
  
###### Sum  Data ----
  
  df <- 
    SumIncubation(time= time, species = species, ...) %>% 
    add_column(Count_Method = "Direct Count") %>% 
    mutate(time= as.numeric(as.character(time)))%>% 
    {if(!anyNA(islands))filter(.,Island %in% islands)} #### Subset df by Outer Islands per species 
    
  df<- df %>%
    bind_rows(SumNestSurveys(time= time,species = species,...)) 
          
        }
          
  
  