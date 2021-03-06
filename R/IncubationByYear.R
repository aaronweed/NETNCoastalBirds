#'  Boat-based incubation survey data summarized by island and year
#' 
#' @source Trocki, C. L., B. R. Mitchell, and P. W. C. Paton. 2015. Coastal breeding bird monitoring protocol 
#' for Boston Harbor Islands National Recreation Area: 2015 revision. Natural Resource Report NPS/NETN/NRR—2015/954. 
#' National Park Service, Fort Collins, Colorado. \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @format A data frame with columns:
#' \describe{
#'  \item{Species_Code}{Unique, 4-letter code for species.}
#'  \item{CommonName}{Common name this species}
#'  \item{FullLatinName}{Accepted Latin name (genus and species)}
#'  \item{Island}{Island associated with each survey event.}
#'  \item{time}{Date as year}
#'  \item{year}{year; numeric;needed for indexing in \code{PlotBirds}}
#'  \item{variable}{Incubating adults on nests}
#'  \item{value}{raw count}
#'  \item{valuePerSurveySize}{raw count divided by Survey_Size}
#'  \item{Survey_Size}{Survey_Size}{numeric value of area or distance surveyed}
#'  \item{Survey_Units}{Units of distance (km) or area (km2) surveyed)}
#'  
#' }
#' @examples
#' \dontrun{
#'  IncubationByYear
#' }
"IncubationByYear"
