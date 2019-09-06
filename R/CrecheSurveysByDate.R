#'  Boat-based COEI creche survey data summarized by island and day
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
#'  \item{time}{Date as year-month-day}
#'  \item{year}{year; numeric; needed for indexing in \code{PlotBirds}}
#'  \item{variable}{Life stage surveyed; Adult female COEI tending, Average creche size, COEI Ducklings, Total Number of Female COEI Observed}
#'  \item{value}{raw count}
#'  \item{valuePerSurveySize}{raw count divided by Survey_Size}
#'  \item{Survey_Size}{numeric value of area or distance surveyed}
#'  \item{Survey_Units}{Units of distance (km) or area (km2) surveyed)}
#'  \item{Observer}{Initial(s) of Observer(s)}
#'  
#' }
#' @examples
#' \dontrun{
#'  CrecheSurveysByDate
#' }
"CrecheSurveysByDate"
