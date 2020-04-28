#'  Boat-based Common Eider creche data summarized by Observer on each survey date
#' 
#' @source Trocki, C. L., B. R. Mitchell, and P. W. C. Paton. 2015. Coastal breeding bird monitoring protocol 
#' for Boston Harbor Islands National Recreation Area: 2015 revision. Natural Resource Report NPS/NETN/NRR—2015/954. 
#' National Park Service, Fort Collins, Colorado. \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @format A data frame with columns:
#' \describe{
#'  \item{Species_Code}{4 letter species code of observation}
#'  \item{Island}{Island associated with each survey event.}
#'  \item{Segment}{Island segment associated with survey event (ex. All, North, Northeast, Northwest, South, Southeast, East, West).}
#'  \item{time}{Date in year-month-day format}
#'  \item{month}{month; numeric}
#'  \item{year}{year; numeric}
#'  \item{Survey_Primary}{If "Yes, denotes data was collected by lead biologist.}
#'  \item{Survey_Type}{Denotes whether data was collected as part of a "Boat" or "Ground" Survey.}
#'  \item{Survey_Duplicate}{“Yes” if data represents double-counting the same birds, using the same methods, in the same season.}
#'  \item{Survey_Complete}{Dentes whether the entire island segment was surveyed; "Yes" or "No"}
#'  \item{Species_Unit}{COEI Life stage: F-Lone (females alone), Chick (ducklings),  F-Tend (females tending ducklings)}
#'  \item{value}{raw count}
#'  \item{valuePerSurveySize}{raw count divided by Survey_Size}
#'  \item{Survey_Size}{Area or distance surveyed; numeric}
#'  \item{Survey_Units}{Units of distance (km) or area (km2) surveyed)}
#'  \item{Observer}{Initial(s) of Observer(s)}
#'  
#' }
#' @examples
#' \dontrun{
#'  CrecheByObserver
#' }
"CrecheByObserver"
