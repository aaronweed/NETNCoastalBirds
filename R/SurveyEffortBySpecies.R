#' Data table of survey effort by species
#' 
#' @source Trocki, C. L., B. R. Mitchell, and P. W. C. Paton. 2015. Coastal breeding bird monitoring protocol 
#' for Boston Harbor Islands National Recreation Area: 2015 revision. Natural Resource Report NPS/NETN/NRR—2015/954. 
#' National Park Service, Fort Collins, Colorado. \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @format A data frame with columns:
#' \describe{
#'  \item{Species_Code}{4 letter species code of observation}
#'  \item{Island}{Island associated with each survey event.}
#'  \item{Segment}{Island segment associated with survey event (ex. All, North, Northeast, Northwest, South, Southeast, East, West).}
#'  \item{Survey_Class}{Postal code abbreviation for territory or province.}
#'  \item{Survey_Type}{Type of survey (e.g. Incubation, Creche, Nest)}
#'  \item{Survey_Size}{Length (if line) or area (if polygon) of survey route}
#'  \item{Size_Units}{Size of segment; km for linear features; km2 for area}
#' }
#' @examples
#' \dontrun{
#'  SurveyEffortBySpecies
#' }
"SurveyEffortBySpecies"