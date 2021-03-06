% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creche_raw.R
\docType{data}
\name{creche_raw}
\alias{creche_raw}
\title{Raw creche data for COEI boat-based surveys}
\format{
A data frame with columns:
\describe{
 \item{Park}{4 letter code of Boston Harbor Islands NRA}
 \item{Island}{Island associated with each survey event.}
 \item{Segment}{Island segment associated with survey event (ex. All, North, Northeast, Northwest, South, Southeast, East, West).}
 \item{Survey_Class}{Class of survey; Ground}
 \item{Survey_Type}{Type of survey (e.g. Incubation, Creche, Nest)}
 \item{Date}{date of the event; year-month-day}
 \item{Start_Time}{Time the survey event started}
 \item{year}{year of event; numeric}
 \item{month}{month of event; numeric}
 \item{Survey_MultiPart}{Denotes if several surveys provide complete coverage of the defined survey area (Y/N)}
 \item{Survey_Duplicate}{Survey represent duplication of counting effort (e.g. side-by-side surveys). “Yes” if data represents double-counting the same birds, using the same methods, in the same season.}
 \item{Survey_Primary}{"Yes" if survey was conducted by lead biologist}
 \item{Survey_Complete}{Survey covered the area it is meant to (Y/N).  Requires a note if answer is "No", indicating a partial survey was conducted}
 \item{Recorder}{The person recording the survey data collected during this event}
 \item{Observer}{The person(s) making the observations during the survey}
 \item{Species_Code}{4 letter species code of observation}
 \item{Group_Count}{Number of groups (e.g. COEI creche) summed to obtain group observations (Females, chicks, etc.).  '999' = unknown value.}
 \item{Group_Time}{Time first observation of group was made - autopopulated when record is created}
 \item{Group_NewTerritory}{Yes/No/NA field used to document if birds represent a new territory for the season or are a re-observation of a known territory (relevant for AMOY; not relevant for COEI)}
 \item{Group_Notes}{Notes about this group observation}
 \item{Group_Coords}{Geographic coordinates observation of group was made from}
 \item{Species_Unit}{Unit of species observed (e.g. adult, pair, juvenile, etc.); filtered from value list}
 \item{Unit_Count}{Count of units observed}
 \item{Survey_Notes}{Notes/comments for this survey event}
 \item{Wind_Direction}{The direction the wind is blowing from at Start Time (values: N, NE, E, SE, S, SW, W, NW, 0; unknown values coded as NR)}
 \item{Wind_Speed}{Speed of wind (Beaufort scale) at Start Time; unknown values are coded as 999}
 \item{Air_Temp_F}{Temperature of the air in Farenheit at Start Time; unknown values are coded as 999}
 \item{Cloud_Perc}{Cloud cover at start of survey event; unknown values are coded as 999}
 \item{Tide_Stage}{Level of tide at start of survey event}
 \item{pk_EventID}{Primary key for this route survey event; unique ID for each record in this table}
}
}
\source{
Trocki, C. L., B. R. Mitchell, and P. W. C. Paton. 2015. Coastal breeding bird monitoring protocol 
for Boston Harbor Islands National Recreation Area: 2015 revision. Natural Resource Report NPS/NETN/NRR—2015/954. 
National Park Service, Fort Collins, Colorado. \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
}
\usage{
creche_raw
}
\description{
Raw creche data for COEI boat-based surveys
}
\examples{
\dontrun{
 creche_raw
}
}
\keyword{datasets}
