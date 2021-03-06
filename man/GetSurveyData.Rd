% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetSurveyData.R
\name{GetSurveyData}
\alias{GetSurveyData}
\title{Get survey effort per species}
\usage{
GetSurveyData(x, species = NA, survey = NA, connect = "ODBC", DBfile = NULL)
}
\arguments{
\item{x}{Denote in parentheses to return all surveys}

\item{species}{Enter species code as character if you would like to filter by species (e.g., "DCCO")}

\item{survey}{Enter survey type to filter for species detected within that survey type. Options are "Incubation","Nest", and "Creche".}

\item{connect}{Should the function connect to the Access DB? The default 
(\code{connect = `ODBC`}) is to try to connect using the Windows ODBC manager. 
If the connection is not available or not desired, one can use \code{connect = `Hmisc`}
and include a patch to a saved version of the database, or
the function can return the saved data from the package (\code{connect = `No`}). 
Note the saved data may not be up-to-date.}

\item{DBfile}{If \code{connect = `Hmisc`}, user must provide a location of the DB from which
it will grab the data.}
}
\value{
Returns a \code{data.frame} of survey effort (area and distance searched) by species for each survey event.
}
\description{
This function returns the survey effort per species for each island segment. 
This information can be filtered by species and survey type if desired.
}
\section{Warning}{

User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
}

\examples{
 
GetSurveyData(survey= "Nest")
GetSurveyData(species ="DCCO")
}
\seealso{
\url{ https://www.nps.gov/im/netn/coastal-birds.htm}
}
