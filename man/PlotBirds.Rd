% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotBirds.R
\name{PlotBirds}
\alias{PlotBirds}
\title{Plot coastal bird survey data}
\usage{
PlotBirds(
  data,
  raw_count = FALSE,
  species = NA,
  island = NA,
  year = NA,
  stat = NA,
  scale = "norm",
  facet = "Island",
  var = NA,
  overlay = "var",
  print = TRUE,
  plot_title = TRUE,
  legend = FALSE,
  Y_scale = "fixed"
)
}
\arguments{
\item{data}{A \code{data.frame}  of coastal bird observations summarized for plotting. Typically from \code{\link{SumIncubation}}, \code{\link{CrecheSum}}, or \code{\link{SumNestSurveys}}.}

\item{raw_count}{\code{TRUE} or \code{FALSE}. Plot effort-adjusted (default) or raw count data?}

\item{species}{Character. A  vector of species name codes, e.g. "BCNH"}

\item{island}{A vector of island names (e.g., "Calf"). To view surveys summed across all islands, use 
"All Islands". When using "All Islands" you need to supply agrument to facet (such as "variable").}

\item{year}{Numeric. Calendar year(s) to view data by. Useful when wanting to view seasonal survey data in a year.}

\item{stat}{Character. Specify type of statistic to plot. Defaults to plotting the sum, but you can plot other
annual statistics by setting to "mean","max", or "min" when the \code{df} is generated by \code{\link{SumIncubation}} or \code{\link{CrecheSum}} functions.}

\item{scale}{Character. Convert to log scale by entering "log".}

\item{facet}{Character. Plot the data into separate facets by Island, Species, or variable. Defaults to Island. If \code{overlay = "island"} set \code{facet} to "CommonName", "Species_Code" , "FullLatinName" or "variable".}

\item{var}{Character. Select a variable to plot, typically a life stage (e.g., Eggs, Nests, Creche size). Defaults to all values.}

\item{overlay}{Character. Defaults to \code{"var"} which overlays data by lifestage. Enter \code{"species"} to overlay time series 
by species when \code{data} contains multiple species. Enter \code{"island"} to overlay data by island. When overlaying by island and 
when multiple species are in \code{data}, denote \code{facet} as "CommonName", "Species_Code" or "FullLatinName".
In cases where the \code{data} has multiple life stages (e.g., nests), provide argument to \code{var} to plot only one life stage at a time.}

\item{print}{Logical. Defaults to \code{TRUE} to print plot. If \code{FALSE} outputs graph data as \code{y2$data} object}

\item{plot_title}{Logical. Add a caption to the plot? Defaults to \code{TRUE}.}

\item{legend}{Logical. Add legend. Defaults to \code{FALSE}.}

\item{Y_scale}{Character. Should y-scale of individual facets be on the same scale (default, "fixed") or not ("free_y")?}
}
\value{
Outputs a ggplot graph of species detections over time.
}
\description{
Plots bird detections over time.
}
\examples{
#Incubation surveys by year
dcco <- SumIncubation(time = "year", species = "DCCO",stat ="mean")
PlotBirds(dcco)

# Incubation surveys by year of multiple species
incub<-SumIncubation(time = "year", species = c("DCCO", "HERG", "GBBG"))
PlotBirds(incub, overlay= "species", raw_count= FALSE, stat ="mean", plot_title ="no")
PlotBirds(incub, overlay= "island", raw_count= FALSE, stat ="mean", plot_title ="no", facet="CommonName", legend= T) 
# Incubation surveys by date to view repeat effort
gbbg <- SumIncubation(time = "date", species = "GBBG")
PlotBirds(gbbg, year= "2012", legend =TRUE, stat ="sum")

# Creche surveys by date; typically to view efforts in a single season
creche <- CrecheSum(time ="date")
# View survey counts in 2018
PlotBirds(creche, year = "2018", stat= "sum")
# surveys summed across all islands
PlotBirds(creche, year = "2018", island= "All Islands", facet= "variable", legend = TRUE, stat= "sum")

# Nest surveys
nests <- SumNestSurveys(time= "year", species = "BCNH")# annual counts of BCNH
PlotBirds(nests, var = "Nests")
PlotBirds(nests, island = "All Islands", facet= "variable", raw_counts= FALSE)
PlotBirds(nests, var= "EggsPerNest", legend =TRUE)

# Nest surveys of all species
nests<-SumNestSurveys(time= "year")
PlotBirds(nests, var = "Nests", overlay= "species")

}
\seealso{
\url{https://www.nps.gov/im/netn/coastal-birds.htm}
}
