#' @include SumIncubation.R
#' @include SumNestSurveys.R
#' @include CrecheSum.R
#' 
#' @title Plot coastal bird survey data
#'
#' @import ggplot2 
#'
#' @description Plots bird detections over time.
#' @section Warning:
#' User must have Access backend entered as 'NETNCB' in Windows ODBC manager.
#' @param data A \code{data.frame}  of coastal bird observations summarized for plotting. Typically from \code{\link{SumIncubation}}, \code{\link{CrecheSum}}, or \code{\link{SumNestSurveys}}.
#' @param island A vector of island names (e.g., "Calf"). To view surveys summed across all islands, use "All Islands". WHen using "All Islands" you need to supply agrument to facet (such as "variable").
#' @param species  A  vector of species name codes, e.g. "BCNH"
#' @param var Select a variable to plot, typically a life stage (e.g., Eggs, Nests, Creche size). Defaults to all values.
#' @param scale Convert to log scale by entering "log".
#' @param year Calendar year(s) to view data by. Useful when wanting to view seasonal survey data in a year.
#' @param facet Plot the data into separate facets by Island, Species, or variable. Deafults to Island.
#' @param overlay_spp Defaults to \code{FALSE}. Enter \code{TRUE} if you would like to overlay time series for each species. This is needed to properly plot data when the \code{data.frame} to be plotted contains multiple species. In cases where the \code{data.frame} has multiple life stages (e.g., nests), provide argument to \code{var} to plot only one life stage at a time
#' @return Outputs a ggplot graph of species detections over time.
#' @seealso \url{ https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples 
#' # Incubation surveys by year
#' dcco <- SumIncubation(time = "year", species = "DCCO")

#' PlotBirds(dcco)
#' 
#' # Incubation surveys by year of multiple species
#' incub<-SumIncubation(time = "year", species = c("DCCO", "HERG", "GBBG"))
#' PlotBirds(incub, overlay_spp= TRUE)
#' 
#' # Incubation surveys by date to view repeat effort
#' lete <- SumIncubation(time = "date", species = "COTE")
#' PlotBirds(lete, year= "2012")
#' 
#' # Creche surveys by date; typically to view efforts in a single season
#' creche <- CrecheSum(time ="date")
#' # View survey counts in 2018
#' PlotBirds(creche, year = "2018")
#' # surveys summed across all islands
#' PlotBirds(creche, year = "2018", island= "All Islands", facet= "variable")
#' 
#' # Nest surveys
#' nests <- SumNestSurveys(time= "year", species = "BCNH")# annual counts of BCNH
#' PlotBirds(nests, var = "Nests")
#' PlotBirds(nests, island = "All Islands", facet= "variable")
#' 
#' # Nest surveys of all species
#' nests<-SumNestSurveys(time= "year")
#' PlotBirds(nests, var = "Nests", overlay_spp= TRUE)
#' 
#' @export

PlotBirds<-function(data, species= NA, island=NA, year= NA, 
                    scale="norm", facet= "Island", var= NA, overlay_spp = FALSE){
  
  library(ggplot2)
  
  # subset data
  graphdata <- data
  
  if(!anyNA(species)) graphdata<-graphdata[graphdata$Species_Code %in% species, ]
  
  if(!anyNA(island)) graphdata <- graphdata[graphdata$Island %in% island, ]
  
  if(!anyNA(var)) graphdata<-graphdata[graphdata$variable %in% var, ]
  
  if(!anyNA(year)) graphdata<-graphdata[graphdata$year %in% year, ] # for subsetting data ByDate
  
  if(facet == "Island") graphdata <- graphdata[!graphdata$Island %in% "All Islands", ]
  
  # graphdata<-graphdata[na.omit(graphdata),]
  # #graphdata<-droplevels(graphdata)
  # 
  
  # setup plot
  
  if(!overlay_spp){
  
  if(scale == "log"){

    
    y2 <- ggplot(graphdata, 
                 aes(x=time, y= log(value), colour= variable, group= variable)) +
      geom_point(size = 2) + 
      geom_line() +
      labs(y = "log(Number Detected)", x= "") +
      ggtitle(paste0(if(!anyNA(var)) var, " counts of ", graphdata$CommonName[1], 
                     " per ", facet))
  }
  
  if(scale == "norm") {
    
    y2 <- ggplot(graphdata, 
                 aes(x=time, y= value, colour= variable, group= variable)) +
      geom_point(size=2) + 
      geom_line() +
      labs(y = "Number Detected", x= "") +
      ggtitle(paste0(if(!anyNA(var)) var, " counts of ", graphdata$CommonName[1], 
                     " per ", facet))
    
    } 
      }else{
    
    y2<-ggplot(graphdata, aes(x=time, y= value, colour= CommonName,group= CommonName))+geom_point(size=2)+
    geom_line()+
    labs(y = "Number Detected", x= "")+
    ggtitle(paste0(if(!anyNA(var)) var, " counts of ",graphdata$variable[1], " per ", facet))
  
      }

  if(!anyNA(facet)) {
    y2 <- (y2 + facet_wrap(facet, scales = "free_y" ))
  }
  
  y2 <- (y2 +
         theme(legend.position = "top", legend.text = element_text(size = 12), 
               legend.title = element_blank()) +
         theme(axis.text.y = element_text(color="black", vjust= 0.5, size = 13, 
                                          face="bold")) +
         theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 12 )) +
         theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
         theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F)) +
         theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F)) +
         theme(panel.background =  element_rect(fill="white", colour="black")) +
         theme(panel.grid.major = element_line(colour = "grey90")) +
         theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
         theme(strip.background= element_rect(size=10, color="gray" )))
  
 # theme_ipsum()+scale_colour_ipsum(guide = FALSE) + ) 
  
  print(y2)
  
}
