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
#' @param raw_count \code{TRUE} or \code{FALSE}. Plot effort-adjusted (default) or raw count data?
#' @param island A vector of island names (e.g., "Calf"). To view surveys summed across all islands, use "All Islands". WHen using "All Islands" you need to supply agrument to facet (such as "variable").
#' @param species  A  vector of species name codes, e.g. "BCNH"
#' @param var Select a variable to plot, typically a life stage (e.g., Eggs, Nests, Creche size). Defaults to all values.
#' @param scale Convert to log scale by entering "log".
#' @param year Calendar year(s) to view data by. Useful when wanting to view seasonal survey data in a year.
#' @param facet Plot the data into separate facets by Island, Species, or variable. Deafults to Island.
#' @param overlay_spp Defaults to \code{FALSE}. Enter \code{TRUE} if you would like to overlay time series for each species. This is needed to properly plot data when the \code{data.frame} to be plotted contains multiple species. In cases where the \code{data.frame} has multiple life stages (e.g., nests), provide argument to \code{var} to plot only one life stage at a time
#' @param print To not print plot enter "no".
#' @param  plot_title Add a caption to the plot? Defaults to "yes". Enter "no" for no caption.
#' @param legend Add legend. Defaults to \code{FALSE}.
#' @param Y_scale Should y-scale of individual facets be on the same scale (default, "fixed") or not ("free_y")?
#' @return Outputs a ggplot graph of species detections over time.
#' @seealso \url{https://www.nps.gov/im/netn/coastal-birds.htm}
#' @examples 
#' #Incubation surveys by year
#' dcco <- SumIncubation(time = "year", species = "DCCO")
#' PlotBirds(dcco)
#' 
#' # Incubation surveys by year of multiple species
#' incub<-SumIncubation(time = "year", species = c("DCCO", "HERG", "GBBG"))
#' PlotBirds(incub, overlay_spp= TRUE, raw_count= FALSE)
#' 
#' # Incubation surveys by date to view repeat effort
#' gbbg <- SumIncubation(time = "date", species = "GBBG")
#' PlotBirds(gbbg, year= "2012", legend =TRUE)
#' 
#' # Creche surveys by date; typically to view efforts in a single season
#' creche <- CrecheSum(time ="date")
#' # View survey counts in 2018
#' PlotBirds(creche, year = "2018")
#' # surveys summed across all islands
#' PlotBirds(creche, year = "2018", island= "All Islands", facet= "variable", legend = TRUE)
#' 
#' # Nest surveys
#' nests <- SumNestSurveys(time= "year", species = "BCNH")# annual counts of BCNH
#' PlotBirds(nests, var = "Nests")
#' PlotBirds(nests, island = "All Islands", facet= "variable", raw_counts= FALSE)
#' PlotBirds(nests, var= "EggsPerNest", legend =TRUE)
#' 
#' # Nest surveys of all species
#' nests<-SumNestSurveys(time= "year")
#' PlotBirds(nests, var = "Nests", overlay_spp= TRUE)
#' 
#' @export

PlotBirds<-function(data, raw_count= FALSE, species= NA, island=NA, year= NA, 
                    scale="norm", facet= "Island", var= NA, overlay_spp = FALSE, print= "yes", plot_title = "yes", legend= FALSE, Y_scale ="fixed"){
  
  library(ggplot2)
  
  # subset data
  graphdata <- data
  
  if(!anyNA(species)) graphdata<-graphdata[graphdata$Species_Code %in% species, ]
  
  if(!anyNA(island)) graphdata <- graphdata[graphdata$Island %in% island, ]
  
  if(!anyNA(var)) graphdata<-graphdata[graphdata$variable %in% var, ]
  
  if(!anyNA(year)) graphdata<-graphdata[graphdata$year %in% year, ] # for subsetting data ByDate
  
  if(facet == "Island") graphdata <- graphdata[!graphdata$Island %in% "All Islands", ]
  
  #if(!anyNA(method)) graphdata <- graphdata[!graphdata$Count_Method %in% "Direct Count", ]
  
  ### SETUP PLOTS 
  
  ## what value should be plotted? 
  #The raw counts (value) or effort-adjusted counts (valuePerSurveySize)
  if(!raw_count){
    # just rename the vector to grab effort adjusted counts
    graphdata$value<- graphdata$valuePerSurveySize
  }
  
  if(!overlay_spp){## overlay species
  
  if(scale == "log"){### MAKE LOG SCALE

    y2 <- ggplot(graphdata, 
                 aes(x=time, y= log(value), color= variable, group= variable)) +
      geom_point(size = 2) +
      geom_line() + scale_colour_viridis_d(option="D")
  }
  
  if(scale == "norm") {#### GROUP BY VARIABLE OR COMMONNAME
    #### GROUP BY VARIABLE
    y2 <- ggplot(graphdata, 
                 aes(x=time, y= value, color= variable, group= variable)) +
      geom_point(size=2) + 
      geom_line() + scale_colour_viridis_d(option="D")
    
    } 
      }else{
        if(scale == "log"){### MAKE LOG SCALE
          # group by CommonName
          y2 <- ggplot(graphdata, 
                       aes(x=time, y= log(value), color= CommonName, group= CommonName)) +
            geom_point(size = 2) +
            geom_line() + scale_colour_viridis_d(option="D")
        }else{
        
          # group by CommonName
    y2<-ggplot(graphdata, aes(x=time, y= value, colour= CommonName,group= CommonName))+
    geom_point()+ 
    geom_line()+ scale_colour_viridis_d(option="D")+
    ggtitle(paste0(if(!anyNA(var)) var, "Counts of ",graphdata$variable[1], " per ", facet))
        }
      }
  
 
## Setup y-axis labels
  
  if(!raw_count & scale =="log"){
    y2<-(y2+ labs(y = paste0("log(Number Detected) per ", graphdata$Survey_Units[1]), x= ""))}
  
  else if(!raw_count & scale =="norm"){
    y2<-(y2+ labs(y = paste0("Number Detected per ", graphdata$Survey_Units[1]), x= ""))}
  
  else if(raw_count & scale =="log"){
    y2<-(y2+ labs(y = "log(Number Detected) per Survey", x= ""))}
  
  else if(raw_count & scale =="norm"){
    y2<-(y2+ labs(y = "Number Detected per Survey", x= ""))}
  
  
### ADD FACETING 
  if(!anyNA(facet)) {
    y2 <- (y2 + facet_wrap(facet, scales = Y_scale, ncol = 3 ))
    
  }
#### TOGGLE plot title (above plot) 
  if(plot_title == "yes"){
    
    if(scale == "log"){
      y2<- (y2 + ggtitle(paste0("Log-transformed annual counts of ", graphdata$CommonName[1]," ", if(!anyNA(var)) var, " per ", facet)))
  }else{
    y2<- (y2 + ggtitle(paste0("Annual counts of ", graphdata$CommonName[1]," ", if(!anyNA(var)) var," per ", facet)))
  }
    }else{
      y2<- y2
    }

  ###DEFINE THEME
  y2 <- (y2 +
         {if(legend) theme(legend.position = "top")}+
         {if(!legend)theme(legend.position = "none")}+
         theme(axis.text.y = element_text(color="black", vjust= 0.5, size = 12)) +
         theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 12 )) +
          theme(axis.text.y = element_text(size = 12 )) +
         theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
         theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F)) +
         theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F)) +
         theme(panel.background =  element_rect(fill="white", colour="black")) +
         theme(panel.grid.major = element_line(colour = "grey90")) +
         theme(plot.title=element_text(size=12, vjust=2, face= "bold")) +
         theme(strip.background= element_rect(size=10, color="gray" )))
  
#### CHOOSE TO PRINT ON EXECUTION OR CREATE OBJECT; THE LATTER HELPFUL WHEN LOOPING  
  
  if(print== "yes"){
    
    suppressWarnings(print(y2))
    
  }
  if(print== "no"){
  
  return(y2)
    }
}
