
#' @title CBPlot
#'
#' @importFrom ggplot2 aes element_line geom_point geom_line ggplot ggtitle labs  theme theme_minimal facet_wrap element_text element_rect
#'
#' @description Plots bird detections over time.
#'
#' @param data A \code{data.frame}  of coastal bird observations
#' @param island A  vector of island names. To view summariaes across all islands, "All Islands"
#' @param species  A  vector of species name codes, e.g. "BCNH"
#' @param var Select a variable to plot, typically a life stage (e.g., Eggs, Nests, Creche size). Defaults to all values.
#' @param scale Convert to log scale by entering "log"
#' @param year Calendar year(s) to view data by. Useful when wanting to view seasonal survey data in a year.
#' @param facet Plot the data into separate facets by Island, Species, etc. 
#'
#' @details This function produces a graph of species detections over time.
#'
#' @export


plotCB<-function(data, species= NA, island=NA, year= NA, scale="norm", facet= "Island", var= NA){
  
  library(ggplot2)
  
  # subset data
  graphdata<-data
  
  if(!anyNA(species)) graphdata<-graphdata[graphdata$Species_Code %in% species,]
  
  if(!anyNA(island)) graphdata<-graphdata[graphdata$Island %in% island,]
  
  if(!anyNA(var)) graphdata<-graphdata[graphdata$variable %in% var,]
  
  if(!anyNA(year)) graphdata<-graphdata[graphdata$year %in% year,] # for subsetting data ByDate
  
  if(facet == "Island") graphdata<-graphdata[!graphdata$Island %in% "All Islands",]
  
  # graphdata<-graphdata[na.omit(graphdata),]
  # #graphdata<-droplevels(graphdata)
  # 
  
  # setup plot
  
  if(scale == "log"){
    
    y2<-ggplot(graphdata, aes(x=time, y= log(value), colour= variable,group= variable))+geom_point(size=2)+ 
      geom_line()+
      labs(y = "log(Number Detected)", x= "")+
      ggtitle(paste0(if(!anyNA(var)) var, " counts of ",graphdata$CommonName[1], " per ", facet))
  }
  
  if(scale == "norm"){
    
    y2<-ggplot(graphdata, aes(x=time, y= value, colour= variable,group= variable))+geom_point(size=2)+ 
      geom_line()+
      labs(y = "Number Detected", x= "")+
      ggtitle(paste0(if(!anyNA(var)) var, " counts of ",graphdata$CommonName[1], " per ", facet))
    
  }
  
  
  if(!anyNA(facet)){
    
    y2<-(y2+facet_wrap(facet, scales = "free_y" ))
  }
  
  y2<-(y2+
         theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_blank()) +
         theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
         theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 12 )) +
         theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
         theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F))+
         theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
         theme(panel.background =  element_rect(fill="white", colour="black")) +
         theme(panel.grid.major = element_line(colour = "grey90"))+
         theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
         theme(strip.background= element_rect(size=10, color="gray" )))
  
 # theme_ipsum()+scale_colour_ipsum(guide = FALSE) + ) 
  
  print(y2)
  
}
