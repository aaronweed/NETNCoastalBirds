
#' @title CBPlot_generic
#'
#' @importFrom ggplot2 aes element_line geom_point geom_line ggplot ggtitle labs  theme theme_minimal facet_wrap element_text element_rect
#'
#' @description Plots bird detections over time.
#'
#' @param object A \code{data.frame}  of coastal bird observations
#' @param island A  vector of island names. To view summariaes across all islands, "All Islands"
#' @param species  A  vector of species names, e.g. "BCNH"
#' @param var Select a variable to plot, typically a life stage (e.g., eggs, nests, creche size). Defaults to all values.
#' @param scale Convert to log scale by entering "log"
#' @param year Calendar year(s) to view data by. Useful when wanting to view seasonal survey data in a year.
#' @param facet Plot the data into separate facets by Island, Species, etc. 
#'
#' @details This function produces a graph of species detections over time.
#'
#' @export

########################

setGeneric(name="CBPlot",function(object, island=NA, species=NA,year= NA, var=NA, scale, facet = NA,
                                 ...){standardGeneric("CBPlot")}, signature="object")

#facet="Island"
setMethod(f="CBPlot",  signature=c(object="data.frame"),
          function(object, island, species, year,  var, scale="norm",facet= "Island", ...) {
            
            ## returns the subsetted df based on the inputs

            graphdata<-object

            if(!anyNA(island)) graphdata<-graphdata[graphdata$Island %in% island,]
            
              if(facet == "Island") graphdata<-graphdata[!graphdata$Island %in% "All Islands",]
            
            if(!anyNA(year)) graphdata<-graphdata[graphdata$year %in% year,]
            
            if(!anyNA(species)) graphdata<-graphdata[graphdata$Species_Code %in% species,]

            if(!anyNA(var)) graphdata<-graphdata[graphdata$variable %in% var,]


          ## plotting
                   
                   
                   if(scale == "log"){
                     
                     
                     GraphOut<-ggplot(graphdata, aes(x=time, y= log(value), colour= variable,group= variable))+
                       labs(y = paste0("log(Number Detected)"), x= "")+
                       geom_point(size=2)+ geom_line()
                   } 
                   else{
                     
                     GraphOut<-ggplot(graphdata, aes(x=time, y= value, colour= variable,group= variable))+
                       labs(y = paste0("Number Detected"), x= "")+
                       geom_point(size=2)+ geom_line()
                     
                   }
                   
                   if(!is.null(facet)){

                     GraphOut<-(GraphOut+facet_wrap(facet, scales = "free_y" ))
                   }


              GraphOut<-(GraphOut+
                                theme(legend.position = "right", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
                                theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
                                theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 14 , face="bold")) +
                                theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
                                theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F))+
                                theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                                theme(panel.background =  element_rect(fill="white", colour="black")) +
                                theme(panel.grid.major = element_line(colour = "grey90"))+
                                theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                                theme(strip.background= element_rect(size=10, color="gray" )))

                   return(GraphOut)

                         })
