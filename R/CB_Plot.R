
#' @title CBPlot
#'
#' @importFrom dplyr group_by right_join left_join summarise
#' @importFrom ggplot2 ggplot aes element_line geom_point ggplot ggtitle labs scale_x_continuous theme theme_minimal facet_wrap
#' @importFrom magrittr %>%
#' @importFrom tidyr add_column
#'
#' @description Plots bird detections over time.
#'
#' @param object A \code{data.frame}  of coastal bird observations
#' @param time  A numeric vector. Indicates which years should be graphed.
#' @param island A  vector of island names
#' @param species  A  vector of species names
#' @param scale Convert to log scale by entering "log"
#' @param facet Plot the data into separate facets by Island, Species, etc.
#'
#' @details This function produces a graph of species detections over time.
#'
#' @export

########################

setGeneric(name="CBPlot",function(object, species=NA, island=NA, var=NA, scale=NA, year= NA,
                                  facet=NA, ...){standardGeneric("CBPlot")}, signature="object")


setMethod(f="CBPlot",  signature=c(object="data.frame"),
          function(object, species=NA, island=NA, var=NA, scale=NA,year= NA, facet=NA, ...) {
            
            ## returns the subsetted df based on the inputs

            graphdata<-object

                   if(!anyNA(species)) graphdata<-graphdata[graphdata$Species_Code %in% species,]

                   if(!anyNA(island)) graphdata<-graphdata[graphdata$Island %in% island,]

                   if(!anyNA(variable)) graphdata<-graphdata[graphdata$variable %in% variable,]

                   if(!anyNA(facet)) graphdata<-graphdata[!graphdata$Island %in% "All Islands",]
                   
                   if(!anyNA(year)) graphdata<-graphdata[graphdata$year %in% year,]
                   

          ## plotting
                   
                   if(anyNA(scale)){
                     
                     GraphOut<-ggplot(graphdata, aes(x=time, y= value, colour= variable,group= variable))+
                       labs(y = paste0("Number Detected"), x= "")+
                       geom_point(size=2)+ geom_line()
                     
                   }
                   
                   if(scale == "log"){

            
            GraphOut<-ggplot(graphdata, aes(x=time, y= log(value), colour= variable,group= variable))+
                       labs(y = paste0("log(Number Detected)"), x= "")+
                       geom_point(size=2)+ geom_line()
              }
                   
          


              GraphOut<-(y2+facet_wrap(~facet, scales = "free_y" )+
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
