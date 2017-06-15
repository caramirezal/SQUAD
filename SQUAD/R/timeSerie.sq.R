#' timeSerie.sq plots regulatory network dynamic as a time serie
#' @description timeSerie.sq plots regulatory network dynamic as a time serie.
#' @name timeSerie.sq
#' @export timeSerie.sq
#' @param dynamic a trajectory of a continuous interpolation regulatory network model. A data frame as the result
#' of applying the ode() or sqaud() solver function.
#' @param indexes a vector of numerical or integer which corresponds to the node indexes are to be plotted.
#' @examples 
#' library(SQUAD)
#' data("cellcycle")
#' cellcycle.sq <- asContinuous(cellcycle)
#' sim <- squad(cellcycle.sq,initialState = generateState(cellcycle,specs = c("CycD"=1)))
#' timeSerie.sq(sim,indexes = c(1,2,3,4))

timeSerie.sq <- function(dynamic,indexes="default") {
        
        times <- dynamic[,1]
        
        dynamic <- dynamic[,2:length(dynamic[1,])]
        
        correctDefault <- ( length(indexes) == 1 ) && ( indexes == "default" ) 
        
        correctIndexes <- ( length(indexes) <= length(dynamic) )  && ( class(indexes) %in% c("numeric","integer")  )
        
        if ( ! ( correctDefault | correctIndexes  ) ) {
                
                stop("Indexes most be a vector numeric round numbers or integers of length less than the number of nodes ")       
        }
        
        par(mar=c(3, 3, 3, 6),xpd=TRUE)
        
        colores <- selectedColors()
        
        if ( correctDefault ) {
                
                if ( ( length(dynamic[1,] )) <= length(colores) ) {
                        
                        indexes <- 1:length(dynamic[1,])
                        
                }
                
                if (  length(dynamic[1,]) > length(colores)  ) {
                        
                        print("Number of nodes exceeds the maximum number of printable genes. Just the first 13 will be plotted.")
                        
                        indexes <- 1:length(colores)
                        
                }
                
        }
        
        
        if ( correctIndexes ) {
                
                if ( length(colores) < length(indexes) ) {
                        
                        indexes <- indexes[1:length(colores)]
                        
                        print("Non default given indexes are more than the maximum number of printable genes.\nOnly the first 26 will be plotted")
                        
                }
        }
        
        etiquetas <- colnames(dynamic)
        lineType<-rep(1,length(indexes))
        timePeriod <- times[length(times)]
        
        plot(times,dynamic[,indexes[1]],col=colores[1],
             ylim = c(0,1),
             xlab = "Time",ylab = "Node Level of activation",
             type = "l",lwd=2.5,frame.plot = TRUE)
        
        for (i in 2:( length(indexes) ) ){
                
                color <- colores[i]
                
                lines(times,dynamic[,indexes[i]],
                      col=color,type = "l",lwd=2.5)
                
        }
        
        legend(timePeriod+log(timePeriod),1,cex = 0.8,legend=etiquetas[indexes],lwd = 2.5,
               lty=lineType,col = colores[1:length(indexes)],bty = "n")
        
}