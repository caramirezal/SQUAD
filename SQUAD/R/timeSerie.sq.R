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

        colores <- selectedColors()

        if ( correctDefault ) {

                if ( ( length(dynamic[1,] )) <= length(colores) ) {

                        indexes <- 1:length(dynamic[1,])

                }

                if (  length(dynamic[1,]) > length(colores)  ) {

                        warning("Number of nodes exceeds the maximum number of printable genes. Just the first 10 will be plotted.")

                        indexes <- 1:length(colores)

                }

        }


        if ( correctIndexes ) {

                if ( length(colores) < length(indexes) ) {

                        indexes <- indexes[1:length(colores)]

                        warning("Non default given indexes are more than the maximum number of printable genes.\nOnly the first 10 will be plotted")

                }
        }

        etiquetas <- colnames(dynamic)
        lineType<-rep(1,length(indexes))
        timePeriod <- times[length(times)]

        par(mar=c(6,6,2,8),xpd=TRUE)
        plot(times,dynamic[,indexes[1]],col=colores[1],
             ylim = c(0,1),
             ylab = "Node Level of activation",
             xlab = "Time",
             type = "l",lwd=3,frame.plot = TRUE)

        for (i in 2:( length(indexes) ) ){

                color <- colores[i]

                lines(times,dynamic[,indexes[i]],
                      col=color,type = "l",lwd=3)

        }
        legend(timePeriod + 0.05*timePeriod,
               y=1,
               cex = 1,
               legend=etiquetas[indexes],
               lwd = 2.5,
               lty=lineType,
               col = colores[1:length(indexes)],
               bty = "n")

}
