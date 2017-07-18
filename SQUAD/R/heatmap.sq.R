#' heatmap.sq draws a heatmap from the ODE simulations. In general, this function can plot any result
#'  derived from ode() function of the deSolve R Package
#' @description draw a heatmap of the simulation of a trajectory of the regulatory network using a
#' a continuous formalism.
#' @name heatmap.sq
#' @export heatmap.sq
#' @param sim a trajectory of a continuous interpolation regulatory network model. A data frame as the result
#' of applying the ode() solver function.
#' @param indexes a vector of numerical or integer which corresponds to the node indexes are to be plotted.
#' @examples
#' library(SQUAD)
#' data("cellcycle")
#' cellcycle.sq <- asContinuous(cellcycle)
#' sim <- squad(cellcycle.sq,initialState = generateState(cellcycle,specs = c("CycD"=1)))
#' heatmap.sq(sim,indexes = c(1,2,3,4))

heatmap.sq <- function(sim, indexes="default") {

        correctDefault <- ( length(indexes) == 1 ) && ( indexes == "default" )

        correctIndexes <- ( length(indexes) <= length(sim) )  && ( class(indexes) %in% c("numeric","integer")  )

        if ( ! ( correctDefault | correctIndexes  ) ) {

                stop("Indexes most be a vector numeric round numbers or integers of length less than the number of nodes ")
        }

        times <- sim[,"time"]
        if ( correctDefault ){

                if ( indexes == "default"  ) {

                        sim <- sim[,2:length(sim[1,])]
                        rownames(sim) <- rep(" ",length(sim[,1]))
                        rownames(sim)[1] <- 0
                        rownames(sim)[length(times)] <- times[length(times)]

                        heatmap(sim,Rowv = NA,Colv = NA,ylab = "Time",cexRow = 1,
                                col=colorRampPalette(c("black","green"))(6))
                }
        }

        if (  correctIndexes ) {

                sim <- sim[,2:length(sim[1,])]
                rownames(sim) <- rep(" ",length(sim[,1]))
                rownames(sim)[1] <- 0
                rownames(sim)[length(times)] <- times[length(times)]

                heatmap(sim[,indexes],Rowv = NA,Colv = NA,ylab = "Time",cexRow = 1,
                        col=colorRampPalette(c("black","green"))(6) )

        }
}
