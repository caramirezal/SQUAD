#' function similar to plotSequence() in boolnet. Plot the time serie of the regulatory network based on SQUAD method
#' in various formats (heatmap and time-serie)
#' @description  Plot trajectories of continuous interpolation of boolean regulatory network models.
#' @name plotSequence.sq 
#' @export plotSequence.sq 
#' @param net an object of the class "squad" as that defined by asContinuous(). Or alternatively, an object of the class
#' "BoolNet" as the result of applying the loadNetwork() function of the BoolBet  R Package.
#' @param initialState  a vector of length equal to the number of nodes in the network. The entries of the vectors
#' are real values in the interval [0,1]. If no initial States are provided a random state is generated.
#' @param indexes a vector of length less or equal to the number of nodes in the network. The entries of the vector are
#' the indexes of the nodes to be plotted. Default values are the first 13 nodes of the network. The maximum printable
#' number of nodes curves are 13.
#' @param parameters a list with to vectors. Each vector has length equal to the number of nodes of the network.
#' The vectors correspond to h and gamma parameters needed to define the squad ODE system as that defined in 
#' Mart\'inez-Sosa, 2013.
#' @param timePeriod the virtual time duration of the simulation.
#' @param lengthInterval the time steps for integration to be shown. The lesser length interval provided the better
#' resolution of the curves would be.
#' @param ... additional parameters passed to ode() solver function defined in the deSolve R Package 
#' developed by Soetaert, 2010.
#' @examples 
#' library(SQUAD)
#' data("cellcycle")
#' cellcycle.sq <- asContinuous(cellcycle)
#' plotSequence.sq(cellcycle.sq,initialState = generateState(cellcycle,specs = c("CycD"=1)),
#'                 format = "heatmap")
#' 
#' 
plotSequence.sq<-function(net,initialState="random",indexes="default",parameters="default",
                          timePeriod=10,lengthInterval=0.01,format="heatmap",...){
        
        sim <- squad(net, initialState=initialState, parameters=parameters,
                     timePeriod=timePeriod, lengthInterval=lengthInterval, ...)
        
        if ( format == "heatmap") {
                
                heatmap.sq(sim = sim,indexes = indexes)
                
        } else if ( format == "timeserie" ) {
                
                timeSerie.sq(dynamic = sim,indexes = indexes)
                
        } else {
                
                cat("Please select a valid format: 'heatmap' or 'timeserie'","\n")
        }
        
        
}

##################################################################################################



# select colors for plotting
selectedColors <- function(){
        colores <- c("gray","green","orange","red","blue","darkmagenta","darkturquoise","brown",
                     "yellow","darkgreen","pink","black","magenta")
        return(colores)
}



