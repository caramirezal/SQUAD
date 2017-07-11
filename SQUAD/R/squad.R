#' squad performs simulations of regulatory network models using the SQUAD methodology
#'
#' @description Performs a variety simulations of Regulatory network models using the
#' SQUAD methodology. It can be used to set up different initial states, parameters and
#' also to simulate gain/loss of function mutants. It returns a trajectory of the system
#' in different formats
#' @name squad
#' @export squad
#' @param net an object of class "BooleanNetwork" or "SQUAD"
#' @param initialState a vector of variables that belongs to the interval $[0,1]$ which
#' serves as initial values for the ODE system simulation.
#' @param parameters a list containing two vectors of length n. Where n is the number of nodes
#' of the network. The vectors are the beta and gamma parameters as that defined in
#' Mart\'inez-Sosa, 2013.
#' @param ... additional parameters passed to ode() solver function defined in the deSolve R Package
#' developed by Soetaert, 2010.
#'
#' @examples
#' net <- loadNetwork("cartoonNetwork.R")
#' getAttractorsAsynchronous(net)
#' data("cellcycle")
#' net.sq <- asContinuous(cellcycle)
#' initialState <- runif(length(cellcycle$genes),0,1)
#' times <- seq(0,10,by=0.5)
#' h <- rep(50,length(cellcycle$genes))
#' gamma <- rep(1,length(cellcycle$genes))
#' parameters <- list(h,gamma)
#' sim <- squad(net.sq)
#' sim



adjusTime <- function(value,timeInterval) {
        if ( value < 0 | max(timeInterval) <= value) {
                stop("One or more time events are not in the simulation time Interval!")
        }
        counter <- 1
        while ( timeInterval[counter] <= value ) {
                counter <- counter + 1
        }
        timeInterval[counter]
}



###########################################################################################################################

# plot time serie as a heatmap
## Modificar squad solo debe dar como output el resultado del odeSOlver
## teniendo como parámetros si se una un objecto de BoolNet o de SQUAD
squad <- function(net, initialState="random",
                  parameters="default", timePeriod=7.5,
                  lengthInterval=0.01,
                  type = "squad",
                  fixed = "default",
                  perturbations = FALSE,
                  events = list(),
                  plot = "Null",
                  ...) {

        if ( ! ( class(net) %in% c("BooleanNetwork","squad") ) ) {
                stop('A net object of class "BooleanNetwork" or "squad" most be provided!')
        }

        times <- seq(0,timePeriod,by = lengthInterval)

        if ( perturbations ) {
                specs <- events
                specs$time <- sapply(events$time,
                                        function(x) adjusTime(x,times))
        }


        if (length(initialState)==1) {
                if (initialState=="random"){
                        initialState<-runif(length(net$genes),min=0,max = 1)
                }
        }
        names(initialState) <- net$genes
        #if (length(parameters)==1) {

        #        if (parameters=="default") {

        #                gamma <- rep(1,length(net$genes))
        #                h <- rep(50,length(net$genes))
        #                parameters<-list(h,gamma)
        #        }

        #}

        ## setting mutants
        if ( length(fixed) == 1 ) {
                if ( fixed != "default" ) {
                        fixedGene <- names(fixed)
                        initialState[fixedGene] <- fixed[fixedGene]
                        #cat(initialState)
                }
        }
        if ( length(fixed) > 1 ) {
                fixedGenes <- names(fixed)
                initialState[fixedGenes] <- fixed[fixedGenes]
        }

        if ( class(net) == "BooleanNetwork" ) {
                net.sq <- asContinuous(net,
                                       parameters = parameters,
                                       fixed = fixed)
                if ( type == "squad" ) {
                        if ( ! perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$squad,
                                     parms = parameters,
                                     fixed = fixed,
                                     atol=10e-6,
                                     rtol=10e-6,...)
                        }
                        if ( perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$squad,
                                     parms = parameters,
                                     fixed = fixed,
                                     events = list(data=specs),
                                     atol=10e-6,
                                     rtol=10e-6,...)
                        }

                }
                if ( type == "normHillCubes" ) {
                        if ( ! perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$normHillCubes,
                                     fixed = fixed,
                                     parms = parameters,
                                     atol=10e-6,
                                     rtol=10e-6,...)
                        }
                        if ( perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$normHillCubes,
                                     fixed = fixed,
                                     parms = parameters,
                                     events = list(data=specs),
                                     atol=10e-6,
                                     rtol=10e-6,...)
                        }

                }
        }
        if ( class(net) == "squad" ) {
                if ( type == "squad" ) {
                        if ( ! perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net$squad,
                                     parms = parameters,
                                     fixed = fixed,
                                     atol=10e-6,
                                     rtol=10e-6,
                                     ...)
                        }
                        if ( perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net$squad,
                                     parms = parameters,
                                     fixed = fixed,
                                     events = list(data=specs),
                                     atol=10e-6,
                                     rtol=10e-6,
                                     ...)
                        }

                }
                if ( type == "normHillCubes" ) {
                        net.sq <- net[c("interactions","genes","fixed")]
                        class(net.sq) <- "BooleanNetwork"
                        net.sq <- asContinuous(net.sq,
                                               parameters = parameters,
                                               fixed = fixed)
                        if ( ! perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$normHillCubes,
                                     parms = parameters,
                                     fixed = fixed,
                                     atol=10e-6,
                                     rtol=10e-6,...)
                        }
                        if ( perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$normHillCubes,
                                     parms = parameters,
                                     fixed = fixed,
                                     events = list(data=specs),
                                     atol=10e-6,
                                     rtol=10e-6,...)
                        }
                }
        }
        colnames(dynamic) <- c("time",net$genes)

        if ( plot == "Null") {
                return(dynamic)
        }
        if ( plot == "timeSerie") {
                timeSerie.sq(dynamic)
        }

}
