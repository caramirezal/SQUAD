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
#' @param ... the parameters of ode() solver function defined in the deSolve R Package 
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





###########################################################################################################################

# plot time serie as a heatmap
## Modificar squad solo debe dar como output el resultado del odeSOlver
## teniendo como parámetros si se una un objecto de BoolNet o de SQUAD
squad <- function(net, initialState="random", 
                  parameters="default", timePeriod=7.5, 
                  lengthInterval=0.01, ...) {
        
        if ( (class(net) != "BooleanNetwork") && (class(net) != "SQUAD") ) {
                stop('A net object of class "BooleanNetwork" or "SQUAD" most be provided!')
        }
        
        times <- seq(0,timePeriod,by = lengthInterval)
        
        if (length(initialState)==1) { 
                
                if (initialState=="random"){
                        
                        initialState<-runif(length(net$genes),min=0,max = 1)
                }
        }
        
        if (length(parameters)==1) { 
                
                if (parameters=="default") {
                        
                        gamma <- rep(1,length(net$genes))
                        h <- rep(50,length(net$genes))
                        parameters<-list(h,gamma)
                }
                
        }
        
        # Usar ...
        # agregar opciones BooleanNetwork y SQUAD
        if (class(net) == "BooleanNetwork") {
                
                net.sq <- asContinuous(net)
                dynamic<-ode(y=initialState,times=times,func=net.sq$fun,
                             parms = parameters,atol=10e-6, rtol=10e-6,...)
        }
        
        if (class(net) == "SQUAD") {
                
                dynamic<-ode(y=initialState,times=times,func=net$fun,
                             parms = parameters,atol=10e-6, rtol=10e-6,...)
        }
        

        colnames(dynamic) <- c("time",net$genes)
        dynamic
}
