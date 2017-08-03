#' @export
#' @description asContinuous transforms a boolean regulatory network model in BooleanNet format to
#' a continuous model object that can be supplied to ode() function from deSolve R package
#' Algorithm:
#' extract w vector of parameters w_i through the application of fuzzy logic functions
#' according to the Boolean functions of the network.
#' Return f(w). Where f is the generic SQUAD function (see SQUAD() function in this script
#' for the definition).
#' @param net an object of the class "BooleanNetwork" as that obtained by loadNetwork()
#' BoolNet function
#' @usage asContinuous(net)
#' @details The continuous network object retrieved by the function represent a system of ordinary
#' equations according to the methodology described in Mart\'inez-Sosa, 2013 \cite{martinez2013}.
#' This methodology have the same fixed point attractors of that in the Boolean model but have the
#' advantage that it takes into account kinetic parameters.
#' @examples
#' > data(cellCycle)
#' # setting initial state
#' state <- generateState(cellCycle,specs=c("CycA"=1,"CycD"=1))
#' # setting intervals for numerical integration
#' times <- seq(min=0,max=1,seq.out=100)
#' cellCycle.s <- asContinuous(cellCycle)
#' simulations <- ode(y=state, fun=cellCycle.s$fun, times=times,
#'                    parameters = "default")
#' # note that cellCycle$fun can be given as the function
#' # parameter to ode solver method
#' simulations
#' @return an allready to use function to pass as argument to ODE() fun parameter, or to use
#' in combination with squad()
#' @name asContinuous
#' @title asContinuous

asContinuous <- function(net,parameters="default",fixed="default") {
        if ( ! ( class(net) %in% c("BooleanNetwork","squad","normHillCubes" ) ) ) {
                stop("A net object of the class 'BooleanNetwork' or 'squad' or 'normHillCubes' most be provided!")
        }
        if ( ! ( class(parameters) %in% c("list","character") ) ) {
                stop("The 'parameters' argument most be a list or a default character.")
        }

        # BoolNet to squad format
        if ( class(net) == "BooleanNetwork") {
                res <- list("genes" = net$genes, "interactions"=net$interactions,
                            "fixed" = net$fixed)
                if ( ! ( length(parameters) %in% c(1,2,3) ) ) {
                        stop("The 'parameters' argument most contain two or three numerical vectors or be set to 'default'. ")
                }
                network <- function(times,state,parameters,fixed) {
                        with(as.list(c(state,parameters,fixed)),{
                                newState.vect<-rep(0,length(net$genes))
                                ## Definition of parameters h, w and gamma.
                                if (length(parameters)==1) {
                                        if (parameters=="default") {
                                                gamma <- rep(1,length(net$genes))
                                                h <- rep(50,length(net$genes))
                                        }
                                }
                                if ( length(parameters) > 1 ) {
                                        h<-parameters$h
                                        gamma<-parameters$gamma
                                }
                                w <- extractw(net,state)
                                names(w) <- net$genes
                                # applying SQUAD
                                for (i in 1:length(net$genes)){
                                        newState.vect[i] <- SQUAD(x=state[i],
                                                                w=w[i],
                                                                gamma = gamma[i],
                                                                h=h[i])
                                }
                                names(newState.vect)<-net$genes

                                ## definition of mutants
                                if ( length(fixed) == 1 ) {
                                        if ( fixed != "default" ) {
                                                fixedGene <- names(fixed)
                                                newState.vect[fixedGene] <- 0
                                        }
                                }
                                if ( 1 < length(fixed) ) {
                                        fixedGenes <- names(fixed)
                                        newState.vect[fixedGenes] <- 0
                                }

                                return(list(newState.vect))
                        })
                }
                res$"squad" <- network

                # BoolNet to normHillCube
                        #if ( ! ( length(parameters) %in% c(1,3) ) ) {
                        #        stop("The 'parameters' argument most contain three numerical vectors or be set to 'default'. ")
                        #}
                        ## Setting parameters
                        if (length(parameters)==1) {
                                if (parameters=="default") {
                                        n <- rep(50,length(net$genes))
                                        k <- rep(0.5,length(net$genes))
                                        gamma <- rep(1,length(net$genes))
                                }
                        }

                        network.hc <- asNormHillCube(net,n=n,k=k,gamma = gamma)
                        res$"normHillCubes" <- network.hc

        }

        for (i in 1:length(res$genes)) {
                res$interactions[[i]]$"nonZeros" <- which(res$interactions[[i]]$func==1)
        }

        class(res) <- "squad"
        return(res)
}




