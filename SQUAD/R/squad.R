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
                  parameters="default",
                  timePeriod=7.5,
                  lengthInterval=0.01,
                  type = "squad",
                  fixed = "default",
                  perturbations = FALSE,
                  events = list(),
                  plot = "timeSerie",
                  indexes = "default",
                  atol = 10e-6,
                  rtol =10e-6,
                  ...) {

        if ( ! ( class(net) %in% c("BooleanNetwork","squad") ) ) {
                stop('A net object of class "BooleanNetwork" or "squad" most be provided!')
        }

        if ( ! ( type %in% c("squad","normHillCubes") ) ) {
                stop("type most be a character string in c('squad','normHillCubes')")
        }

        if ( length(parameters) == 1 && parameters != "default" ) {
                stop("Please provide a valid list of parameters values or set to 'default'")
        }

        if ( length(parameters) > 1 ) {
                if ( type == "normHillCubes" ) {
                        if ( ! ( length(parameters) == 3 ) ) {
                                stop("Parameters most be a list of three numeric vectors")
                        }
                        n <- parameters$n
                        k <- parameters$k
                        gamma <- parameters$gamma
                }
                if ( type == "squad" ) {
                        if ( ! ( length(parameters) == 2 ) ) {
                                stop("Paramters most be a list of two numeric vectors")
                        }
                        h <- parameters$h
                        gamma <- parameters$gamma
                }
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

        #initialState <- unname(initialState)
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
                                     atol=atol,
                                     rtol=rtol,...)
                        }
                        if ( perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$squad,
                                     parms = parameters,
                                     fixed = fixed,
                                     events = list(data=specs),
                                     atol=atol,
                                     rtol=rtol,...)
                        }

                }
                if ( type == "normHillCubes" ) {
                        if ( ! perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$normHillCubes,
                                     fixed = fixed,
                                     parms = parameters,
                                     atol = atol,
                                     rtol = rtol,...)
                        }
                        if ( perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$normHillCubes,
                                     fixed = fixed,
                                     parms = parameters,
                                     events = list(data=specs),
                                     atol = atol,
                                     rtol = rtol,...)
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
                                     atol = atol,
                                     rtol = rtol,
                                     ...)
                        }
                        if ( perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net$squad,
                                     parms = parameters,
                                     fixed = fixed,
                                     events = list(data=specs),
                                     atol = atol,
                                     rtol = rtol,
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
                                     atol = atol,
                                     rtol = rtol,...)
                        }
                        if ( perturbations ) {
                                dynamic<-ode(y=initialState,
                                     times=times,
                                     func=net.sq$normHillCubes,
                                     parms = parameters,
                                     fixed = fixed,
                                     events = list(data=specs),
                                     atol = atol,
                                     rtol = rtol,...)
                        }
                }
        }
        colnames(dynamic) <- c("time",net$genes)

        if ( plot == "raw") {
                return(dynamic)
        }
        if ( plot == "timeSerie" ) {
                timeSerie.sq(dynamic,indexes = indexes)
        }
        if ( plot == "heatmap" ) {
                heatmap.sq(dynamic,indexes = indexes)
        }

}




decimalToBinary<-function(value,positions){
        ## note that the binary vector is in inverse order
        val = value
        output <- rep(0,positions)
        counter <- 1
        while(val>0){
                output[[counter]]<-val%%2
                val <- val %/% 2
                counter <- counter+1
        }
        output
}




####################################################################################################################################

# Takes x or 1-x values according to conjunctive forms
# of the variables
eval.input<-function(plantilla,state){
        new.state<-rep(0,length(plantilla) )
        k <- length(new.state)
        ## here the order is inverted because
        ## plantilla is result of decimalToBinary function
        ## See, decimalToBinary comments.
        for ( i in 1:k ){
                if ( plantilla[k-i+1] == 0 ) {
                        new.state[i] <- 1-state[i]
                } else if ( plantilla[k-i+1] == 1 ) {
                        new.state[i] <- state[i]
                } else {
                        print("Error: Non Boolean value")
                }
        }
        return(new.state)
}


####################################################################################################################################

# Record the minimum value of the conjunctive forms and stores them
# in a vector
defineDisjunction<-function(net,state,nodeIndex){
        disjunction<-c()
        nodeFunction<-net$interactions[[nodeIndex]][[2]]
        regulatorsIndexes<-net$interactions[[nodeIndex]][[1]]
        numberOfRegulators<-length(regulatorsIndexes)
        ## nonZeros <-length(nodeFunction) - which(nodeFunction==1)
        ## for i in length(nonZeros):1
        if ( class(net) == "BooleanNetwork" ) {
                nonZeros <- which( nodeFunction == 1 )
        } else if ( class(net) == "squad" ) {
                nonZeros <- net$interactions[[nodeIndex]]$nonZeros
        }

        for (i in nonZeros) {
                conjunction<-decimalToBinary(i-1,numberOfRegulators)
                ## modificar aqui para quitar rev
                ##conjunction<-rev(conjunction)
                newInput<-eval.input(plantilla = conjunction,state[regulatorsIndexes])
                value<-min(newInput)
                disjunction<-c(disjunction,value)
        }
        ## modifcar para eliminar new.input y tomar solo non zero indexes
        return(disjunction)
}




####################################################################################################################################

# extractw(net,state)
# state is a vector of n real valued entries, n is the number of nodes of the network.
# extract() applies the Boolean functions to state but in a generallized manner using
# fuzzy logic operators instead of Boolean operators.
# extract() algorithm:
# 1. Create a vector x of length n called w
# 2. Traverse node Boolean functions in net object
#  2.1. Define a empty vector called disjunction
#  2.2. Traverse the Boolean function (represented as a vector of the Boolean outputs
#       assigned to conjunctions in the normal disyuntive form)
#    2.2.1. If the value of the conjunction is 1, append the min({x_i'}) to disjunction.
#           Where x_i' are the values of the node i (let call x_i) taken as x_i or 1 - x_i
#           according to the conjunctive proposition
#  2.3. Define w_i as max(disyunction)
# 3. Return w
extractw<-function(net,state){
        w<-rep(0,length(net$genes))
        for (i in 1:length(net$genes)){
                nodeFunction<-net$interactions[[i]][[2]]
                newInput<-defineDisjunction(net,state,i)
                #newInput<-new.input(disjunction,nodeFunction)
                w[i]<-max(newInput)
        }
        return(w)
}


##################################################################################################################################

# squad generic function
squadFun<-function(x,w,gamma,h){
        val <- ((-exp(0.5*h) + exp(-h*(w-0.5))) / ((1-exp(0.5*h)) * (1+exp(-h*(w-0.5))))) - (gamma*x)
        return(val)
}


#####################################################################################################################

