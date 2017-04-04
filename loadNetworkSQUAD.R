## March 28, 2017
## Mexico City
## Written by Carlos

#library(deSolve)




####################################################################################################

# squad generic function
SQUAD<-function(x,w,gamma,h){
        val <- ((-exp(0.5*h) + exp(-h*(w-0.5))) / ((1-exp(0.5*h)) * (1+exp(-h*(w-0.5))))) - (gamma*x)
        return(val)
}

###################################################################################################


## parse w equations into a deSolve function
wToSQUAD <- function(genes,wExpressions,fixed="default"){

       squadODEs <- function(times,state,parameters){
                with(as.list(c(state,parameters)), {
                        for (i in 1:length(genes)) {
                                assign(genes[i],state[i])
                        }
                        w <- sapply(wExpressions, function(x) eval(parse(text=x)))
                        if (length(parameters)==1) {
                                if (parameters=="default") {
                                        evalSQUAD <- sapply(1:length(genes), function(i) SQUAD(x = state[i], w= w[i],
                                                                                               gamma = 1, h = 50))
                                }
                        } else {
                                evalSQUAD <- sapply(1:length(genes), function(i) SQUAD(x = state[i], w= w[i],
                                                                                       gamma = gamma[i], h = h[i]))
                        }
                        names(evalSQUAD) <- genes
                        if (length(fixed)==1) {
                                if (fixed != "default") {
                                        i <- names(fixed)
                                        evalSQUAD[i] <- 0
                                }
                        } else {
                                for (i in names(fixed)) {
                                        evalSQUAD[i] <- 0 
                                }
                        }
                        return(list(evalSQUAD))
                })
        }

       squadODEs        

}

#####################################################################################################

## get the regulators of the nodes by checking if the names of the nodes
## are in the string of the corresponding ODE continuous function
getRegulators.sq <- function(net.df) {
        res <- list()

        for (i in 1:length(net.df$targets)) {
                regulators <- net.df$factors[i]
                #for (j in 1:length(net.df$targets)) {
                res[[i]] <- regulators
                #}
        }
        res
}


######################################################################################################################################

## defines a function input for ode solver importing w parameters of SQUAD format
loadNetworkSQUAD <- function(file,fixed="default"){
        net <- read.csv(file,header = T,colClasses = "character",sep = ";")
        if ( ! ( ("targets" == colnames(net)[1]) && ("factors" == colnames(net)[2]) ) ) {
                stop("Please, provide a valid SQUAD format")
        }
        
        squadODEs <- wToSQUAD(net$targets,net$factors,fixed = fixed)
        
        if (length(fixed)==1){
                if (fixed=="default"){
                        fixedGenesVal <- rep(-1, length(net$targets))
                        names(fixedGenesVal)<-net$targets
                }
        } else {
                if ( ! (length(fixed)<=length(net$factors)) ) {
                        stop("Error: More fixed genes than the actual gene nodes number!")
                }
                fixedGenesVal <- rep(-1, length(net$targets))
                names(fixedGenesVal) <- net$targets
                for (i in names(fixed)) {
                        fixedGenesVal[i] <- fixed[[i]] 
                }
        }
        
        regulators <- getRegulators.sq(net.df = net)
        
        net.sq <- list("genes"=net$targets,"fun"=squadODEs,
             "fixed"=fixedGenesVal,"regulators"=regulators)
        
        class(net.sq) <- "SQUAD"
        
        net.sq
}


#net <- loadNetworkSQUAD(file = "cartoonNetworkSQUAD.R")