## March 28, 2017
## Mexico City
## Written by Carlos

library(deSolve)

## define a function input for ode solver importing w parameters of SQUAD format
loadNetworkSQUAD <- function(file,fixed="default"){
        rules <- read.csv(file,header = T,colClasses = "character",sep = ";")
        if ( ! ( ("targets" == colnames(rules)[1]) && ("factors" == colnames(rules)[2]) ) ) {
                stop("Please, provide a valid SQUAD format")
        }
        rules
}


####################################################################################################

# squad generic function
SQUAD<-function(x,w,gamma,h){
        val <- ((-exp(0.5*h) + exp(-h*(w-0.5))) / ((1-exp(0.5*h)) * (1+exp(-h*(w-0.5))))) - (gamma*x)
        return(val)
}

###################################################################################################


## parse w equations into a deSolve function
wToSQUAD <- function(genes,wExpressions){

       squadODEs <- function(times,state,parameters){
                with(as.list(c(state,parameters)), {
                        for (i in 1:length(net$targets)) {
                                assign(net$targets[i],state[i])
                        }
                        w <- sapply(net$factors, function(x) eval(parse(text=x)))
                        evalSQUAD <- sapply(1:length(net$targets),
                                            function(i) SQUAD(x = state[i], w= w[i],
                                                              gamma = 1, h = 50))
                        names(evalSQUAD) <- net$targets
                        return(list(evalSQUAD))
                })
       }
        squadODEs
}

#set.seed(1000)
net <- loadNetworkSQUAD("cartoonNetworkSQUAD.R")
state <- runif(length(net$targets))
squadInteractions <- wToSQUAD(net$targets,net$factors)
times <- seq(0,50,by=0.5)
parameters <- list()
result<-ode(y=state,times=times,func=squadInteractions,parms = parameters,atol=10e-6, rtol=10e-6)