#' load a regulatory network model in squad format
#'
#' @description Load a regulatory network model in squad format. It returns a squad object that
#' can be used to perform continuous simulations using interpolations of Boolean Regulatory
#' network models
#' @name loadNetwork.sq
#' @export loadNetwork.sq
#' @param fixed a vector of values in the range [0,1] to node values to be fixed.
#'
#' @examples
#' net.sq <- loadNetwork.sq("cartoonNetworkSQUAD.txt")
#' sim <- squad(net.sq)
#' timeSerie.sq(sim)


## squad generic function
SQUAD<-function(x,w,gamma,h){
        val <- ((-exp(0.5*h) + exp(-h*(w-0.5))) / ((1-exp(0.5*h)) * (1+exp(-h*(w-0.5))))) - (gamma*x)
        return(val)
}


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


## get the regulators of the nodes by checking if the names of the nodes
## are in the string of the corresponding ODE continuous function
getRegulators.sq <- function(net.df) {
        res <- c()

        for (i in 1:length(net.df$targets)) {
                regulators <- net.df$factors[i]
                #for (j in 1:length(net.df$targets)) {
                res[[i]] <- regulators
                #}
        }
        res
}



## defines a function input for ode solver importing w parameters of SQUAD format
loadNetwork.sq <- function(file,fixed="default"){

        firstLine <- readLines(file,1)

        if ( ! ( grepl("targets",firstLine) && grepl("factors",firstLine) ) ) {

                stop("Please, provide a valid BoolNet/SQUAD format")
        }

        fileLines <- readLines(file)

        leftSideEq <- sub(",.*","",fileLines)

        rigthSideEq <- sub("*.,","",fileLines)

        ## construct a df called net
        net <- list("targets"=leftSideEq[2:length(leftSideEq)],
                    "factors"=rigthSideEq[2:length(rigthSideEq)])

        squadODEs <- wToSQUAD(net$targets,net$factors,fixed = fixed)

        if (length(fixed)==1){

                if (fixed=="default"){

                        fixedGenesVal <- rep(-1, length(net$targets))

                        names(fixedGenesVal)<-net$targets
                }

        }

        if ( ! (length(fixed)<=length(net$factors)) ) {

                stop("Error: More fixed genes than the actual gene nodes number!")
        }

        fixedGenesVal <- rep(-1, length(net$targets))

        names(fixedGenesVal) <- net$targets

        for (i in names(fixed)) {

                fixedGenesVal[i] <- fixed[i]
        }

        interactions <- list()

        for (i in 1:length(net$targets)) {

                input <- getInputs(net$targets,net$factors[i])

                func <- vectRepresentation(net$targets,
                                           input,
                                           net$factors[i])

                expression <- net$factors[i]

                interactions[[i]] <- list("input"=input,
                                          "func"=func,
                                          "expression"=expression)

        }
        names(interactions) <- net$targets

        net.sq <- list("genes"=net$targets,
                       "interactions"=interactions,
                       "fixed"=fixedGenesVal,
                       "squad"=squadODEs)

        # definition of boolNet object
        net.boolNet <-  net.sq[c("genes","interactions","fixed")]
        class(net.boolNet) <- "BooleanNetwork"

        ## getting normalized Hill Cubes continuous interpolations
        net.continuous <- asContinuous(net.boolNet)
        net.sq$"normHillCubes" <- net.continuous$normHillCubes

        class(net.sq) <- "squad"
        net.sq
}



## getInputs() obtains the indexes of input regulators already present in a boolean expression string
## The definition of nodeRegExp can be better understood taking into account the next expression
## grep("^a[[:punct:]| ]|[[:punct:]| ]a[[:punct:]| ]|[ |[:punct:]]a$",c("aa ","aaa"," aa ","&a&", " a "))
## which finds the name of a node in a boolean expression.
getInputs <- function(nodeNames,boolExpression){

        if ( class(nodeNames) != "character") {

                stop("net most be an object of class BoolNet or SQUAD")

        }

        if ( class(boolExpression) != "character" ) {

                stop("boolExpresson argument most be a string")
        }

        res <- c()

        for (i in 1:length(nodeNames)) {

                nodeRegExp <- paste("^",nodeNames[i],"[[:punct:]| ]|[[:punct:]| ]",
                                nodeNames[i],"[[:punct:]| ]|[ |[:punct:]]",nodeNames[i],"$",sep="")


                NodeFound <- grep(nodeRegExp,boolExpression)

                if ( 0 < length(NodeFound) ) {

                        res <- c(res,i)

                }

        }

        res

}


## vectRepresentation() transforms a boolean function in String representation to a binary
## vector representation
## Takes three arguments. The argument nodeNames is the character vector of node names of the network
## inputs is a numeric or integer vector with the indexes of the regulators of a node with that function
## boolExpression is the boolean function of a node in String format.
vectRepresentation <- function(nodeNames,inputs,boolExpression) {

        if ( class(nodeNames) != "character" ) {

                stop("nodeNames argument most be a character vector")

        }

        if ( ! ( class(inputs) %in% c("numeric","integer") ) ) {

                stop("inputs argument most be a numeric vector")

        }

        if ( class(boolExpression) != "character" ) {

                stop("boolExpression argument most be a String Boolean function")

        }

        res <- numeric(2**length(inputs))

        for ( i in 1:length(res) ) {

                ## decimalToBinary most be used carefully since the values are returned in reverse order
                state <- decimalToBinary(i-1,length(inputs))

                k <- length(inputs)

                for (j in 1:k) {

                        assign(nodeNames[inputs[j]],state[k-j+1])

                }

                res[i] <- eval(parse(text = boolExpression))

        }

        res

}

