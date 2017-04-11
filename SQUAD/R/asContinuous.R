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
#' simulations <- ode(y=state, fun=cellCycle.s\$fun, times=times,
#'                    parameters = "default") 
#'> \# note that cellCycle\$fun can be given as the function
#'> \# parameter to ode solver method 
#'> simulations
#' @return an allready to use function to pass as argument to ODE() fun parameter, or to use
#' in combination with squad()
#' @name asContinuous
#' @title asContinuous

asContinuous <- function(net) {
        if ( class(net) != "BooleanNetwork") {
                stop("A net object of class BooleanNetwork most be provided!")
        } 
        network <- function(times,state,parameters) {
                with(as.list(c(state,parameters)),{
                        newState.vect<-rep(0,length(net$genes))
                        # Definition of parameters h, w and gamma.
                        if (length(parameters)==1) { 
                                if (parameters=="default") {
                                        gamma <- rep(1,length(net$genes))
                                        h <- rep(50,length(net$genes))
                                }
                        }
                        if ( length(parameters) > 1 ) {
                                h<-parameters[[1]]
                                gamma<-parameters[[2]]
                        }
                        w <- extractw(net,state)
                        names(w) <- net$genes
                        # applying SQUAD
                        for (i in 1:length(net$genes)){
                                newState.vect[i]<-SQUAD(x=state[i],w=w[i],gamma = gamma[i],h=h[i])
                        }
                        names(newState.vect)<-net$genes
                        return(list(newState.vect))
                })
        }
        
        res <- list("genes" = net$genes, "fun"=network,
                    "fixed" = net$fixed)
        
        class(res) <- "SQUAD"
        
        res
}



decimalToBinary<-function(value,positions){
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
        for (i in 1:length(new.state) ){
                if ( plantilla[i] == 0 ) { 
                        new.state[i] <- 1-state[i]
                } else if ( plantilla[i] == 1 ) {
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
        for (i in 1:length(nodeFunction)){
                conjunction<-decimalToBinary(i-1,numberOfRegulators)
                conjunction<-rev(conjunction)
                newInput<-eval.input(plantilla = conjunction,state[regulatorsIndexes])
                value<-min(newInput)
                disjunction<-c(disjunction,value)
        }
        
        return(disjunction)
}



#####################################################################################################################################

# vectorial product of the new continuous values
# with the binary boolean function
new.input<-function(disjunction,nodeFunction){
        new.input<-rep(0,length(disjunction))
        for (i in 1:length(disjunction)){
                new.input[i]<-disjunction[i] * nodeFunction[i]
        }
        return(new.input)
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
                disjunction<-defineDisjunction(net,state,i)
                newInput<-new.input(disjunction,nodeFunction)
                w[i]<-max(newInput)
        }
        return(w)
}


##################################################################################################################################

# squad generic function
SQUAD<-function(x,w,gamma,h){
        val <- ((-exp(0.5*h) + exp(-h*(w-0.5))) / ((1-exp(0.5*h)) * (1+exp(-h*(w-0.5))))) - (gamma*x)
        return(val)
}


#####################################################################################################################



