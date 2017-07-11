
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
SQUAD<-function(x,w,gamma,h){
        val <- ((-exp(0.5*h) + exp(-h*(w-0.5))) / ((1-exp(0.5*h)) * (1+exp(-h*(w-0.5))))) - (gamma*x)
        return(val)
}


#####################################################################################################################

