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

# defines string boolean expressions from binary vectors in BooleanNetwork object
makeExpression <- function(net){
        # expression contains expressions for all nodes
        expressions <- vector(mode = "character",length = length(net$genes))
        # traverse vector
        for (i in 1:length(net$genes)) {
                # getting inputs names
                inputs <- net$genes[ net$interactions[[i]][[1]] ]
                # disjunctive forms strings container
                disjunctiveForm <- c()
                # length of boolean vector function
                n <- length(net$interactions[[i]][[2]])
                for ( j in 1:n ) {
                        if ( net$interactions[[i]][[2]][j] == 1 ) {
                                conjunctiveForm <- decimalToBinary(j-1,length(inputs))
                                #cat("Index",j,"Conjunctive form: ",conjunctiveForm,"\n")
                                unaryNodes <- vector("character",length(inputs))
                                m <- length(inputs)
                                for (k in 1:m) {
                                        if ( conjunctiveForm[k] == 1 ) {
                                                unaryNodes[k] <- inputs[m-k+1]
                                        }
                                        if ( conjunctiveForm[k] == 0 ) {
                                                unaryNodes[k] <- paste("!", inputs[m-k+1])
                                        }
                                }
                                conjunctiveExp <- paste(rev(unaryNodes),collapse = " & ")
                                conjunctiveExp <- paste("(",conjunctiveExp,")")
                                disjunctiveForm <- c(disjunctiveForm,conjunctiveExp)
                                #cat("Disjunctive form: ",disjunctiveForm,"\n")
                        }
                        disjunctiveExp <- paste(disjunctiveForm, collapse = " | ")
                        expressions[i] <- disjunctiveExp
                }
        }
        return(expressions)

}

#library(SQUAD)
#net <- loadNetwork("~/scripts/RegulatoryNetworkGMPModel/regulatoryNetworkGMPModel.txt")


