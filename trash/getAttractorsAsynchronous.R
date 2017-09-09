## March 26, 2017. Mexico City
## Written by Carlos Ramirez

#library(BoolNet)

# getAttractors() performs the asynchronous stable states search by calling GINSIM
getAttractorsAsynchronous <- function(net){
        ## checks arguments
        if ( class(net) != "BooleanNetwork" ) {
                stop("A net object of class BooleanNetwork most be provided!")
        }
        
        ## export net in boolnet to sbml format
        toSBML(network = net,file = "regulatoryNetworkModel.sbml")
        
        ## execute java method
        system( 'javac -cp ".:GINsim-2.9.4-with-deps.jar" GetAttractors.java' )
        system( 'java -cp ".:GINsim-2.9.4-with-deps.jar" GetAttractors > "regulatoryNetworkModelRes.tmpl"' )

        ## import the result from the temporal file
        attractors <- read.csv("regulatoryNetworkModelRes.tmpl", header = F)
        colnames(attractors) <- net$genes
        attNames <- paste("Attr.",1:nrow(attractors),sep = "")
        rownames(attractors) <- attNames
        attractors
}


#getAttractorsAsynchronous(net)


