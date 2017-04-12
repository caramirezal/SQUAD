#' getAttractors calculates all asynchronous attractors of a Boolean Regulatory Network model
#' @param 
#' @export

getAttractorsAsynchronous <- function(net) {
        ## checks arguments
        if ( class(net) != "BooleanNetwork" ) {
                stop("A net object of class BooleanNetwork most be provided!")
        }
        
        ## export net in boolnet to sbml format stored in the workspace file
        toSBML(network = net,file = "regulatoryNetworkModel.sbml")
        
        ## calling the SSSearcher java script 
        searcher <- .jnew("SSSearcher")
        res <- .jcall(searcher,"[[B","getAttractors")
        attractors <- list()
        #as.numeric(.jevalArray(res[[1]]))
        
        for (i in 1:length(res)) {
                # cat(as.numeric(.jevalArray(res[[i]])),"\n")
                attractors[[i]] <- as.numeric(.jevalArray(res[[i]]))
        }
        
        
        attractors <- as.data.frame(attractors)
        attNames <- paste("Attr.",1:length(attractors),sep = "")
        colnames(attractors) <- attNames
        #rownames(attractors) <- net$genes
        
        attractors
}