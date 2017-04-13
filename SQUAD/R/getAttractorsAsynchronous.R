#' getAttractors calculates all asynchronous attractors of a Boolean Regulatory Network model
#' @title 
#' Calculations of asynchronous steady state attractors of a Boolean Network
#' @description 
#' Finds all asynchronous stationary states of a Boolean Regulatory Network model. 
#' Takes as arguments an object of the "BoolNet" (or "SQUAD" under construction) and returns a 
#' data frame which contains the attractors of the model. 
#' @details 
#' The function is straightforward. It calls the ginsim library written by Naldi and 
#' colleagues, 2009 \cite{naldi2009logical}. 
#' @param net an object of class "BooleanNetwork" as that defined by the loadNetwork() function
#' of BoolNet R package
#' @examples 
#' data(cellCycle) 
#' attractors <- getAttractorsAsynchronous(cellCycle)
#' attractors
#' @return a data frame with numerical binary values of the attractors
#' @usage 
#' getAttractorsAsynchronous(net)
#' @name getAttractorsAsynchronous
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
        rownames(attractors) <- net$genes
        
        attractors
}