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
#' @export getAttractorsAsynchronous

getAttractorsAsynchronous <- function(net,returnPlot=FALSE) {
        ## checks arguments
        if ( class(net) != "BooleanNetwork" ) {
                stop("A net object of class BooleanNetwork most be provided!")
        }
        
        if (file.exists("regulatoryNetworkModel.sbml")) {
                file.remove("regulatoryNetworkModel.sbml")
        }
        
        ## export net in boolnet to sbml format stored in the workspace file
        toSBML(network = net,file = "regulatoryNetworkModel.sbml")
        
        ## calling the SSSearcher java script 
        searcher <- .jnew("SSSearcher")
        res <- .jcall(searcher,"[[I","getAttractors")
        
        file.remove("regulatoryNetworkModel.sbml")
        
        attractors <- list()
        #as.numeric(.jevalArray(res[[1]]))
        
        if ( length(res) > 0 ) {
                for (i in 1:length(res)) {
                        # cat(as.numeric(.jevalArray(res[[i]])),"\n")
                        attractors[[i]] <- as.numeric(.jevalArray(res[[i]]))
                }
                
                attractors <- as.data.frame(attractors)
                attNames <- paste("Attr.",1:length(attractors),sep = "")
                colnames(attractors) <- attNames
                rownames(attractors) <- net$genes
                

        }

        if ( length(res) <= 0 ) {
                
                print("No fixed points found!")
        }
        
        if ( returnPlot == TRUE ) {
                
                plotAttractors.sq(attractors)
                
        }
                
        return(attractors)

}


#####################################################################################################

## Plotting function
plotAttractors.sq<-function(data,rowOrder=FALSE,rowIndexes,
                           colOrder=FALSE,colIndexes){
        
        data.u <- as.matrix(data)
        
        for ( i in 1:length(data.u[,1])) {
                        
                for (j in 1:length(data.u[1,])) {
                        
                        if ( data.u[i,j] == -1 ) {
                                
                                data.u[i,j] <- 0.5
                                
                        }
                }
                
        }
        
        colnames(data.u) <- make.unique(colnames(data))
        rownames(data.u) <- make.unique(rownames(data))
        
        if ( colOrder == TRUE ) {
                
                data.u <- data.u[,rev(colIndexes)]
                
        }
        
        if ( rowOrder == TRUE ) {
                
                data.u <- data.u[rowIndexes,]
                
        }
        
        data.m<-melt(data.u)
        data.m$X2<-factor(data.m$X2,
                          levels = unique(data.m$X2),
                          ordered = TRUE)
        
        data.m$X1<-factor(data.m$X1,
                          levels = unique(data.m$X1),
                          ordered = TRUE)
        
        gg<-ggplot(data = data.m, aes(x=X1, y=X2, colours=value,fill=value )) 
        gg<-gg + geom_tile(color="white",size=0.1)  
        gg<-gg + scale_fill_gradient(low = "black", high = "gray", limit = c(0,1))
        gg<-gg + theme(text=element_text(size = 16),axis.text.x=element_text(angle = 90,vjust = 0.4,colour = "black",face = "bold",size = 15))
        gg<-gg + labs(x="",y="")
        gg<-gg + coord_equal()
        
        plot(gg)
}