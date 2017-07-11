#' function for testing proposes
#' @name checkFun
#' @title test
#' @description function for testing proposes
#' @export checkFun
#' @param net a "SQUAD" or "BoolNet" object
#' @usage test.sq(net, parameters)
#' @example
#' test.sq()


checkFun <- function(net,attractors) {
        # calculate attractors of the Boolean network model
        # extract fixed point attractors in a matrix
        #att.Matrix<-plotClusteredAttractors(net,attractors,fixedPointsOnly = TRUE)
        att.Matrix <- attractors
        # fixed point attractors are invariant under extractw()
        for (i in 1:length(att.Matrix[1,])){
                st<-att.Matrix[,i]
                cat("att.Matrix[",i,"]: ",st,"\n")
                cat("statetransition:  ",stateTransition(net,st),"\n")
                cat("extractw:         ",extractw(net,st),"\n")
                if ( all(att.Matrix[,i] == extractw(net,att.Matrix[,i]) ) )
                        cat("Attractor",i,"is invariant under extractw(). Validation: OK. \n")
                else { print("Error")}
                cat("\n")
        }
}
