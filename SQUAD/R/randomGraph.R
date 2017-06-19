#' create a random conectivity matrix that can be used to test graphToModel() function
#' @description randomGraph() create a random conectivity matrix that can be used to test graphToModel() function
#' @export randomMatrix 
#' @param matrixDimension the number of rows (=columns) in the square matrix of connectivity
#' @param lamda a numeric positive value which represent the lamda parameter of the exponential distribution
#' for the number of outputs of each node.
#' @usage randomMatrix(numeric,integer)
#' @examples
#' randomMatrix(100,3)
#' str(randomMatrix)


# Generates conectivity matrices that has poisson distribution for 
# inputs and exponential distribution for outputs.
randomMatrix<-function(matrixDimension=20,lamda=3){
        A<-matrix(0,matrixDimension,matrixDimension)
        nodeNames<-paste("node",1:matrixDimension,sep="")
        for (i in 1:length(A[,1]) ){
                x<-runif(1,min = 0,max = 1)
                e<-exponentialFunction(x,lamda = lamda)
                n<-floor(e)
                regulators<-sample(1:matrixDimension,n)
                for (j in regulators ){
                        A[i,j]<-sample(c(-1,1),1)
                }
        }
        for (k in 1:matrixDimension){
                x<-rpois(1,lambda = 0.1)
                n<-1+x
                regulators<-sample(1:matrixDimension,n)
                for (r in regulators){
                        A[r,k]<-sample(c(-1,1),1)
                }
        }
        rownames(A)<-nodeNames
        colnames(A)<-nodeNames
        return(A)
}


# exponential functions for outputs
exponentialFunction<-function(x,lamda){
  return(lamda*exp(-lamda*x))
}

# poisson distribution for inputs
poissonFunction<-function(k,lamda=2){
  return( ( exp(-lamda)*( lamda**k ) ) / factorial(k) )
}




# auxiliary function that returns a random conectivity matrix with entries values -1, 0 or 1
randomMatrix.h<-function(matrixDimension=20){
  nodeNames<-paste("node",1:matrixDimension,"")
  A<-matrix(0,matrixDimension,matrixDimension)
  for (i in 1:length(A[,1])){
    for (j in 1:length(A[1,])){
      A[i,j]<-sample((-1):1,1)
    }
  }
  rownames(A)<-nodeNames
  colnames(A)<-nodeNames
  return(A)
}

#A<-randomMatrix.e(matrixDimension = 30,lamda = 3)

# plot conectivity matrix (as defined above)
plot.graph<-function(A){ 
  for (i in 1:length(A[,1]) ) {
    for (j in 1:length(A[1,])){
      if (A[i,j]!=0){
        A[i,j]<-1
      }
    }
  }
  trans.graph<-graph.adjacency(A)
  tkplot(trans.graph)
}