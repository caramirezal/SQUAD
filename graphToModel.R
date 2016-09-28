

# graphToModel() convert a matrix of conectivity to a Boolean regulatory network in BoolNet format.
# Algorithm:
# Let A be the matrix of conectivity where {a_ij} = 1 (-1) iff the node i is positive (negative) regulator
# and 0 if there is no interaction.
# let positives (negatives) be the boolean input of the positive (negative) regulators. 
# define b = min ( max(positives), 1 - max(negatives)  )
# That is, it is assumed that the node j is going to be active iff at leat one of the positive regulators
# is active and none of the negative regulators are active. 
# let k be the number of regulators and f be the boolean function for the node i to be defined. 
# f is a vector of boolean values of length 2**k that maps 
# every boolean vector input of length k (ordered according to node indexes) to a boolean value.
# 1. Define an object of the class net BoolNet format.
# 2. Traverse the column of A[,j] and extract positive and negative regulators.
# 3. Define f[i] as b[x[i]] where x is the boolean vector representation of decimal number i from 1 to
#       2**k.
# 4. Add the node and its function to the net object.
# 5. Repeat for every column in A.

#########################################################################################################################
# Define a function that extract regulators of the node i
# return a list of the vectors of positives and negative regulators.
# Let A be the matrix of conectivity where {a_ij} = 1 (-1) iff the node i is positive (negative) regulator
# and 0 if there is no interaction.

# Algorithm: traverse the i-th column of A and extract regulators
# return a list with two vectors of positive and negative regulators (in order)
getRegulators<-function(matrix,nodeIndex){
  positive.r<-c()
  negative.r<-c()
  for ( i in 1:length(matrix[,nodeIndex]) ){
    #nm<-rownames(matrix)[i]
    if ( matrix[i,nodeIndex] == 1 ) {
      positive.r<-c(positive.r,i )
    }
    if ( matrix[i,nodeIndex] == -1  ) {
      negative.r<-c(negative.r,i)
    }
  }
  if (length(positive.r)>0){
    names(positive.r)<-rownames(matrix)[positive.r]
  }
  if (length(negative.r)>0){  
    names(negative.r)<-rownames(matrix)[negative.r]
  }
  return(list(positive.r,negative.r))
}

# test getRegulators()
testGetRegulators<-function(){ 
  A<-matrix(0,10,10)
  for (i in 1:length(A[,1])){
    for (j in 1:length(A[1,])){
      A[i,j]<-sample((-1):1,1)
    }
  }
  rownames(A)<-letters[1:10]
  colnames(A)<-letters[1:10]
  A
  for (i in 1:length(A[1,])){
    cat(row.names(A)[i],"\n")
    l<-getRegulators(A,i)
    # Note that lists returned by getRegulators() can be empty list
    if (length(l[[1]])>0){
      cat("positive regulators: \n")
      cat(l[[1]],"\n")
    }
    if (length(l[[2]])>0){
      cat("negative regulators: \n")
      cat(l[[2]],"\n")
    }
  }
}

#testGetRegulators()

########################################################################################################################

# Auxiliary function to test the order in which f is defined in boolNet format
trueTableTest<-function(net){ 
  for (i in 1:length(net$genes)) {
    nmOfNodes<-length(net$genes)
    cat("Node: ",net$genes[i],"\n")
    nodeFunction<-net$interactions[[i]][[2]]
    #cat("1: ",net$interactions[[i]][[1]],"\n")
    nmOfRegulators<-length(net$interactions[[i]][[1]])
    #cat(nodeFunction,"\n")
    cat(net$interactions[[i]][[3]],"\n")
    cat("rev(decToBin[x_i])         f","\n")
    for (j in 1:length(nodeFunction)){
      input<-decimalToBinary(j-1,nmOfRegulators)
      cat("     ",rev(input),"             ")
      cat(nodeFunction[j],"\n")
    }
    cat("\n")
  }
}

#trueTableTest(net)

# Define the boolean function f according to the type of regulators of the node
# f = min( max(positives), 1 - max(negatives) )
# as explained above.
getBooleanFunction<-function(matrix,nodeIndex){
  regulators<-getRegulators(matrix,nodeIndex)
  positive.r<-regulators[[1]]
  negative.r<-regulators[[2]]
  all.r<-c(regulators[[1]],regulators[[2]])
  numberOfRegulators<-length(all.r)
  if (numberOfRegulators>0) {
    # function is not well defined if there is no regulators of the node nodeIndex-th
    names.r<-names(all.r)
    names.r<-sort(names.r)
    f<-vector(mode = "numeric",2**numberOfRegulators)
    for (i in 1:length(f)){
      input<-decimalToBinary(i-1,numberOfRegulators)
      # input most be reversed
      input<-rev(input)
      names(input)<-names.r
      if ( ( length(positive.r)>0 ) & (length(negative.r) >0) ){ 
        f[i]<-min( max( input[names(positive.r)] )  ,1 - max( input[names(negative.r)] ) )
      }
      if ( ( length(positive.r) > 0 ) & ( length(negative.r) == 0 ) ) {
        f[i]<-max( input[names(positive.r)] )  
      }
      if ( ( length(positive.r) == 0 ) & ( length(negative.r) > 0 ) ) {
        f[i]<- 1 - max( input[names(negative.r)] )  
      }
      #cat(input[names(positive.r)]," ",input[names(negative.r)] ," -> ",f[i],"\n")
    }
    #cat("node: ",nodeIndex,"\n")
    #cat(all.r,"\n")
  }
  if (length(all.r)==0) {
    #cat(regulators[[1]],"--",regulators[[2]],"\n")
    cat("Node ",nodeIndex, "has no regulators and f can not be defined.\n")
    cat("An input was assumed.\n")
    f<-c(0,1)
  }
  return(f)
}



##########################################################################################################################

# Transform a static representation of a regulatory network
# to a dynamic Boolean discrete model
graphToModel<-function(conectivityMatrix){
  # define the net list object 
  interactions<-list()
  genes<-list()
  fixed<-list()
  network<-list(interactions,genes,fixed)
  class(network)<-"BooleanNetwork"
  nameList<-c("interactions","genes","fixed")
  names(network)<-nameList
  for (i in 1:length(conectivityMatrix[1,])){
    reg<-getRegulators(conectivityMatrix,i)
    input<-c(reg[[1]],reg[[2]])
    vector<-rep(0,length(input))
    for (j in 1:length(input)){
      vector[j]<-input[j]
    }
    fun<-getBooleanFunction(conectivityMatrix,i)
    expression<-" min( max(positive), 1 - max(negative) ) "
    network$interactions[[i]]<-pairlist(sort(vector),fun,expression)
    names(network$interactions[[i]])<-c("input","fun","expression")
  }
  names(network$interactions)<-rownames(conectivityMatrix)
  #network$genes[[1]]<-colnames(netMatrix)
  network[[2]]<-colnames(conectivityMatrix)
  fixed<-rep(-1,length(conectivityMatrix[1,]) )
  names(fixed)<-rownames(conectivityMatrix)
  network[[3]]<-fixed
  return(network)
}

