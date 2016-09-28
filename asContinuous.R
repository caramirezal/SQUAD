# asContinuous() transform a boolean regulatory network model in BooleanNet format to 
# a continuous model object that can be supplied to ode() function from deSolve R package


# transform decimal number to a binary representation (binary vector)
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


# Algorithm:
# extract w vector of parameters w_i through the application of fuzzy logic functions
# according to the Boolean functions of the network.
# Return f(w). Where f is the generic SQUAD function (see SQUAD() function in this script
# for the definition).


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


testEvalInput<-function(){
  input<-rep(0,5)
  for (i in 1:length(input)){
    input[i]<-sample(c(0,1),1)
  }
  cat("Plantilla:",input,"\n")
  inputNode<-rep(0,5)
  for  (j in 1:length(inputNode)) {
    inputNode[j]<-sample(c(0,1),1)
  }
  cat("State: ",inputNode,"\n")
  cat("new.state: ",eval.input(input,inputNode),"\n")
}



####################################################################################################################################

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


testDefineDisjunction<-function(net,state){
  for (i in 1:length(net$genes)){
    cat(net$genes[i],"\n")
    cat(defineDisjunction(net,state,i),"\n")
  }  
}



#####################################################################################################################################


new.input<-function(disjunction,nodeFunction){
  new.input<-rep(0,length(disjunction))
  for (i in 1:length(disjunction)){
    new.input[i]<-disjunction[i] * nodeFunction[i]
  }
  return(new.input)
}


testNewInput<-function(net,state){
  for (i in 1:length(net$genes)){
    nodeFunction<-net$interactions[[i]][[2]]
    disjunction<-defineDisjunction(net,att.1,i)
    cat(net$genes[i],"\n")
    cat("Disjunction",disjunction,"\n")
    cat("Node function: ",nodeFunction,"\n")
    cat("New input: ",new.input(disjunction,nodeFunction),"\n\n")
  }
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
    #cat(net$genes[i], ": ",newInput,"\n")
    w[i]<-max(newInput)
  }
  return(w)
}



testExtractw<-function(net,attractors){
  # calculate attractors of the Boolean network model
  # extract fixed point attractors in a matrix
  att.Matrix<-plotClusteredAttractors(net,attractors,fixedPointsOnly = TRUE)
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


##################################################################################################################################

# squad generic function
SQUAD<-function(x,w,gamma,h){
  val<- ((-exp(0.5*h) + exp(-h*(w-0.5))) / ((1-exp(0.5*h)) * (1+exp(-h*(w-0.5))))) - (gamma*x)
  return(val)
}



#####################################################################################################################



asContinuous<-function(net){
  # parameters will be given as a list = {h,gamma}
  # function most take parameters
  # if parameters are default you can use the next function
  network<-function(times,state,parameters=parameters) {
    with(as.list(c(state,parameters)),{
      geneNames<-net$genes
      #names(state)<-geneNames
      newState.vect<-rep(0,length(geneNames))
      # Definition of parameters h, w and gamma.
      if (length(parameters)==1) { 
        if (parameters=="default") {
          gamma<-rep(1,length(net$genes))
          #gammaNames<-paste("gamma",1:length(geneNames),sep = "")
          #names(gamma)<-gammaNames
          h<-rep(50,length(net$genes))
          #hNames<-paste("h",1:length(geneNames),sep = "")
          #names(h)<-hNames
        }
      }
      if ( length(parameters) > 1 ) {
        h<-parameters[[1]]
        gamma<-parameters[[2]]
      }
      w<-extractw(net,state)
      names(w)<-geneNames
      # applying SQUAD
      for (i in 1:length(geneNames)){
        newState.vect[i]<-SQUAD(x=state[i],w=w[i],gamma = gamma[i],h=h[i])
      }
      names(newState.vect)<-geneNames
      return(list(newState.vect))
    })
  }
  # else you most use other function
  return(network)
}


testAsContinuous<-function(){ 
  for (i in 1:30){ 
    conectivityNet<-randomMatrix.e()
    net<-graphToModel(conectivityNet)
    gamma<-rep(1.2,length(net$genes))
    h<-rep(10,length(net$genes))
    parameters<-list(h,gamma)
    state<-runif(length(net$genes),0,1)
    times<-seq(0,1,length.out = 30)
    res1<-ode(y=state,times=times,func = asContinuous(net),parms=parameters)
    heatmap(res1,Rowv = NA, Colv = NA)
  }
}  

