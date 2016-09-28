# getTransitionsMatrix() simulate boolean network model under noisy updating regimen
# return a matrix of transitions between attractors caused by noise

# set the noise level for the program can use it
perturbationCoin<-function(noiseDegree){
  number<-runif(1,min=0,max=1)
  if (number<noiseDegree){
    coin<-TRUE
  }
  else { coin <- FALSE }
  return(coin)
}

testPerturbationCoin<-function(noiseLevel=0.41){
  noiseVal<-noiseLevel
  repetitions<-1000
  valuesRep<-rep(0,repetitions)
  for (i in 1:repetitions){
    valuesRep[i]<-perturbationCoin(noiseVal)
  }
  numbTRUES<-0
  for (i in valuesRep){
    if (i == TRUE) {
      numbTRUES<-numbTRUES+1
    }
  }
  cat("Noise Level: ",noiseLevel," Number of heads: ",numbTRUES/repetitions,"\n")
}

# testPerturbationCoin()

###########################################################################################################################

# Perturb a state
# Take an state, give it a random perturbation and update
noisyUpdate<- function(net,state,noiseDegree,type="synchronous" ){
  # first perturbs 
  for (i in 1:length(state)) {                
    coin<-perturbationCoin(noiseDegree)               
    if (coin == TRUE) {                             
      state[i]<-(state[i]+1)%%2               
    }                             
  }
  # then update perturbed state
  if (type =="synchronous"){
    return(stateTransition(net,state,type = "synchronous"))
  }
  if (type =="asynchronous"){
    return(stateTransition(net,state,type = "asynchronous"))
  }
}

# testNoisyUpdate(net)

###########################################################################

# identifies if an allready calculated attractor has been found and return
# attractor number of FALSE if no attractor is reached
findAttractor<- function(net,state,attractors){
  result<-FALSE
  for ( i in 1:length( attractors[2]$attractors ) ) {
    for ( j in 1:length( attractors[2]$attractors[[i]][[1]] ) ){
      attractor <- attractors[2]$attractors[[i]][[1]][[j]]
      decimalState<-bitsToInt(rev(state))
      attractorFound<- attractor == decimalState
      if ( attractorFound == TRUE ) break                                         
    }
    # cat(attractor,decimalState,i,j,"\n")
    if ( attractorFound == TRUE ) { result <- i}
    if ( attractorFound == TRUE ) break
  }
  return(result)
}

validateFindAttractor<-function(attractors){
  for ( i in 1:length( attractors[2]$attractors ) ) {
    for ( j in 1:length( attractors[2]$attractors[[i]][[1]] ) ){
      state<-attractors[2]$attractors[[i]][[1]][j]
      cat( "Attractor", i,": ",state, "fits attractor -> ",findAttractor(net,
                                decimalToBinary(state,length(net$genes)),
                                attractors),"\n" ) 
    }
  }
}

validateFindAttractor(attractors)

########################################################################

# Iterate n times untill some already calcuted attractor is found
# return new attractor and time necesary to reach it
randomWalk<-function(net,state,attractors,noiseDegree,numberOfIterations,
                     type="synchronous"){
  i<-1
  newState<-noisyUpdate(net,state,noiseDegree,type)
  attractorFound<-findAttractor(net,newState,attractors)
  while ( ( i < numberOfIterations ) & attractorFound==FALSE ) {
    newState<-noisyUpdate(net,newState,noiseDegree,type) 
    attractorFound <- findAttractor(net,newState,attractors)
    #print(c(attractorFound,i))
    i<-i+1
  }
  return(attractorFound)
}


testRandomWalk<-function() {
  numberOfIterations<-100
  noiseDegree<-0.03
  net<-loadNetwork("BRN_MODEL_V1.txt")
  attractors<-getAttractors(net)
  state<-randomState(net)
  for ( i in 1:1000) {
    attractorFound<-randomWalk(net,state,attractors,noiseDegree,numberOfIterations)
    cat(attractorFound,"\n")
  }
}

testRandomWalk()

############################################################################

# Repeat previous funtions n times and return frecuency of
# transition events ( and/or mean time to reach it )

#numberOfWalks<-30
#noiseDegree<-10  # settable
#state<-generateState(net,specs=c("cmyb"=1,"cjun"=1))
#numberOfIterations<-200

# take an an state (attractors), simulate a random walk and return frecuency
# vector of transitions to other attractors
transitionsFrecuencies<-function(net,state,attractors,noiseDegree,numberOfIterations,numberOfWalks){
  transitions<-vector(mode="numeric",length(attractors$attractors))
  for (i in 1:numberOfWalks){
    walkResult<-randomWalk(net,state,attractors,noiseDegree,numberOfIterations)
    attractorFound<-walkResult[1]
    #attractorNumber<-walkResult[2]  # Modify for a second version   
    if ( attractorFound != "False" ) { 
      transitions[attractorFound]<-transitions[attractorFound] + 1  
      #transitions[attractorFound]<-transitions[attractorFound] + 1 # modify for a second version  
    }
    #cat(walkResult,"\n")  # output result for checking
  }
  return((transitions)/numberOfWalks)
}

validateTransitionsFrecuencies<-function() {
  numberOfWalks<-30
  noiseDegree<-0.1
  numberOfIterations<-200
  net<-loadNetwork('RN_MURINE_MODEL.txt')
  attractors<-getAttractors(net)
  for ( i in 1:length( attractors[2]$attractors ) ) {
    for ( j in 1:length( attractors[2]$attractors[[i]][[1]] ) ){
      state<-decimalToBinary(attractors[2]$attractors[[i]][[1]][j],length(net$genes))
      transitions<-transitionsFrecuencies(net,state,attractors,noiseDegree,numberOfIterations,numberOfWalks)
      cat("attractor", i,"\n",length(transitions),"\n")
      cat( state, findAttractor(net,decimalToBinary(state,length(net$genes)),attractors),"\n" ) 
    }
  }
}

#validateTransitionsFrecuencies()

#result<-transitionsFrecuencies(net,state,attractors,noiseDegree,numberOfIterations,numberOfWalks)
#cat(result)

######################################################################################################################


# Construct a matrix of transitions
# Use the previous function to define every entry 
# as the frecuency of transition events (or mean time)


#net<-loadNetwork('RN_MURINE_MODEL.txt')
#attractors<-getAttractors(net)
#numberOfAttractors<-length(attractors$attractors)
#markovChain<-matrix(0,numberOfAttractors,numberOfAttractors)
#for (i in 1:markovChain[1,]) {
#numberOfWalks<-30
#noiseDegree<-0.1  # settable
#numberOfIterations<-100

getTransitionsMatrix<-function(net,attractors,numberOfWalks,noiseDegree,numberOfUpdates){
  transitionsMatrix<-matrix(0,length(attractors$attractors),length(attractors$attractors))
  for (i in 1:length(attractors$attractors)){
    periodSize<-length(attractors$attractors[[i]][[1]])
    #cat(i,periodSize,"\n")
    if (1==periodSize) {
      state<-decimalToBinary(attractors$attractors[[i]][[1]][1],length(net$genes))
      frecuencies<-transitionsFrecuencies(net,state,attractors,noiseDegree,numberOfUpdates,numberOfWalks)
      transitionsMatrix[,i]<-frecuencies
      cat("transition frecuencies of attractor",i,"was calculated","\n")
    }
    else {
      representative<-sample(1:periodSize,1)
      state<-decimalToBinary(attractors$attractors[[i]][[1]][representative],length(net$genes))
      frecuencies<-transitionsFrecuencies(net,state,attractors,noiseDegree,numberOfUpdates,numberOfWalks)
      transitionsMatrix[,i]<-frecuencies
      cat("transition frecuencies of attractor",i,"was calculated","\n")
    }
  }
  heatmap(transitionsMatrix,Colv = NA,Rowv = NA)
  return(transitionsMatrix)
}
