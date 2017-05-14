
boolCubeFun <- function(boolInput,Input) {
        if ( length(boolInput) != 2**(length(Input)) ) {
                stop("The boolean function has not length 2**(length(Input)) !")
        }
        N <- 2**(length(Input))
        suma <- 0
        k <- length(Input)
        for (i in 1:N )  {
                state <- decimalToBinary(i-1,length(Input))
                ## This part can be optimized if the loop does not perform any action 
                ## when  boolInput[i] == 0/false

                if (boolInput[i]==1) {
                        mult <- 1
                        for (j in 1:k ) {
                                mult <- mult*boolInput[i]*( state[k-j+1]*Input[j] + ( 1 - state[k-j+1] )*( 1- Input[j] ) )
                        }
                }
                if (boolInput[i]==0) {
                        mult <- 0
                }

                suma <- suma + mult 
        }
        suma
}


boolCubeFunTest <- function(binVector){
        k <- log2(length(binVector))
        cat("boolean function: ",binVector,"\n")
        for (i in 1:length(binVector)) {
                input <- decimalToBinary(i-1,k)
                cat(input," -> ", boolCubeFun(binVector,input) ,"\n")
        }
}


#################################################################################################################################

asNormHillCube <- function(net,n,k,gamma){
        if ( ! ( class(net) %in% c("BooleanNetwork","squad") ) ) {
                stop("net most be an object of class BooleanNetwork")
        }
        if ( class(net) == "BooleanNetwork") {
                network <- function(times,state,parameters) {
                        with(as.list(c(state,parameters)),{
                                newState <- rep(0,length(net$genes))
                                for (i in 1:length(net$genes)) {
                                        regulators <- net$interactions[[i]]$input
                                        continuousInput <- hillFunNorm(state[regulators],n=n[i],k=k[i])
                                        newState[i] <-  boolCubeFun(net$interactions[[i]]$func,continuousInput) - gamma[i]*state[i]
                                }
                                names(newState) <- net$genes
                                return(list(newState))
                        })
                }
                
        }

        network
}


###################################################################################################################################

hillFun <- function(x,k=0.5,n=50) { (x**n) / ( x**n + k**n ) }
    
hillFunNorm <- function(x,k=0.5,n=50) { hillFun(x,k,n) / hillFun(1) }
        
#x <- seq(0,1,length.out = 100)    
#plot(x,hillFunNorm(x,n=50,k=0.5),type = "l",col="green")



#library(deSolve)
#net <- loadNetwork("scripts/RegulatoryNetworkGMPModel/regulatoryNetworkGMPModel.txt")
#net.bc <- asBoolCube(net)
#initialState <- runif(length(net$genes))
#times <- seq(0,10,length.out = 100)
#parameters <- "default"
#sim <- ode(y=initialState,times = times,func=net.bc,parms = parameters)
#colnames(sim)[2:length(colnames(sim))] <- net$genes
#sim.m <- as.data.frame(sim)
#heatmap(as.matrix( sim.m[,2:length(sim.m[1,])]),Rowv = NA,Colv = NA)
#plot(sim.m[,1],sim.m[,2],type = "l",col="red",ylim = c(0,1))
#lines(sim.m[,1],sim.m[,3],type = "l",col="blue")
#lines(sim.m[,1],sim.m[,4],type = "l",col="green")
