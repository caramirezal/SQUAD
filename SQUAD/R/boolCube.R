
boolCube <- function(boolInput,Input) {
        
        N <- 2**(length(Input))

        suma <- 0

        
        k <- length(Input)
        
        for (i in 1:N )  {
                
                state <- decimalToBinary(i-1,length(Input))
                
                ## This part can be optimized if the loop does not perform any action 
                ## when  boolInput[i] == 0/false
                
                mult <- 1
                
                for (j in 1:k ) {
                        
                        mult <- mult*boolInput[i]*( state[k-j+1]*Input[j] + ( 1 - state[k-j+1] )*( 1- Input[j] ) )
                        
                }
                
                suma <- suma + mult 
                
        }
        
        suma
}