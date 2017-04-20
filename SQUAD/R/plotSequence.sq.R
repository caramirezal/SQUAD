
heatmap.sq <- function(sim, indexes="default") {
        
        correctDefault <- ( length(indexes) == 1 ) && ( indexes == "default" ) 
        
        correctIndexes <- ( length(indexes) <= length(sim) )  && ( class(indexes) %in% c("numeric","integer")  )
        
        if ( ! ( correctDefault | correctIndexes  ) ) {
                
                stop("Indexes most be a vector numeric round numbers or integers of length less than the number of nodes ")       
        }
        
        if ( correctDefault ){
                
                if ( indexes == "default"  ) {
                        
                        sim <- sim[,2:length(sim[1,])]
                        heatmap(sim,Rowv = NA,Colv = NA,
                                col=colorRampPalette(c("black","green"))(6))
                }
        }
        
        if (  correctIndexes ) {
                
                heatmap(sim[,indexes],Rowv = NA,Colv = NA,
                        col=colorRampPalette(6))
                
        }
}

############################################################################################################################

## separar una funcion de heatmap y otra de timeSeries

# plot regulatory network dynamic as a time serie
timeSerie.sq <- function(dynamic,indexes="default") {
        
        times <- dynamic[,1]
        
        dynamic <- dynamic[,2:length(dynamic[1,])]
        
        correctDefault <- ( length(indexes) == 1 ) && ( indexes == "default" ) 
        
        correctIndexes <- ( length(indexes) <= length(sim) )  && ( class(indexes) %in% c("numeric","integer")  )
        
        if ( ! ( correctDefault | correctIndexes  ) ) {
                
                stop("Indexes most be a vector numeric round numbers or integers of length less than the number of nodes ")       
        }
        
        par(mar=c(3, 3, 3, 6),xpd=TRUE)
        
        colores <- selectedColors()
        
        if ( correctDefault ) {
                
                if ( ( length(dynamic[1,] )) <= length(colores) ) {
                        
                        indexes <- 1:length(dynamic[1,])
                        
                }
                
                if (  length(dynamic[1,]) > length(colores)  ) {
                        
                        print("Number of nodes exceeds the maximum number of printable genes. Just the first 13 will be plotted.")
                        
                        indexes <- 1:length(colores)
                        
                }
                
        }
        

        if ( correctIndexes ) {
                
                if ( length(colores) < length(indexes) ) {
                        
                        indexes <- indexes[1:length(colores)]
                        
                        print("Non default given indexes are more than the maximum number of printable genes.\nOnly the first 26 will be plotted")
                        
                }
        }
        
        etiquetas <- colnames(dynamic)
        lineType<-rep(1,length(indexes))
        timePeriod <- times[length(times)]
        
        plot(times,dynamic[,indexes[1]],col=colores[1],
             xlim = c(0,timePeriod+1),ylim = c(0,1),
             xlab = "Time",ylab = "Node Level of activation",
             type = "l",lwd=2.5,frame.plot = FALSE)
        
        for (i in 2:( length(indexes) ) ){
                
                color <- colores[i]
                        
                lines(times,dynamic[,indexes[i]],
                              col=color,type = "l",lwd=2.5)

        }
        
        legend(timePeriod+0.5,1,cex = 0.8,legend=etiquetas,lwd = 2.5,
               lty=lineType,col = colores,bty = "n")

}


##########################################################################################################################

# function similar to plotSequence in boolnet. Plot the time serie of the regulatory network based on SQUAD method
# in various formats (heatmap and time-serie)
#plotSequence.sq<-function(net,initialState="random",indexes="default",parameters="default",
#                          timePeriod=10,lengthInterval=0.01,format="heatmap"){
#  if ( format == "heatmap") {
#    heatmapSQUAD(net,initialState = initialState,indexes = indexes,parameters=parameters,
#                 timePeriod=timePeriod,lengthInterval = lengthInterval)
#  }
#  if ( format == "timeserie" ) {
#    timeSerieSQUAD(net,initialState = initialState,indexes = indexes,parameters = parameters,
#                   timePeriod=timePeriod,lengthInterval = lengthInterval)
#  }
#}

##################################################################################################



# select colors for plotting
selectedColors <- function(){
        colores <- c("gray","green","orange","red","blue","darkmagenta","darkturquoise","brown",
                     "yellow","darkgreen","pink","black","magenta")
        return(colores)
}



