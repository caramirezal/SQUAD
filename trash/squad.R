# Written by Carlos Ramirez
# January 30, 2017
# Mexico city

#source("boolNetToSQUAD_14.R")

############################################################################################################################


# select colors for plotting
selectedColors<-function(){
        colores <- c("gray","green","orange","red","blue","darkmagenta","darkturquoise","brown",
                   "yellow","darkgreen","pink","black","magenta")
        return(c(colores,colores))
}


###########################################################################################################################

# plot time serie as a heatmap
squad <- function(net,initialState="random",indexes="default",
                       parameters="default",
                       timePeriod=7.5,lengthInterval=0.01){
        if ( (class(net) != "BooleanNetwork") && (class(net) != "SQUAD") ) {
                stop('A net object of class "BooleanNetwork" or "SQUAD" most be provided!')
        }
        times=seq(0,timePeriod,by = lengthInterval)
        if (length(initialState)==1) { 
                if (initialState=="random"){
                        initialState<-runif(length(net$genes),min=0,max = 1)
                }
        }
        if (length(parameters)==1) { 
                if (parameters=="default") {
                        gamma <- rep(1,length(net$genes))
                        h <- rep(50,length(net$genes))
                        parameters<-list(h,gamma)
                }
        }
        times=seq(0,timePeriod,by = lengthInterval)
        # Usar ...
        # agregar opciones BooleanNetwork y SQUAD
        if (class(net) == "BooleanNetwork") {
                net.sq <- asContinuous(net)
                dynamic<-ode(y=initialState,times=times,func=net.sq$fun,
                             parms = parameters,atol=10e-6, rtol=10e-6)
        }
        if (class(net) == "SQUAD") {
                dynamic<-ode(y=initialState,times=times,func=net$fun,
                             parms = parameters,atol=10e-6, rtol=10e-6)
        }
        if ( 1 == length(indexes)){
                if ( indexes == "default"  ) {
                        dynamic<-dynamic[,2:length(dynamic[1,])]
                        colnames(dynamic)<-net$genes
                        heatmap(dynamic,Rowv = NA,Colv = NA,
                                col=colorRampPalette(c("black","green"))(6))
                }
        }
        if (  1 < length(indexes) ) {
                heatmap(dynamic[,indexes],Rowv = NA,Colv = NA,
                        col=colorRampPalette(6))
  }
}

############################################################################################################################

# plot regulatory network dynamic as a time serie
#timeSerieSQUAD<-function(net,initialState,indexes="default",parameters="default",timePeriod=7.5,lengthInterval=0.01){
#  times=seq(0,timePeriod,by = lengthInterval)
#  if (length(initialState)==1){ 
#    if (initialState=="random"){
#      initialState<-runif(length(net$genes),min=0,max = 1)
#    }
#  }
#  if (length(parameters)==1) { 
#    if (parameters=="default") {
#      gamma<-rep(1,length(net$genes))
#      #gammaNames<-paste("gamma",1:length(geneNames),sep = "")
#     #names(gamma)<-gammaNames
#      h<-rep(50,length(net$genes))
#      parameters<-list(h,gamma)
#      #hNames<-paste("h",1:length(geneNames),sep = "")
#      #names(h)<-hNames
#    }
#  }
#  dynamic<-ode(y=initialState,times=times,func=asContinuous(net),parms = parameters,atol=10e-6, rtol=10e-6)
#  #paleta<-palette_pander(length(net$genes))
#  pdf("plotTimeSerie.pdf")
#  par(mar=c(3, 3, 3, 6),xpd=TRUE)
#  colores<-selectedColors()
#  if ( ( length(net$genes) < 26 )  | ( length(net$genes) == 26 )) {
#    if ( length(indexes) == 1 ) {
#      if ( indexes == "default") {
#        indexes<-1:length(net$genes)
#      }
#    }
#  }
#  if (  length(net$genes) > 26  ) {
#    print("Number of nodes exceeds the maximum number of printable genes. Just the first 26 will be plotted.")
#    if (indexes=="default"){
#      indexes<-1:26
#    }
#  }
#  if ( 26 < length(indexes) ) {
#    indexes<-indexes[1:26]
#    print("Non default given indexes are more than the maximum number of printable genes.\nOnly the first 26 will be plotted")
#  }
#  etiquetas<-net$genes[indexes]
#  lineType<-rep(1,length(indexes))
#  plot(dynamic[,1],dynamic[,indexes[1]+1],col=colores[1],xlim = c(0,timePeriod),ylim = c(0,1),
#       xlab = "Time",ylab = "Node Level of activation",type = "l",lwd=2.5,frame.plot = FALSE)
#  for (i in 2:( length(indexes) ) ){
#    color<-colores[i]
#    if ( ( i < 13 ) | ( i == 13 ) ) {
#      lines(dynamic[,1],dynamic[,indexes[i]+1],col=color,type = "l",lwd=2.5)
#    }
#    if ( 13 < i ) {
#      lineType[i]<-2
#      lines(dynamic[,1],dynamic[,indexes[i]+1],col=color,type = "l",lty=2,lwd=2.5)
#    }
#  }
#  legend(timePeriod,1,cex = 0.8,legend=etiquetas,lwd = 2.5,lty=lineType,col = colores,bty = "n")
#  dev.off()
#  #colnames(dynamic)<-c("time",net$genes)
#  #x<-dynamic[,2:length(dynamic[1,])]
#  # setting the heatmap
#  #paleta<-colorRampPalette(c("black","green") ) (6)
#  #heatmap( x, Rowv = NA,Colv = NA,col= paleta )
#}


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

##########################################################################################################################





