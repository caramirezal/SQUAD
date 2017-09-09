
# compilling java script
setwd("/home/carlos/")
system("javac -cp '.:scripts/SQUAD/SQUAD/inst/java/GINsim-2.9.4-with-deps.jar' scripts/SQUAD/SQUAD/inst/java/SSSearcher.java")

# creating and or installing the package
library(devtools)
library(roxygen2)
library(rJava)

#create("SQUAD")

setwd("~/scripts/SQUAD/SQUAD/")
document()

setwd("../")
install("SQUAD")


.rs.restartR()
#install_github("caramirezal/SQUAD/SQUAD",force=T)
install_github("caramirezal/SQUAD/SQUAD")

#setwd("/home/carlos/scripts/SQUAD/documentation/")

setwd("/home/carlos/")

library(SQUAD)
library(BoolNet)
library(deSolve)
library(RColorBrewer)
library(ggthemes)
library(ggplot2)
library(reshape)

# loading the Boolean model
#net.bool <- generateRandomNKNetwork(n=100,k=5,topology = "scale_free",simplify = T)
# plotting the BRN topology
#plotNetworkWiring(net.bool)

setwd("/home/carlos/")
net.bool <- loadNetwork("RegulatoryNetworkGMPModel.txt")
att <- getAttractorsAsynchronous(net.bool,returnPlot = T)


net <- loadNetwork("RegulatoryNetworkGMPModel.txt")
getAttractorsAsynchronous(net)
data("cellcycle")

getAttractorsAsynchronous(cellcycle)

remove.packages("SQUAD")

data("cellcycle")
net.sq <- asContinuous(cellcycle)
initialState <- runif(length(cellcycle$genes),0,1)
times <- seq(0,10,by=0.5)
h <- rep(50,length(cellcycle$genes))
gamma <- rep(1,length(cellcycle$genes))
parameters <- list(h,gamma)
squad(net.sq)
#im
remove.packages("SQUAD")
