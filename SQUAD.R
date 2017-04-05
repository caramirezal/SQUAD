# R script project

# boolNetToSQUAD project
# July 6, 2016. Ciudad de Mexico.
# Written by Carlos Ramirez Alvarez.
# Automatically translates a Boolean network model from boolNet R package format to 
# a continuous network model using the SQUAD method as
# described in Martinez-Sosa, 2013. Biosystems. Vol. 113.
# Also add new functions for boolean regulatory network modeling

# dependencies
library(deSolve)
library(RColorBrewer)
library(ggthemes)
library(ggplot2)
library(BoolNet)
library(reshape)

# new functions
source("https://raw.githubusercontent.com/caramirezal/SQUAD/master/loadNetworkSQUAD.R")
source("https://raw.githubusercontent.com/caramirezal/SQUAD/master/asContinuous.R")
source("https://raw.githubusercontent.com/caramirezal/SQUAD/master/getAttractorsAsynchronous.R")
source("https://raw.githubusercontent.com/caramirezal/SQUAD/master/squad.R")


if ( ! file.exists( "GINsim-2.9.4-with-deps.jar" ) ) {
        download.file("http://ginsim.org/sites/default/files/GINsim-2.9.4-with-deps.jar",
                      destfile="GINsim-2.9.4-with-deps.jar")
}

if ( ! file.exists( "GetAttractors.java" ) ) {
        download.file("https://raw.githubusercontent.com/caramirezal/SQUAD/master/GetAttractors.java",
                      destfile = "GetAttractors.java")
}

#source("graphToModel.R")
#source("plotSequence.R")
#source("asContinuous.R")
#source("getAttractorsAsynchronous.R")
#source("randomGraph.R")
#source("usefulFunctions.R")


