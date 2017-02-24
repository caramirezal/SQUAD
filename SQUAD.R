# R script project

# boolNetToSQUAD project
# July 6, 2016. Ciudad de Mexico.
# Written by Carlos Ramirez Alvarez.
# Automatically translate a Boolean network model from boolNet R package format to 
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
source("https://github.com/caramirezal/SQUAD/blob/master/asContinuous.R")
source("https://github.com/caramirezal/SQUAD/blob/master/plotSequence.R")
#source("graphToModel.R")
#source("plotSequence.R")
#source("randomGraph.R")
#source("usefulFunctions.R")
