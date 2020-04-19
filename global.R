###########################################
#               Libraries                 #
###########################################
library(exifr)
library(ggplot2)
library(ggmap)
library(rgdal)
library(dplyr)

###########################################
#              Source Files               #
###########################################
source("PhotoManipulation.R")
source("DataManagement.R")
source("MapCreation.R")

###########################################
#            Global Variables             #
###########################################
globalValues <- reactiveValues(b.box=NULL)