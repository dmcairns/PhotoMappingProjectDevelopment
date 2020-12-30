###########################################
#               Libraries                 #
###########################################

library(exifr)
library(ggplot2)
library(ggmap)
library(rgdal)
library(dplyr)
library(DT)

###########################################
#              Source Files               #
###########################################

source("PhotoManipulation.R")
source("DataManagement.R")
source("MapCreation.R")
source("gisFunctions.R")

###########################################
#            Global Variables             #
###########################################

globalValues <- reactiveValues(b.box = NULL, selectedPhoto = NULL, photoDistTable = NULL, photoOverlapTable = NULL, photoDistTableDT = NULL, photoOverlapTableDT = NULL)
directoryPath <- ".//Data//Photos//"
