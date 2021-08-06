###########################################
#               Libraries                 #
###########################################

library(DT)
library(exifr)
library(ggplot2)
library(ggmap)
library(magick)
library(OpenImageR)
library(reticulate)
library(rgdal)
library(shinycssloaders)

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

globalValues <- reactiveValues(b.box = NULL, selectedPhotos = NULL, selectedPhoto2 = NULL)
directoryPath <- ".//Data//Photos//"