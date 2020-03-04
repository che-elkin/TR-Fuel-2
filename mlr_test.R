# first test of mlr for fuel classification

# install.packages("mlr", dependencies = TRUE)
library(mlr)
library(tidyverse)
library(sf)
library(rgdal)
library(raster)
library(stars)
library(GSIF)
library(tmap)
library(RColorBrewer)

## Load the data
modDat <- read.csv("F:/workspace/TR-Fuel-2/Height_.csv",
                   stringsAsFactors = TRUE)  ## the var to be predicted needs to be a factor
# str(modDat)


## define output directory
outDir <- "F:/workspace/TR-Fuel-2/test/"