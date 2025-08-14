library(data.table)
library(dplyr)
library(parallel)
library(progress)
library(terra)

# data.table::getDTthreads()
# data.table::setDTthreads(2)
# data.table::setDTthreads(20)

dataFolder <- "G:/Home/FromMaria/CanESM2_run1"

cohortData <- as.data.table(readRDS(file.path(dataFolder, "cohortData_year2011.rds")))

cohortData = cohortData[pixelGroup<=10000]
length(unique(cohortData$pixelGroup))
pixelGroupMap <- rast(readRDS(file.path(dataFolder, "pixelGroupMap_year2011.rds")))

source("G:/Home/MyTests/reclassModel/modules/standClass/R/standClass.r")
classRast <- classifyStand(cohortData, pixelGroupMap, 
                           jackPineSp = c("Pinu_Ban"), 
                           larchSp = c("Lari"), 
                           spruceSp = c("Pice"))
summary(classRast)
hist(classRast)
fname <- file.path("G:/Home/FromMariaOutput", "standclass_year2011.tif")
terra::writeRaster(classRast, fname, overwrite = TRUE)
