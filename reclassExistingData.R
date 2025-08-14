library(data.table)
library(dplyr)
library(parallel)

dataFolder <- "G:/Home/FromMaria/CanESM2_run1"

cohortData <- as.data.table(readRDS(file.path(dataFolder, "cohortData_year2011.rds")))

cohortData = cohortData[pixelGroup<=10000]
length(unique(cohortData$pixelGroup))
pixelGroupMap <- rast(readRDS(file.path(dataFolder, "pixelGroupMap_year2011.rds")))

source("G:/Home/MyTests/reclassModel/modules/vegReclass/R/reclass.r")
vegRast <- vegReclass(cohortData, pixelGroupMap, 
                      jackPineSp = c("Pinu_Ban"), 
                      larchSp = c("Lari"), 
                      spruceSp = c("Pice"))
summary(vegRast)
hist(vegRast)
fname <- file.path("G:/Home/FromMariaOutput", "vegclass_year2011.tif")
terra::writeRaster(vegRast, fname, overwrite = TRUE)
