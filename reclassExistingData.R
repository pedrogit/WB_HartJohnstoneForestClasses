library(data.table)
library(dplyr)
library(parallel)

dataFolder <- "G:/Home/FromMaria/CanESM2_run1"

cohortData <- as.data.table(readRDS(file.path(dataFolder, "cohortData_year2011.rds")))

cohortData = cohortData[pixelGroup<60]
length(unique(cohortData$pixelGroup))
source("G:/Home/MyTests/reclassModel/modules/vegReclass/R/reclass.r")
unique_cohortDataWithB <- reclassCohortForLichen(cohortData, 
                                                 jackPineSp = c("Pinu_Ban"),
                                                 larchSp = c("Lari"),
                                                 spruceSp = c("Pice"))
unique_cohortDataWithB$vegClass <- as.factor(unique_cohortDataWithB$vegClass)
summary(unique_cohortDataWithB$vegClass)

pixelGroupMap <- readRDS(file.path(dataFolder, "pixelGroupMap_year2011.rds"))
rast <- cohortDataToRaster(unique_cohortDataWithB, pixelGroupMap)
fname <- file.path("G:/Home/FromMariaOutput", "vegclass_year2011.tif")
terra::writeRaster(rast, fname, overwrite = TRUE)
