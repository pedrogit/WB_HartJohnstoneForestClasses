library(data.table)
library(dplyr)
library(parallel)
library(progress)
library(terra)
source("G:/Home/MyTests/reclassModel/modules/standClass/R/standClass.r")
source("G:/Home/MyTools/myFunctions.R")

######################################################
# Development
######################################################
dataFolder <- "G:/Home/FromMaria/CanESM2_run1"

cohortData <- as.data.table(readRDS(file.path(dataFolder, "cohortData_year2011.rds")))

#cohortData = cohortData[pixelGroup<=1000000]
length(unique(cohortData$pixelGroup))
pixelGroupMap <- rast(readRDS(file.path(dataFolder, "pixelGroupMap_year2011.rds")))
classRast <- classifyStand(cohortData, pixelGroupMap, 
                           jackPineSp = c("Pinu_Ban"), 
                           larchSp = c("Lari"), 
                           spruceSp = c("Pice"))
summary(classRast)
hist(classRast)
fname <- file.path("G:/Home/FromMariaOutput", "standclass_year2011.tif")
terra::writeRaster(classRast, fname, datatype = "INT1U", overwrite = TRUE)

fname <- file.path("G:/Home/FromMariaOutput", "pixelGroupMap_year2011.tif")
terra::writeRaster(pixelGroupMap, fname, overwrite = TRUE)

######################################################
# Function used by sequential and parallel processing
######################################################
testfnc <- function(found_file, source_dir, target_dir = NULL, file_suffix = NULL, ext = NULL) {
  message("Processing ", found_file, "...")
  # create a target file name using the found file
  new_full_path <- new_file_path(found_file, source_dir, target_dir, file_suffix, ext)
  # create the target dir if it does not exist
  dir_name <- dirname(new_full_path)
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  }
  # build the cohort source file name
  cohortFile <- sub("pixelGroupMap", "cohortData", found_file)
  # read it
  cohortData <- as.data.table(readRDS(cohortFile))
  #read the pixel group map
  pixelGroupMap <- rast(readRDS(found_file))
  
  # classify them
  classRast <- classifyStand(cohortData, pixelGroupMap, 
                             jackPineSp = c("Pinu_Ban"), 
                             larchSp = c("Lari"), 
                             spruceSp = c("Pice"))
  
  # build the target file name for the class raster
  classFile <- sub("pixelGroupMap", "standClass", new_full_path)
  # browser()
  # write it
  terra::writeRaster(classRast, classFile, datatype = "INT1U", overwrite = TRUE)
  # and write the pixel group map raster too
  terra::writeRaster(pixelGroupMap, new_full_path, overwrite = TRUE)
}

# undebug(testfnc)

######################################################
# Sequential processing
######################################################
apply_to_files(source_dir = "G:/Home/FromMaria", regex = "^pixelGroupMap.*", fn = testfnc, target_dir = "G:/Home/FromMariaOutput", ext="tiff")

######################################################
# Parallel processing
######################################################
# debug(apply_to_files_p)
apply_to_files_p(source_dir = "G:/Home/FromMaria", regex = "^pixelGroupMap.*", fn = testfnc, target_dir = "G:/Home/FromMariaOutput", ext="tiff")


