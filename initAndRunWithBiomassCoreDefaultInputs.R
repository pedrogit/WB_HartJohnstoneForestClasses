modelPath <- "projects/base_withSimInit"

# source(file.path(modelPath, "modules/rutils/rutils.R"))
source("https://raw.githubusercontent.com/pedrogit/rUtils/refs/heads/main/rutils.R")

library(SpaDES)
library(LandR)
library(mapview)
library(terra)
library(data.table)
library(whitebox)
library(caret)

setBasePath(modelPath)
getPaths() # shows where the 4 relevant paths are

#--- Download the module if not done already
SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("PredictiveEcology/Biomass_core@main"),
                          overwrite = FALSE)

SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("PredictiveEcology/Biomass_speciesData@main"),
                          overwrite = FALSE)

SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("PredictiveEcology/Biomass_borealDataPrep@main"),
                          overwrite = FALSE)

SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("PredictiveEcology/Biomass_speciesParameters@main"),
                          overwrite = FALSE)

SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("pedrogit/WB_HartJohnstoneForestClasses@main"),
                          overwrite = FALSE)

SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("pedrogit/WB_VegBasedDrainage@main"),
                          overwrite = FALSE)

SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("pedrogit/WB_NonForestedVegClasses@main"),
                          overwrite = FALSE)

SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("pedrogit/WB_LichenBiomass@main"),
                          overwrite = FALSE)

options = options(
  spades.DTthreads = 20
  , spades.cacheChaining = TRUE
  , reproducible.useMemoise = TRUE
  # , reproducible.interactiveOnDownloadFail = FALSE
)
modelTimeStep <- 10
options(spades.DTthreads = 20)

# options('reproducible.interactiveOnDownloadFail' = FALSE)
sim <- SpaDES.core::simInit(
  times = list(start = 0, end = 20),
  # modules = list("Biomass_core", "WB_HartJohnstoneForestClasses", "WB_VegBasedDrainage"),
  # modules = list("Biomass_core", "WB_HartJohnstoneForestClasses"),
  # modules = list("Biomass_core"),
  # modules = list("WB_VegBasedDrainage"),
  modules = list(
    "Biomass_speciesData"
  , "Biomass_borealDataPrep"
  , "Biomass_speciesParameters"
  , "Biomass_core"
  , "WB_HartJohnstoneForestClasses"
  , "WB_VegBasedDrainage"
  , "WB_LichenBiomass"
  ),
  params = list(
    .globals = list(sppEquivCol = 'LandR'),
    # Biomass_borealDataPrep = list(overrideAgeInFires = FALSE
    # ),
    Biomass_speciesParameters = list(maxBInFactorial = 500,
                                     .useCache = c(".inputObjects", "init")
    ),

    Biomass_speciesData = list(
      .plots = NA
    ),
    Biomass_core = list(successionTimestep = modelTimeStep,
                        sppEquivCol = "LandR",
                        seedingAlgorithm = "noSeeding",
                        .plots = NA,
                        calcSummaryBGM = NULL,
                        .useCache = FALSE
    ),
    
    WB_HartJohnstoneForestClasses = list(
      WB_HartJohnstoneForestClassesTimeStep = modelTimeStep,
      .saveInitialTime = 0,
      .saveInterval = modelTimeStep,
      useDrainage = TRUE
    ),
    
    WB_VegBasedDrainage = list(
      WB_VegBasedDrainageTimeStep = modelTimeStep,
      searchDistInPixelNb = 2
    ),
    
    WB_NonForestedVegClasses = list(
      WB_NonForestedVegClassesTimeStep = modelTimeStep
    ),
    
    WB_LichenBiomass = list(
      WB_LichenBiomassTimeStep = modelTimeStep
    )
  ),
  objects = list(
    studyArea = {
      # create and use a random study area
      # Lambert Conformal Conic for Canada: this is used in NRCan's "KNN" products
      Biomass_corecrs <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      centre <- terra::vect(cbind(-104.757, 55.68663), crs = "epsg:4326") # Lat Long
      centre <- terra::project(centre, Biomass_corecrs)
      studyArea <- LandR::randomStudyArea(centre, size = 2e8, seed = 1234)
    },
    studyAreaLarge = terra::buffer(studyArea, width = 3e4)
  )
)

sim <- spades(sim)
