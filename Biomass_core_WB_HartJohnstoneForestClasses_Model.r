source("G:/Home/MyTools/myFunctions.R")
source("G:/Home/MyTests/reclassModel/modules/rutils/rutils.R")

# resetSpades()
library(SpaDES)
library(SpaDES.tools)
library(LandR)
library(mapview)
library(terra)
library(data.table)

#installMyPackages()

setBasePath("G:/Home/MyTests/reclassModel")
getPaths() # shows where the 4 relevant paths are

# Create a random pixelgroupMap
pixelGroupMap <- getRandomCategoricalMap(
  origin = c(-667296, 1758502),
  width = 1000,
  crs = "ESRI:102002",
  nbregion = 1000,
  valuevect = 0:9,
  seed = 100
)
# mapview(pixelGroupMap)


# Create an ecoregionMap (only one region)
ecoregionMap <- pixelGroupMap
values(ecoregionMap) <- 1
levels(ecoregionMap) <- data.frame(ID = 1, ecoregionGroup = "1_09")
# mapview(ecoregionMap)

# Create a ecoregion table
ecoregion <- data.table(
  ecoregion = 1:1,
  names = c("1_09"),
  active = c("yes"),
  ecoregionGroup = as.factor(1:1)
)

# Create a studyArea polygon (simple square around the raster)
studyArea <- as.polygons(ext(pixelGroupMap), crs = crs(pixelGroupMap))

# mapview(studyArea, color = "red", lwd = 3, col.regions = NA, alpha.regions = 0) + mapview(pixelGroupMap)
# writeVector(studyArea, file.path(getPaths()$input,"studyArea.shp"), filetype = "ESRI Shapefile", overwrite=TRUE)

# Create the cohortData table
species_levels <- c("Pinu_ban", "Pice_mar", "Betu_pap", "Popu_tre", "Acer_rub")

# set.seed(42)
# cohortData <- data.table(do.call(rbind, lapply(pixel_groups, function(pg) {
#   n_cohorts <- sample(2:3, 1)  # 1–3 cohorts per pixel group
#   data.frame(
#     pixelGroup = as.integer(pg),
#     speciesCode = factor(
#       sample(species_levels, n_cohorts, replace = FALSE),
#       levels = species_levels
#     ),
#     age = as.integer(sample(seq(5, 150, by = 5), n_cohorts, replace = TRUE)),
#     B = as.integer((round(runif(n_cohorts, 0.5, 10), 2))),
#     ecoregionGroup = as.factor("1_09")
#     #mortality = round(runif(n_cohorts, 0, 0.5), 2)
#   )
# })))

cohortData <- fread("
pixelGroup,speciesCode,age,B,ecoregionGroup
0,Acer_rub, 75,1,1_09
0,Pice_mar,  5,3,1_09
0,Popu_tre, 45,3,1_09
1,Pinu_ban, 20,1,1_09
1,Betu_pap, 15,9,1_09
1,Acer_rub,135,5,1_09
2,Acer_rub,150,4,1_09
2,Pice_mar,100,1,1_09
3,Pinu_ban,135,5,1_09
3,Acer_rub, 70,7,1_09
3,Popu_tre, 70,6,1_09
4,Pice_mar,100,1,1_09
4,Pinu_ban,105,9,1_09
4,Betu_pap, 25,3,1_09
5,Betu_pap, 80,7,1_09
5,Pinu_ban,130,2,1_09
5,Acer_rub, 60,5,1_09
6,Pinu_ban, 10,1,1_09
6,Betu_pap, 80,1,1_09
7,Pinu_ban,145,9,1_09
7,Pice_mar, 95,9,1_09
7,Betu_pap, 50,4,1_09
8,Betu_pap,130,8,1_09
8,Pinu_ban,130,5,1_09
9,Pinu_ban, 40,7,1_09
9,Betu_pap, 20,3,1_09
") 
# Convert the speciesCode and the ecoregionGroup columns to factors
cohortData$speciesCode <- as.factor(cohortData$speciesCode)
cohortData$ecoregionGroup <- as.factor(cohortData$ecoregionGroup)

# Create the species table
species <- data.table(
  speciesCode = factor(species_levels),
  species = species_levels,
  longevity = c(140, 200, 100, 150, 300),
  maturity = c(20, 30, 10, 25, 40),
  shadetolerance = c(2, 4, 3, 5, 5),
  firetolerance = c(4, 2, 3, 1, 1),
  sexualmature = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  veg = c(FALSE, FALSE, TRUE, FALSE, TRUE),
  resproutProb = c(0.0, 0.0, 0.8, 0.0, 0.6)
)

# Create the speciesEcoregion table
speciesEcoregion <- data.table(
  ecoregionGroup = factor(c("1_09", "1_09", "1_09", "1_09", "1_09")),
  speciesCode = factor(species_levels),
  establishprob = c(0.112, 0.302, 0.714, 0.607, 0.997),
  maxB = as.integer(c(1499, 3143, 2569, 3292, 6020)),
  maxANPP = c(50, 102, 86, 110, 45),
  year = 0
)

# Create the minRelativeB table
minRelativeB <- data.frame(
  ecoregionGroup = factor(c("1_01", "1_03", "1_04", "1_05", "1_06", "1_07", "1_08", "1_09")),
  X1 = rep(0.15, 8),
  X2 = rep(0.25, 8),
  X3 = rep(0.35, 8),
  X4 = rep(0.45, 8),
  X5 = rep(0.55, 8)
)

#--- Download the module if not done already
SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("PredictiveEcology/Biomass_core@master"),
                          overwrite = FALSE)
modelTimeStep <- 10
options(spades.DTthreads = 20)

#--- simInit

# Run the WB_HartJohnstoneForestClasses module by itself
# sim <- simInit(times = list(start = 0, end = 0),
#                  modules = list("WB_HartJohnstoneForestClasses"),
#                  params = list(WB_HartJohnstoneForestClasses = list(WB_HartJohnstoneForestClassesTimeStep = 1,
#                                                  .saveInitialTime = 0,
#                                                  .saveInterval = modelTimeStep,
#                                                  useDrainage = TRUE)),
#                  objects = list(
#                    drainageMap = pixelGroupMap
#                  )
#                )


# Run a model with WB_HartJohnstoneForestClasses module in conjunction with Biomass_core
sim <- simInit(
  times = list(start = 0, end = 20),
  modules = list("Biomass_core", "WB_HartJohnstoneForestClasses"),
  # modules = list("Biomass_core"),
  params = list(
    Biomass_core = list(successionTimestep = modelTimeStep,
                        sppEquivCol = "LandR",
                        seedingAlgorithm = "noSeeding",
                        .plots = NA,
                        # calcSummaryBGM = NULL,
                        .useCache = FALSE
    )
    , WB_HartJohnstoneForestClasses = list(WB_HartJohnstoneForestClassesTimeStep = modelTimeStep,
                        .saveInitialTime = 0,
                        .saveInterval = modelTimeStep,
                        useDrainage = FALSE)
  ),
  objects = list(
    cohortData = cohortData,
    pixelGroupMap = pixelGroupMap,
    rasterToMatch = pixelGroupMap,
    #species = species,
    sppNameVector = species_levels,
    minRelativeB = minRelativeB,
    speciesEcoregion = speciesEcoregion,
    ecoregionMap = ecoregionMap,
    ecoregion = ecoregion,
    studyArea = studyArea
  )
)

sim <- spades(sim)

