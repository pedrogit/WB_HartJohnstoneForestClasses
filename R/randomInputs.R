###################################################################
# getRandomPixelGroupMap
###################################################################
getRandomPixelGroupMap<- function(origin = c(1541912, 1072021),
                       width = 1000,
                       crs = "ESRI:102002",
                       nbPixelGroup = 200){
  tempRast <- terra::rast(terra::ext(0, width, 0, width), resolution = 1, vals = 0)
  # Define the study area base raster
  tempRast <- terra::rast(
    crs = crs,
    nrows = width, 
    ncols = width, 
    xmin = origin[1], 
    xmax = origin[1]+width, 
    ymin = origin[2], 
    ymax = origin[2]+width,
    vals = 0)
  # mapView(tempRast)
  
  randomPolygons(ras = tempRast, numTypes = nbPixelGroup)
}
###################################################################
# getRandomCohortData
#  Return a randomly generated cohortdata table
###################################################################
getRandomCohortData <- function(nbPixelGroup, pixelSize, seed = NULL){
  set.seed(seed)
  # Restrict the number of possible species
  maxNbSpecies <- 7
  species <- as.factor(c("Pice_mar", "Pinu_ban", "Popu_tre", "Betu_pap", 
                         "Pice_gla", "Abie_bal", "Acer_rub"))
  speciesProb <- c(0.25, 0.18, 0.17, 0.12, 0.1, 0.1, 0.08)
  speciesMaxAge <- c(275, 175, 90, 110, 275, 225, 175)
  nbSpecies <- min(maxNbSpecies, length(species))
  species <- head(species, nbSpecies)
  speciesProb <- head(speciesProb, nbSpecies)
  speciesMaxAge <- head(speciesMaxAge, nbSpecies)
  
  # Set
  nbSpPerGroupWeights <- c(0.2, 0.3, 0.3, 0.15, 0.05)
  minNbSpPerGroup <- 1
  maxNbSpPerGroup <- min(length(nbSpPerGroupWeights), nbSpecies)
  
  # sum the extra weight together if the number of species is smaller than the 
  # maximum number of species per group
  while (length(nbSpPerGroupWeights) > maxNbSpPerGroup) {
    l <- length(nbSpPerGroupWeights)
    nbSpPerGroupWeights[l - 1] <- nbSpPerGroupWeights[l - 1] + nbSpPerGroupWeights[l]
    nbSpPerGroupWeights <- nbSpPerGroupWeights[-l]
  }
  
  # number of species per group
  repeats <- sample(minNbSpPerGroup:maxNbSpPerGroup, 
                    nbPixelGroup, 
                    replace = TRUE, 
                    prob=nbSpPerGroupWeights)
  #hist(repeats)
  
  # define constants needed to generate random age and biomass
  sdlog <- 0.6
  maxBiomassConst <- 100000 # g/m2 or 100 kg/m2
  growthRate <- 0.05
  inflectionAgeConst <- 0.25
  
  # This function generates a random biomass estimate using a logistic growth model.
  # The estimate depends on the age of the tree and the specified maximum age.
  randomBiomass <- function(ageV, ageMaxV){
    maxBiomassConst / (1 + exp(-growthRate * (ageV - inflectionAgeConst * ageMaxV)))
  }
  
  # # test
  # ageMax <- 300
  # plot(
  #   1:ageMax,
  #   randomBiomass(1:ageMax, rep(ageMax, ageMax))
  # )

  #debug(bio)
  
  # Define a function that generates random ages following a log-normal distribution.
  randomAge <- function(spCode){
    mean_target <- speciesMaxAge[spCode] / 4
    meanlog <- log(mean_target) - (sdlog ^ 2) / 2
    min(round(rlnorm(1, meanlog, sdlog)), 1.25 * speciesMaxAge[spCode])
  }
  
  # test
  # hist(replicate(10000, randomAge(1)))
  
  # build the data table
  pixelGroup <- c()
  speciesCode <- c()
  age <- c()
  ageMax <- c()
  spProb <- c()
  bPerPixel <- c()
  b <-  c()
  
  # for each group
  for (i in seq_along(1:nbPixelGroup)) {
    n <- repeats[i]
    
    pixelGroup <- c(pixelGroup, rep(i, n))
  
    # pick n unique numbers between 1 and 5
    newSpCodes <- sort(sample(1:nbSpecies, size=n, replace=FALSE, prob=speciesProb))
    speciesCode <- c(speciesCode, newSpCodes)
  
    # random age 
    newAges <- sapply(newSpCodes, randomAge)
    age <- c(age, newAges)
    
    # add ageMax to the table
    newAgeMax <- speciesMaxAge[newSpCodes]
    ageMax <-  c(ageMax, newAgeMax)
    
    # add species probability
    newSpProb <- speciesProb[newSpCodes]
    spProb <- c(spProb, newSpProb)
    
    # estimated biomass per pixel
    newBPerPixel <- randomBiomass(newAges, newAgeMax) * pixelSize * pixelSize
    bPerPixel <- c(bPerPixel, newBPerPixel)
    
    # multiply biomass per pixel by the prob of finding the species in the cohort
    newB <- round(newBPerPixel * newSpProb)
    b <- c(b, newB)
  }
  
  # Combine into a data frame
  dt <- data.table(pixelGroup = pixelGroup, 
                   speciesCode = factor(speciesCode, labels = species),
                   age = age,
                   ageMax = ageMax,
                   spProb = spProb,
                   bPerPixel = bPerPixel,
                   B = b)
  # plot(
  #   dt[as.integer(speciesCode) == 1]$age,
  #   bio(dt[as.integer(speciesCode) == 1]$age)
  # )
  # 
  
  return(dt)
}

#getRandomCohortData(100, 250, 20)

# old methods
# set.seed(42)
# species_levels <- c("Pinu_ban", "Pice_mar", "Betu_pap", "Popu_tre", "Acer_rub")
# cohortData <- data.table(do.call(rbind, lapply(pixel_groups, function(pg) {
#   n_cohorts <- sample(1:3, 1)  # 1–3 cohorts per pixel group
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
# 
# 
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
