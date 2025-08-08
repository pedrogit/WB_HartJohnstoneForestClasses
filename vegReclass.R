 ## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "vegReclass",
  description = paste("the aim of this module is to make a reclassification of cohortData into predefined vegetation classes for
  six subpregions whitin the WBI project"),
  keywords = c("WB", "habitat types"),
  authors =  c(
    person("Alex M", "Chubaty", email= "achubaty@for-cast.ca", role = "aut"),
    person("Ana", "Raymundo", email= "angeles-ana-paula.raymundo-sanchez.1@ulaval.ca", role = "aut")
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.6.9020", vegReclass = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "vegReclass.Rmd")),
  reqdPkgs = list("qs", "fasterize", "SpaDES.tools", "googledrive", "data.table", "raster", "reproducible", "LandR"),
  parameters = rbind(
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter("reclassTimeStep", "numeric", 1, NA, NA,
                     "Describes the simulation time at which the reclassify event should occur."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter("studyAreaName", "character", "AB", NA, NA,
                    paste("study area name for each of the provinces in WB. Options are: BC, AB",
                          "SK", "MB", "NT", "NU", "YU")),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should caching of events or module be activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bindrows(
    expectsInput("cohortData",  "data.table",
                 desc = paste("Initial community table, created from available biomass (g/m2)",
                              "age and species cover data, as well as ecozonation information",
                              "Columns: B, pixelGroup, speciesCode")),
    expectsInput("sppEquivCol", "data.table",
                 desc = "The column in sim$speciesEquivalency data.table to use as a naming convention" ),
    expectsInput("sppEquiv", "data.table",
                 desc = "The column in sim$speciesEquivalency data.table to use as a naming convention" ),
    expectsInput("studyArea", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for REPORTING. This shapefile is created in the WBI preamble module"),
    expectsInput("pixelGroupMap", "RasterLayer",
                 desc = "Initial community map that has mapcodes match initial community table"),
    expectsInput("rstLCC", "RasterLayer",
                 desc = "Initial Land Cover 2005 classes")

    ),

  outputObjects = bindrows(
    createsOutput("vegTypesRas", "RasterLayer",
                  desc = paste("reclassification of cohort data into pre-defined",
                               "vegetation classes for the WBI project")),
    createsOutput("nonForestRas", "RasterLayer",
                  desc = "reclassification of non forested pixels"),

    createsOutput("ageRas", "RasterLayer",
                  desc = "reclassification of age from cohorData"),


  )
))

## event types
#   - type `init` is required for initialization

doEvent.vegReclass = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
# browser()
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "vegReclass", "reclass", 1)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "vegReclass", "plot", 2)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "vegReclass", "save", 2)
    },

    reclass = {
 browser()
      ###########################################################
      # Extract values from LandR raster of vegetation groups (stands) 
      # for the study area. 433161 rows
      ###########################################################
      SApixels <- terra::extract(sim$pixelGroupMap,
                                    sim$studyArea,
                                    cellnumbers = FALSE, df = TRUE)
      
      # Rename columns for easier reference and remove NA values
      names(SApixels) <- c("ID", "pixelGroup")
      SApixels <- na.omit(SApixels) # 371664 rows remaining
      
      ###########################################################
      # Add info and subset the cohortData table. 384778 rows
      # Add the number of pixel per cohort. Useful? Seems useless. Has no impact on the results of vegTypeGenerator()
      # Add leading vegetation. Essential.
      # Add biomass metrics and remove rows with biomass = 0. Essential.
      ###########################################################
# browser()
      # Add the number of pixel per cohort to each cohort. 384778 rows
      pixelCohortData <- LandR::addNoPixel2CohortData(sim$cohortData,
                                                      sim$pixelGroupMap,
                                                      doAssertion = getOption('LandR.assertions', TRUE))

      cohortData <- as.data.table(pixelCohortData)

      # Add leading vegetation type column. 384778 rows
      vegTypeTable <- LandR::vegTypeGenerator(cohortData, vegLeadingProportion = 0.8,
                                              mixedType = 2, sppEquiv = sim$sppEquiv,
                                              sppEquivCol = sim$sppEquivCol,
                                              pixelGroupColName = 'pixelGroup')
      
      # Add the sum of biomass and the relative biomass per pixelGroup & speciesCode
      vegTypeTable[, sumB := sum(B), by = .(pixelGroup)]
      vegTypeTable[, relB := sum(B)/sumB, by = .(pixelGroup, speciesCode)]
      # set the NAs to 0
      vegTypeTable[is.na(relB) & sumB == 0, relB := 0]
      
      # Check for missing values in B
      if (any(is.na(vegTypeTable$relB))) {
        stop("Missing values in relative Biomass")
      }
      
      # Remove null biomass entries. 383516 entries
      vegTypes <- unique(vegTypeTable[B > 0, .(pixelGroup,leading, speciesCode, relB, sumB)])
      
      # Sort and set the key to the pixelGroup column for faster processing
      data.table::setkeyv(vegTypes, cols = "pixelGroup")
      
      ###########################################################
      # Reclass every pixel of the study area
      ###########################################################
# browser()
# assign("vegTypesOld", data.table::copy(vegTypes), envir = .GlobalEnv)

      ## Reclass rules per studyArea and
      vegTypes[, vegClass:= convertToVegType(.SD, pureCutoff = 0.8,
                                             deciSp = c("Popu_tre", "Popu_bal", "Betu_pap"),
                                             coniSp = c("Pinu_ban", "Pinu_con", "Abie_bal", "Abie_las")),
               by = pixelGroup, .SDcols = c("speciesCode", "relB", "leading")]
        
      ###########################################################
      # Generate the vegTypesRas raster
      ###########################################################
# browser()
      
      # Convert the types to factors (to be rasterized)
      vegTypes$vegClass <- as.factor(vegTypes$vegClass)
      
      # Create a reduced list of types per pixelGroups
      vegTypesCD <- vegTypes[, list(vegType = unique(vegClass)), by = "pixelGroup"]
      # Make it a raster
      vegTypesRas <- SpaDES.tools::rasterizeReduced(reduced = vegTypesCD,
                                                    fullRaster = sim$pixelGroupMap,
                                                    mapcode = "pixelGroup", newRasterCols ="vegType")
      

      # Assign it to the sim
      sim$vegTypesRas <- vegTypesRas

# browser()
      sim <- scheduleEvent(sim, time(sim) + P(sim)$reclassTimeStep, "vegReclass", "reclass", 1)

      return(invisible(sim))
    },

   plot = {
 # browser()
     Plot(sim$vegTypesRas)

     sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "vegReclass", "plot", 2)
   },

  save = {
# browser()
    rname <- file.path(outputPath(sim), paste0("vegType_", time(sim), ".tif"))
    terra::writeRaster(sim$vegTypesRas, rname, overwrite = TRUE)
    
    sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "vegReclass", "save", 2)
  },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
              "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  data <- qread(file.path(modulePath(sim), "vegReclass/data/simOutDataPrep_AB.qs"))
  if (!suppliedElsewhere("studyArea", sim)) {
    message("study area not supplied. Using AB province within WB studyARea")
#browser()
    # sim$studyArea <- Cache(prepInputs, 
    #                        url = extractURL(objectName = "studyArea", sim = sim),
    #                        destinationPath = getPaths()$inputPath,
    #                       writeTo = "studyArea", overwrite = TRUE)
    #data$studyArea$BCR <- "6"
    sim$studyArea <- data$studyArea
  }
  if (!suppliedElsewhere("cohortData", sim)) {
    message("cohortData not supplied. Please provide one")
    sim$cohortData <- data$cohortData
  }
  if (!suppliedElsewhere("sppEquiv", sim)) {
    message("sppEquiv not supplied. Please provide one")
    sim$sppEquiv <- data$sppEquiv
  }
  if (!suppliedElsewhere("rstLCC", sim)) {
    message("rstLCC not supplied. Please provide one")
    #sim$rstLCC <- data$rstLCC
    sim$rstLCC <- raster(file.path(modulePath(sim), "vegReclass/data/AB_250_RTMLarge.tif"))
  }
  if (!suppliedElsewhere("sppEquivCol", sim)) {
    message("sppEquivCol not supplied. Please provide one")
    sim$sppEquivCol <- "LandR"
  }
  if(!suppliedElsewhere("pixelGroupMap", sim)){
    message("pixelGrouMap not supplied. Please provide one")
    # sim$pixelGroupMap <- Cache(LandR::prepInputsLCC,
    #                            year = 2005,
    #                            destinationPath = Paths$inputPath,
    #                            studyArea = sim$studyArea #,
    #                            #writeTo = "RTM.tif"
    #                            )
    sim$pixelGroupMap <- data$pixelGroupMap
  }

  return(invisible(sim))
}


