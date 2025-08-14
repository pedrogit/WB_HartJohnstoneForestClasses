defineModule(sim, list(
  name = "vegReclass",
  description = paste("the aim of this module is to make a reclassification of cohortData into predefined vegetation classes to be reclassified as lichen by another module"),
  keywords = c("lichen", "habitat types"),
  authors =  c(
    person("Pierre", "Racine", email= "pierre.racine@sbf.ulaval.ca", role = "aut")
  ),
  childModules = character(0),
  version = list(vegReclass = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  # citation = list("citation.bib"),
  # documentation = deparse(list("README.txt", "vegReclass.Rmd")),
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
                          "and time are not relevant")),
    defineParameter("jackPineSp", "character", c("Pinu_ban"), NA, NA,
                    "List of jackpine species. Can also be the whole genus: \"Pinu\""),
    defineParameter("larchSp", "character", c("Lari"), NA, NA,
                    "List of larch species. Can also be the whole genus: \"Lari\""),
    defineParameter("spruceSp", "character", c("Pice"), NA, NA,
                    "List of larch species. Can also be the whole genus: \"Pice\"")
  ),
  inputObjects = bindrows(
    expectsInput("cohortData",  "data.table",
                 desc = paste("Initial community table, created from available biomass (g/m2)",
                              "age and species cover data, as well as ecozonation information",
                              "Columns: B, pixelGroup, speciesCode")),
    expectsInput("studyArea", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for REPORTING. This shapefile is created in the WBI preamble module"),
    expectsInput("pixelGroupMap", "RasterLayer",
                 desc = "Initial community map that has mapcodes match initial community table"),
    ),

  outputObjects = bindrows(
    createsOutput("vegTypesRas", "RasterLayer",
                  desc = paste("reclassification of cohort data into pre-defined",
                               "vegetation classes for the WBI project")),
  )
))

# Events
doEvent.vegReclass = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "vegReclass", "reclass", 1)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "vegReclass", "plot", 2)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "vegReclass", "save", 2)
    },

    reclass = {
      # browser()
      # Reclass
      unique_cohortDataWithB <- reclassCohortForLichen(cohortData = sim$cohortData, 
                                                       jackPineSp = P(sim)$jackPineSp,
                                                       larchSp = P(sim)$larchSp,
                                                       spruceSp = P(sim)$spruceSp)

      # Save a row for each pixelGroup
      if ("vegSum" %in% colnames(unique_cohortDataWithB)) {
        fname <- file.path(outputPath(sim), paste0("reclassForLichen_", sprintf("%03d", time(sim)), ".csv"))
        grouped <- unique_cohortDataWithB[, lapply(.SD, first), by = pixelGroup, 
                                          .SDcols = c("sumB", "vegSum", "vegClass")]
        write.csv(grouped, fname)
      }
      
      # Rasterize
      sim$vegTypesRas <- cohortDataToRaster(unique_cohortDataWithB, sim$pixelGroupMap)
      levels(sim$vegTypesRas) <- data.frame(value = 1:5,
                              class = c("deci", "Grassland", "Urban"))
      sim <- scheduleEvent(sim, time(sim) + P(sim)$reclassTimeStep, "vegReclass", "reclass", 1)

      return(invisible(sim))
    },

    plot = {
      Plot(sim$vegTypesRas)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "vegReclass", "plot", 2)
    },

    save = {
      # Save the reclassified raster
      fname <- file.path(outputPath(sim), paste0("vegType_", sprintf("%03d", time(sim)), ".tif"))
      terra::writeRaster(sim$vegTypesRas, fname, overwrite = TRUE)

      # Reschedule the event
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


