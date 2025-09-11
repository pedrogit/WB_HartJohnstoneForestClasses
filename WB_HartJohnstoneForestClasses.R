defineModule(sim, list(
  name = "WB_HartJohnstoneForestClasses",
  description = paste("The aim of this module is to classify cohortData into predefined vegetation classes to be reclassified as lichen by another module"),
  keywords = c("lichen", "habitat types"),
  authors =  c(
    person("Pierre", "Racine", email= "pierre.racine@sbf.ulaval.ca", role = "aut")
  ),
  childModules = character(0),
  version = list(WB_HartJohnstoneForestClasses = "0.0.0.1000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  # citation = list("citation.bib"),
  # documentation = deparse(list("README.txt", "WB_HartJohnstoneForestClasses.Rmd")),
  reqdPkgs = list("SpaDES.tools", "data.table", "reproducible", "LandR"),
  parameters = rbind(
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter("WB_HartJohnstoneForestClassesTimeStep", "numeric", 1, NA, NA,
                     "Describes the simulation time at which the reclassify event should occur."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should caching of events or module be activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("jackPineSp", "character", c("Pinu_ban"), NA, NA,
                    "List of jackpine species. Can also be the whole genus: \"Pinu\""),
    defineParameter("larchSp", "character", c("Lari"), NA, NA,
                    "List of larch species. Can also be the whole genus: \"Lari\""),
    defineParameter("spruceSp", "character", c("Pice"), NA, NA,
                    "List of larch species. Can also be the whole genus: \"Pice\""),
    defineParameter("useDrainage", "logical", TRUE, NA, NA,
                    "Weither to use the provided TWI drainage map to refine spruce classes into poorly-drained spruce (pd_spruce) and well-drained sprice (wd_spruce)"),
    defineParameter("drainageThreshold", "numeric", 15.0, NA, NA,
                    "Numeric TWI threshold between well drained sites and poorly drained sites")
  ),
  inputObjects = bindrows(
    expectsInput("cohortData",  "data.table",
                 desc = paste("Initial community table, created from available biomass (g/m2)",
                              "age and species cover data, as well as ecozonation information",
                              "Columns: B, pixelGroup, speciesCode")),
    expectsInput("pixelGroupMap", "SpatRast",
                 desc = "Initial community map that has mapcodes match initial community table"),
    expectsInput("drainageMap", "SpatRast",
                 desc = "TWI soil drainage index raster"),
    ),

  outputObjects = bindrows(
    createsOutput("WB_HartJohnstoneForestClassesRast", "RasterLayer",
                  desc = paste("classification of cohort data into pre-defined",
                               "vegetation classes")),
  )
))

# Events
doEvent.WB_HartJohnstoneForestClasses = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "WB_HartJohnstoneForestClasses", "classifyStand", 1)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "WB_HartJohnstoneForestClasses", "plot", 2)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "WB_HartJohnstoneForestClasses", "save", 2)
    },

    classifyStand = {
      sim$WB_HartJohnstoneForestClassesRast <- classifyStand(cohortData = sim$cohortData, 
                                          pixelGroupMap = sim$pixelGroupMap,
                                          jackPineSp = P(sim)$jackPineSp,
                                          larchSp = P(sim)$larchSp,
                                          spruceSp = P(sim)$spruceSp,
                                          drainageMap = ifelse(P(sim)$useDrainage, sim$drainageMap, NA),
                                          drainageThreshold = P(sim)$drainageThreshold,
                                          time(sim))

      sim <- scheduleEvent(sim, time(sim) + P(sim)$WB_HartJohnstoneForestClassesTimeStep, "WB_HartJohnstoneForestClasses", "classifyStand", 1)

      return(invisible(sim))
    },

    plot = {
      Plot(sim$WB_HartJohnstoneForestClassesRast)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "WB_HartJohnstoneForestClasses", "plot", 2)
    },

    save = {
      # Save the reclassified raster
      fname <- file.path(outputPath(sim), paste0("WB_HartJohnstoneForestClasses_", sprintf("%03d", time(sim)), ".tif"))
      terra::writeRaster(sim$WB_HartJohnstoneForestClassesRast, fname, datatype = "INT1U", overwrite = TRUE)

      # Reschedule the event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "WB_HartJohnstoneForestClasses", "save", 2)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
              "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if(!suppliedElsewhere("pixelGroupMap", sim)){
    nbGroup <- 200
    pixelGroupRastWidth <- 1000
    message("pixelGrouMap not supplied. Please provide one. Creating random map ", 
            pixelGroupRastWidth, 
            " pixels by ", 
            pixelGroupRastWidth, 
            " pixels with ",
            nbGroup,
            " groups...")

    sim$pixelGroupMap <- getRandomPixelGroupMap(origin = c(1541912, 1072021),
                                                width = pixelGroupRastWidth,
                                                crs = "ESRI:102002",
                                                nbPixelGroup = nbGroup)
    # mapView(sim$pixelGroupMap)
  }
  
  if (!suppliedElsewhere("cohortData", sim)) {
    nbGroup <- length(unique(values(sim$pixelGroupMap)))
    message("cohortData not supplied. Please provide one. Generating random cohort data for ",
            nbGroup, " pixel groups...")
    sim$cohortData <- getRandomCohortData(nbPixelGroup = nbGroup, 
                                          pixelSize = res(sim$pixelGroupMap)[1])
  }

  if (P(sim)$useDrainage){
    message("useDrainage set to TRUE. Using drainage map...")
    if (!suppliedElsewhere("drainageMap", sim)){
      minD <- 0
      maxD <- 30
      message("drainageMap not supplied. Please provide one. Generating random ",
              "drainage map aligned on pixelGroupMap with values from ",
              minD, " to ", maxD, "...")

      groups <- unique(values(sim$pixelGroupMap))
      ext = terra::ext(sim$pixelGroupMap)
      drainageMap <- getRandomPixelGroupMap(origin = c(ext$xmin, ext$ymin),
                                            width = ncol(sim$pixelGroupMap),
                                            crs = crs(sim$pixelGroupMap),
                                            nbPixelGroup = length(groups))
      
      newValues <- sample(minD:maxD, length(groups), replace=TRUE)
      reclassMatrix <- cbind(groups, newValues)
      
      # Apply reclassification
      drainageMap <- classify(drainageMap, reclassMatrix)
      # mapView(drainageMap)
      
      sim$drainageMap <- drainageMap
    }
    else {
      if (!compareGeom(sim$pixelGroupMap, sim$drainageMap, 
                       stopOnError = FALSE, warncrs = TRUE)){
        message("drainageMap supplied but is not compatible with pixelGroupMap. Setting useDrainage parameter to FALSE...")
        params(sim)$WB_HartJohnstoneForestClasses$useDrainage <- FALSE
      }
    }
  }
  else {
    message("useDrainage set to FALSE. Not using any drainage map...")
  } 

  return(invisible(sim))
}


