defineModule(sim, list(
  name = "WB_HartJohnstoneForestClasses",
  description = paste("The aim of this module is to classify cohortData into predefined vegetation classes to be reclassified as lichen by another module"),
  keywords = c("lichen", "habitat types"),
  authors =  c(
    person("Pierre", "Racine", email= "pierre.racine@sbf.ulaval.ca", role = "cre"),
    person("Andres", "Caseiro Guilhem", email= "andres.caseiro-guilhem.1@ulaval.ca", role = "aut")
  ),
  childModules = character(0),
  version = list(WB_HartJohnstoneForestClasses = "0.0.0.1000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  # citation = list("citation.bib"),
  # documentation = deparse(list("README.txt", "WB_HartJohnstoneForestClasses.Rmd")),
  reqdPkgs = list("SpaDES.tools", "data.table", "reproducible", "LandR"),
  loadOrder = list(after = c("Biomass_core")),
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
                    paste("Whether to use the WB_VegBasedDrainage module map to refine ",
                          "spruce classes into poorly-drained spruce (pd_spruce) and ",
                          "well-drained sprice (wd_spruce)"))
  ),
  inputObjects = bindrows(
    expectsInput("cohortData",  "data.table",
                 desc = paste("Initial community table, created from available biomass (g/m2)",
                              "age and species cover data, as well as ecozonation information",
                              "Columns: B, pixelGroup, speciesCode")),
    expectsInput("pixelGroupMap", "SpatRast",
                 desc = "Initial community map that has mapcodes match initial community table"),
    expectsInput("WB_VegBasedDrainageMap", "SpatRast",
                 desc = "WB_VegBasedDrainage drainage map raster (1 = poorly drained, 2 = well drained)"),
    ),

  outputObjects = bindrows(
    createsOutput("WB_HartJohnstoneForestClassesMap", "RasterLayer",
                  desc = paste("classification of cohort data into pre-defined",
                               "vegetation classes")),
  )
))

# Events
doEvent.WB_HartJohnstoneForestClasses = function(sim, eventTime, eventType) {
  browser()
  switch(
    eventType,
    
    init = {
      sim <- Init(sim)
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "WB_HartJohnstoneForestClasses", "reComputeHJForestClassesMap", 1)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "WB_HartJohnstoneForestClasses", "plot", 2)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "WB_HartJohnstoneForestClasses", "save", 2)
    },

    reComputeHJForestClassesMap = {
      # browser()
      sim <- reComputeHJForestClassesMap(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$WB_HartJohnstoneForestClassesTimeStep, "WB_HartJohnstoneForestClasses", "classifyStand", 1)
    },

    plot = {
      Plot(sim$WB_HartJohnstoneForestClassesMap)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "WB_HartJohnstoneForestClasses", "plot", 2)
    },

    save = {
      # Save the reclassified raster
      fname <- file.path(outputPath(sim), paste0("WB_HartJohnstoneForestClasses_", sprintf("%03d", time(sim)), ".tif"))
      terra::writeRaster(sim$WB_HartJohnstoneForestClassesMap, fname, datatype = "INT1U", overwrite = TRUE)

      # Reschedule the event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "WB_HartJohnstoneForestClasses", "save", 2)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
              "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  # We initiate a fake cohortData here because it is dependent on sim$pixelGroupMap
  # We do that only if it is not supplied by another module (like Biomass_core)
  # We have also set loadOrder to "after Biomass_core" so pixelGroupMap should be initialized
  if (!suppliedElsewhere(sim$cohortData) && !is.null(sim$pixelGroupMap)) {
    nbGroup <- length(unique(values(sim$pixelGroupMap)))
    message("##############################################################################")   
    message("cohortData not supplied.")   
    message("Please provide one. Generating random cohort data for ", nbGroup, " pixel groups...")
    sim$cohortData <- getRandomCohortData(nbPixelGroup = nbGroup, 
                                          pixelSize = res(sim$pixelGroupMap)[1])
  }
  
  if (P(sim)$useDrainage){
    message("##############################################################################")   
    message("useDrainage set to TRUE. Using WB_VegBasedDrainageMap to refine the spruce classes into well drained and poorly drained spruce...")
    if (!suppliedElsewhere(sim$WB_VegBasedDrainageMap) && !is.null(sim$pixelGroupMap)){
      message("##############################################################################")   
      message("WB_VegBasedDrainageMap not supplied.")
      message("Please couple with the WB_VegBasedDrainage module. Generating random ",
              "drainage map aligned on pixelGroupMap with well drained and poorly drained values...",
              "If you don't this, set the useDrainage parameter to FALSE.")
      
      groups <- unique(values(sim$pixelGroupMap))
      ext = terra::ext(sim$pixelGroupMap)
      sim$WB_VegBasedDrainageMap <- getRandomCategoricalMap(
        origin = c(ext$xmin, ext$ymin),
        ncol = ncol(sim$pixelGroupMap),
        nrow = nrow(sim$pixelGroupMap),
        crs = crs(sim$pixelGroupMap),
        nbregion = length(groups),
        valuevect = 1:2
      )
      sim$WB_VegBasedDrainageMap <- mask(sim$WB_VegBasedDrainageMap, sim$pixelGroupMap)
    }
  }
  else {
    message("useDrainage set to FALSE. Not using any drainage map to refine WB_HartJohnstoneForestClasses...")
  }
  return(invisible(sim))
}

reComputeHJForestClassesMap <- function(sim) {
  drainageMap <- NULL
  if (P(sim)$useDrainage && !is.null(sim$WB_VegBasedDrainageMap)){
    drainageMap <- sim$WB_VegBasedDrainageMap
  }
  
  sim$WB_HartJohnstoneForestClassesMap <- classifyStand(
    cohortData = sim$cohortData, 
    pixelGroupMap = sim$pixelGroupMap,
    jackPineSp = P(sim)$jackPineSp,
    larchSp = P(sim)$larchSp,
    spruceSp = P(sim)$spruceSp,
    drainageMap = drainageMap,
    time(sim)
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  userTags <- c(currentModule(sim), "function:.inputObjects") 
  
  if(!suppliedElsewhere("pixelGroupMap", sim)){
    browser()
    nbGroup <- 200
    pixelGroupRastWidth <- 1000
    message("##############################################################################")
    message("pixelGrouMap not supplied.")
    message("Please provide one. Creating random map ", pixelGroupRastWidth, " pixels by ",
            pixelGroupRastWidth, " pixels with ", nbGroup, " groups...")

    sim$pixelGroupMap <- Cache(
      getRandomCategoricalMap,
      origin = c(-667296, 1758502),
      ncol = pixelGroupRastWidth,
      nrow = pixelGroupRastWidth,
      crs = "ESRI:102002",
      nbregion = nbGroup,
      seed = 100,
      userTags = c(userTags, "WB_pixelGroupMap"),
      omitArgs = c("userTags")
    )
  }

  return(invisible(sim))
}


