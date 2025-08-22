##################################################################
# Custom progress bar tick enforcing integer current and total values.
##################################################################
myTick <- function(pb, len = 100){
  pb$tick(len = len, 
          tokens = list(totint = as.integer(pb$.__enclos_env__$private$total),
                        curint = as.integer(pb$.__enclos_env__$private$current + len))
  )
}

##################################################################
# Sum biomass for row matching a vector of species or genus
##################################################################
sumRelBs <- function(DT, match) {
  DT[as.integer(DT$speciesCode) %in% as.integer(match), relB] %>% sum()
}
# df <- data.table(sc = factor(c("Ab", "Ac", "Bd")), rb = c(1, 2, 3))
# sumRelBs(df, factor(c("Ab", "B")))
# sumRelBs(df, c("Ac", "B"))

##################################################################
# Class cohort data in preparation to be reclassified as lichen 
# by another module.
##################################################################
classStand <- function(DT, jackPineFact, larchFact, spruceFact, pb = NULL, tickFreq){
  # set species percentages
  sprucePct <- sumRelBs(DT, spruceFact)
  jackPinePct <- sumRelBs(DT, jackPineFact)
  larchPct <- sumRelBs(DT, larchFact)
  coniPct <- sprucePct + jackPinePct + larchPct
  
  # For now decideous percentage is everything not a conifer
  deciPct <- 1 - coniPct
  
  dominancePct <- 2/3;
  type <- ifelse(jackPinePct >= dominancePct, 1, # jackpine
          ifelse(larchPct >= 0.75,            2, # larch
          ifelse(sprucePct >= dominancePct,   3, # spruce
          ifelse(coniPct >= dominancePct,     4, # conimix
          ifelse(deciPct > dominancePct,      5, # deci 
                                              6  # mixed 
          )))))

  if (!is.null(pb) && first(DT$pgid) %% tickFreq == 0){
    myTick(pb = pb, len = tickFreq)
  }
  return(type)
}

##################################################################
# Create a factor list of cohortData species matching a vector of 
# corresponding species IDs
##################################################################
getCohortSpeciesFactors <- function(cohortData, speciesMatch){
  # Make sure species is a char vector
  if (!is.character(speciesMatch)) {
    stop("`speciesMatch` must be a character vector of prefixes.")
  }
  # Create a list of unique species code factors
  uniqueSpCodes <- unique(cohortData$speciesCode)
  # Make sure uniqueSPCodes is a factor vector
  if (!is.factor(uniqueSpCodes)) {
    stop("`cf` must be a factor.")
  }

  # get the list of matching SPCodes
  matchingSpCodes <- grep(
    pattern = paste0("^(", paste(speciesMatch, collapse = "|"), ")"),
    x = levels(uniqueSpCodes),
    value = TRUE
  )
  
  speciesFactor <- uniqueSpCodes[uniqueSpCodes %in% matchingSpCodes]
  return (speciesFactor)
}

#test
# x <- getCohortSpeciesIDs(cohortData, c("Pice", "Pinu"))
# as.integer(x)

##################################################################
# Class a whole cohort data table based on relative biomass
# For now classifyStand does not modify cohortData. It only produces a raster.
##################################################################
classifyStand <- function(cohortData, pixelGroupMap, jackPineSp, larchSp, spruceSp, time = 0) {
  saveClassSummaryTable <- TRUE
  labels = c("jackpine", "larch", "spruce", "conimix", "deci", "mixed")
  levels = c(1L, 2L, 3L, 4L, 5L, 6L)
  colors <- c("#ADFF2F", "#0DFF2F", "#228B22", "#225522", "#B22222", "#8B4513")
  jackPineFact <- getCohortSpeciesFactors(cohortData, jackPineSp)
  larchFact <- getCohortSpeciesFactors(cohortData, larchSp)
  spruceFact <- getCohortSpeciesFactors(cohortData, spruceSp)
  tickFreq = 1000L
  
  # Add the sum of biomass and the relative biomass per pixelGroup & speciesCode
  cohortDataWithB <- cohortData
  cohortDataWithB <- cohortDataWithB[, sumB := sum(B), by = .(pixelGroup)]
  cohortDataWithB[, relB := sum(B)/sumB, by = .(pixelGroup, speciesCode)]
  # set the NAs to 0
  cohortDataWithB[is.na(relB) & sumB == 0, relB := 0]
  
  # Check for missing values in B
  if (any(is.na(cohortDataWithB$relB))) {
    stop("Missing values in relative Biomass")
  }
  
  # Reduce to unique tuples having B > 0. XXXXX entries
  unique_cohortDataWithB <- unique(cohortDataWithB[B > 0, .(pixelGroup, speciesCode, relB, sumB)])
  
  # Sort and set the key to the pixelGroup column for faster processing
  data.table::setkeyv(unique_cohortDataWithB, cols = "pixelGroup")
  
  # DT[ , .SD, by = ...] method
  # unique_cohortDataWithB[, pgid := .GRP, by = pixelGroup]
  nbGroup <- uniqueN(unique_cohortDataWithB$pixelGroup)
  
  message(nbGroup, " groups to process...")

  # create the progress bar
  pb <- progress_bar$new(
    format = "Classified :curint/:totint groups. :percent done. Elapsed: :elapsedfull. ETA: :eta",
    total = nbGroup, # rounded to the nearest 100 ticks to get a final 100% 
    clear = FALSE, width = 80,
    show_after = 0
  )
  myTick(pb = pb, len = 0) # force it to display now
  
  browser()
  # convert the species into integer
  unique_cohortDataWithB[, speciesNb := fcase(
    speciesCode %in% jackPineFact, 1, # jackpine
    speciesCode %in% larchFact,    2, # larch
    speciesCode %in% spruceFact,   3, # spruce
    default =                      4  # other
  )]

  # assign a class to every pixelGroup
  unique_cohortDataWithB[, standClass := {
    if (.GRP %% tickFreq == 0L) myTick(pb, len=tickFreq)
    totRelB <- sum(relB)
    
    jackPinePct <- sum(relB[speciesNb == 1]) / totRelB # jackpine
    larchPct    <- sum(relB[speciesNb == 2]) / totRelB # larch
    sprucePct   <- sum(relB[speciesNb == 3]) / totRelB # spruce
    
    coniPct <- sprucePct + jackPinePct + larchPct
    deciPct <- 1 - coniPct
    dominancePct <- 2/3
    
    fcase(
      jackPinePct >= dominancePct, 1L, # jackpine
      larchPct    >= 0.75,         2L, # larch
      sprucePct   >= dominancePct, 3L, # spruce
      coniPct     >= dominancePct, 4L, # conimix
      deciPct     >  dominancePct,5L, # deci 
      default =                    6L  # mixed
    )
  }, by = pixelGroup]
  #unique_cohortDataWithB[, standClass:= classStand(.SD, jackPineFact, larchFact, spruceFact, pb, tickFreq), by = pixelGroup, .SDcols = c("speciesCode", "relB", "pgid")]

  # display the last iteration of the progress bar if it was not
  if (nbGroup %% tickFreq != 0) {
    myTick(pb = pb,len = nbGroup %% tickFreq)
  }

  # Save a summary row for each pixelGroup (to visually validate the classification)
  if (saveClassSummaryTable && exists("getPaths", mode = "function")) {
    fname <- file.path(getPaths()$outputPath, paste0("standClass_", sprintf("%03d", time), ".csv"))
    message("Save classification summary table to \"", fname, "\"...")
    grouped <- unique_cohortDataWithB[, .(sumB = first(sumB), 
                                          speciesSummary = paste0(speciesCode, "(", round(relB, 2), ")", collapse = " "),
                                          standClassInt = first(standClass),
                                          standClasschar = factor(first(standClass), levels = levels, labels = labels)), 
                                      by = pixelGroup]
    write.csv(grouped, fname)
  }
  
  # Rasterize
  # Create a reduced list of types per pixelGroups to be rasterized
  message("Creating standClass raster...")
  cohortDataRD <- unique_cohortDataWithB[, list(standClass = unique(standClass)), by = "pixelGroup"]
  standClassRast <- SpaDES.tools::rasterizeReduced(reduced = cohortDataRD,
                                                fullRaster = pixelGroupMap,
                                                mapcode = "pixelGroup", 
                                                newRasterCols ="standClass")
  
  # rasterizeReduced might produce a RasterLayer object if pixelGroupMap is a RasterLayer
  if ("SpatRaster" %in% class(standClassRast)) {
    levels(standClassRast) <- data.frame(ID = levels, class = labels)
    coltab(standClassRast) <- cbind(ID = levels, col = colors)
  }
  else {
    message("Could not write levels and color palette to standClassRast because it is derived from pixelGroupMap which is not a SpatRaster raster...")
  }
    
  return(standClassRast)
}


