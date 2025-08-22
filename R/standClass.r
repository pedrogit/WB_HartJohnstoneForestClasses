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
  
  # Convert species names to factors ids
  jackPineFact <- getCohortSpeciesFactors(cohortData, jackPineSp)
  larchFact <- getCohortSpeciesFactors(cohortData, larchSp)
  spruceFact <- getCohortSpeciesFactors(cohortData, spruceSp)

  # Copy the main data table to avoid modifying it
  cd <- cohortData

  # Convert the species into integer for faster comparison
  cd[, speciesNb := fcase(
    speciesCode %in% jackPineFact, 1, # jackpine
    speciesCode %in% larchFact,    2, # larch
    speciesCode %in% spruceFact,   3, # spruce
    default =                      4  # other
  )]
  
  # Aggregate the biomass for each pixelGroup 
  data.table::setkeyv(cd, cols = "pixelGroup") # Sort and set the key for faster processing
  cdWithPcts <- cd[
    , .(
      totB      = sum(B),
      jackPineTotB = sum(B[speciesNb == 1]), # jackpine
      larchTotB    = sum(B[speciesNb == 2]), # larch
      spruceTotB   = sum(B[speciesNb == 3])  # spruce
    ),
    by = pixelGroup
  ]
  
  
  # Determine the stand class based on relative biomass
  dominancePct <- 2/3 # Set a dominance threshold
  cdWithPcts[, standClass := fcase(
    (jackPineTotB / totB) >= dominancePct,                                 1L, # jackpine
    (larchTotB / totB)    >= 0.75,                                         2L, # larch
    (spruceTotB / totB)   >= dominancePct,                                 3L, # spruce
    ((jackPineTotB + larchTotB + spruceTotB) / totB) >= dominancePct,      4L, # conimix
    (1 - ((jackPineTotB + larchTotB + spruceTotB) / totB)) > dominancePct, 5L, # deci 
    default =                                                              6L # mixed
  )]

  # Save a summary row for each pixelGroup (to visually validate the classification)
  if (saveClassSummaryTable && exists("getPaths", mode = "function")) {
    fname <- file.path(getPaths()$outputPath, paste0("standClass_", sprintf("%03d", time), ".csv"))
    message("Save classification summary table to \"", fname, "\"...")
    # Add the class as text
    outDdWithPcts <- cdWithPcts[, .(
                 pixelGroup = pixelGroup,
                 B = totB,
                 jackPinePct = as.integer(jackPineTotB / totB * 100),
                 larchPct  = as.integer(larchTotB / totB * 100),
                 sprucePct  = as.integer(spruceTotB / totB * 100),
                 classInt = standClass,
                 classText = factor(standClass, levels = levels, labels = labels)
                )]
    write.csv(outDdWithPcts, fname, quote = FALSE)
  }
  
  # Rasterize
  # Create a reduced list of types per pixelGroups to be rasterized
  message("Creating standClass raster...")
  standClassRast <- SpaDES.tools::rasterizeReduced(reduced = cdWithPcts,
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


