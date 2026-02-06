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
    stop("`speciesCode` must be a factor.")
  }

  # get the list of matching SPCodes
  matchingSpCodes <- grep(
    pattern = paste0("^(", paste(speciesMatch, collapse = "|"), ")"),
    x = levels(uniqueSpCodes),
    ignore.case = TRUE,
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
classifyStand <- function(
    cohortData, 
    pixelGroupMap, 
    jackPineSp = c("Pinu_ban"), 
    larchSp = c("Lari"), 
    spruceSp = c("Pice"), 
    drainageMap = NULL, 
    classificationTablePath = NULL
) {
  
  saveClassSummaryTable <- TRUE
  labels = c("deci", "mixed", "conimix", "jackpine", "larch", "spruce")
  levels = c(1L, 2L, 3L, 4L, 5L, 6L)
  #colors <- c("#B22222", "#8B4513", "#225522", "#ADFF2F", "#0DFF2F", "#228B22")
  colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")
  
  # Convert species names to factors ids
  jackPineFact <- getCohortSpeciesFactors(cohortData, jackPineSp)
  larchFact <- getCohortSpeciesFactors(cohortData, larchSp)
  spruceFact <- getCohortSpeciesFactors(cohortData, spruceSp)

  # Copy the main data table so we do not modify the original one
  cd <- cohortData

  # Convert the species codes into integer for faster comparison
  cd[, speciesNb := fcase(
    speciesCode %in% jackPineFact, 1, # jackpine
    speciesCode %in% larchFact,    2, # larch
    speciesCode %in% spruceFact,   3, # spruce
    default =                      4  # other
  )]
  
  # Aggregate the biomass for each pixelGroup 
  #data.table::setkeyv(cd, cols = "pixelGroup") # Sort and set the key for faster processing
  cdWithForestClass <- cd[
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

  cdWithForestClass[, WB_HartJohnstoneForestClasses := fcase(
    (jackPineTotB / totB) >= dominancePct,                                 match("jackpine", labels),
    (larchTotB / totB)    >= 0.75,                                         match("larch", labels),
    (spruceTotB / totB)   >= dominancePct,                                 match("spruce", labels),
    ((jackPineTotB + larchTotB + spruceTotB) / totB) >= dominancePct,      match("conimix", labels),
    (1 - ((jackPineTotB + larchTotB + spruceTotB) / totB)) > dominancePct, match("deci", labels), 
    default =                                                              match("mixed", labels)
  )]

  # Save a summary row for each pixelGroup (to visually validate the classification)
  # This does not include the spruce class drainage refinement which is a raster based process
  # if (saveClassSummaryTable && exists("sim") && exists("getPaths", mode = "function")) {
  if (!is.null(classificationTablePath)) {
    if (!dir.exists(dirname(classificationTablePath))) {
      dir.create(dirname(classificationTablePath), recursive = TRUE)
    }
    # fname <- file.path(getPaths()$outputPath, paste0("WB_HartJohnstoneForestClasses_", sprintf("%03d", time), ".csv"))
    message("Saving classification summary table to \"", classificationTablePath, "\"...")
    # Add the class as text
    outCDWithForestClass <- cdWithForestClass[, .(
                 pixelGroup = pixelGroup,
                 B = totB,
                 jackPinePct = as.integer(jackPineTotB / totB * 100),
                 larchPct  = as.integer(larchTotB / totB * 100),
                 sprucePct  = as.integer(spruceTotB / totB * 100),
                 classInt = WB_HartJohnstoneForestClasses,
                 classText = factor(WB_HartJohnstoneForestClasses, levels = levels, labels = labels)
                )]
    write.csv(outCDWithForestClass, classificationTablePath, quote = FALSE)
  }
  
  # Rasterize
  # Create a reduced list of types per pixelGroups to be rasterized
  message("Creating WB_HartJohnstoneForestClasses raster...")
  WB_HartJohnstoneForestClassesMap <- SpaDES.tools::rasterizeReduced(reduced = cdWithForestClass,
                                                   fullRaster = pixelGroupMap,
                                                   mapcode = "pixelGroup", 
                                                   newRasterCols ="WB_HartJohnstoneForestClasses")

  # Refine spruce classification with drainage map if it is provided
  # Refine spruce classes as well/poorly drained with drainage map if it is provided
  if (!is.null(drainageMap) && !all(is.na(values(drainageMap)))) {
    message("Refine the spruce class based on drainage...")
    # Add color palette values for well and poorly drained spruce
    labels <- c(labels[-length(labels)], "wd_spruce", "pd_spruce")
    levels <- c(levels, 7L)
    colors <- c(colors, "#1b4f72")
    
    # 1 = poorly drained, 2 = well drained
    # WB_HartJohnstoneForestClassesMapOld <-
    #   ifel(WB_HartJohnstoneForestClassesMap == match("wd_spruce", labels),
    #   ifel(drainageMap == 2, match("wd_spruce", labels), match("pd_spruce", labels)),
    #   WB_HartJohnstoneForestClassesMap)
    WB_HartJohnstoneForestClassesMap[WB_HartJohnstoneForestClassesMap == match("wd_spruce", labels) & drainageMap == 2] <- 7L
  }
  
  # rasterizeReduced might produce a RasterLayer object if pixelGroupMap is a RasterLayer
  if ("SpatRaster" %in% class(WB_HartJohnstoneForestClassesMap)) {
    levels(WB_HartJohnstoneForestClassesMap) <- data.frame(ID = levels, class = labels)
    terra::coltab(WB_HartJohnstoneForestClassesMap) <- cbind(ID = levels, col = colors)
  }
  else {
    message("Could not write levels and color palette to WB_HartJohnstoneForestClassesMap because it is derived from pixelGroupMap which is not a SpatRaster raster...")
  }
  
  names(WB_HartJohnstoneForestClassesMap) <- "standtype"
  varnames(WB_HartJohnstoneForestClassesMap) <- "standtype"

  return(WB_HartJohnstoneForestClassesMap)
}


