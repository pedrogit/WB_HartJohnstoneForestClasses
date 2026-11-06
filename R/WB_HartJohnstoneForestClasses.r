##################################################################
# Format a simulation time for use in an output file name.
# The whole part is zero-padded to 3 digits so files sort by time (003, 010, 100).
# Whole-number times keep their original names (003); fractional times keep their
# fraction (2.5 becomes 002.5).
##################################################################
formatSimulationTime <- function(simTime) {
  if (length(simTime) != 1L || !is.numeric(simTime) || !is.finite(simTime) || simTime < 0) {
    stop("`simTime` must be a single finite non-negative number.")
  }
  whole <- floor(simTime)
  stamp <- sprintf("%03.0f", whole)
  fraction <- simTime - whole
  if (fraction != 0) {
    # 15 significant digits hide floating point noise (2.1 - 2 is not exactly 0.1).
    # The leading zero of "0.5" is dropped so the stamp reads 002.5.
    stamp <- paste0(stamp, sub("^0", "", format(fraction, digits = 15, scientific = FALSE)))
  }
  stamp
}

##################################################################
# Create a factor list of cohortData species matching a vector of 
# corresponding species IDs
##################################################################
getCohortSpeciesFactors <- function(cohortData, speciesMatch, parameterName = "speciesMatch"){
  if (!is.character(speciesMatch) || anyNA(speciesMatch) ||
      any(!nzchar(trimws(speciesMatch)) | trimws(speciesMatch) != speciesMatch)) {
    stop("`", parameterName, "` must be a character vector of nonblank prefixes without surrounding whitespace.")
  }
  # Create a list of unique species code factors
  uniqueSpCodes <- unique(cohortData$speciesCode)
  # Make sure uniqueSPCodes is a factor vector
  if (!is.factor(uniqueSpCodes)) {
    stop("`speciesCode` must be a factor.")
  }

  # All groups use literal prefixes; an empty vector deliberately matches nothing.
  codes <- levels(uniqueSpCodes)
  matching <- rep(FALSE, length(codes))
  for (prefix in unique(tolower(speciesMatch))) {
    matching <- matching | startsWith(tolower(codes), prefix)
  }
  matchingSpCodes <- codes[matching]
  
  speciesFactor <- uniqueSpCodes[uniqueSpCodes %in% matchingSpCodes]
  return (speciesFactor)
}

#test
# x <- getCohortSpeciesIDs(cohortData, c("Pice", "Pinu"))
# as.integer(x)

##################################################################
# Class a whole cohort data table based on relative biomass
# classifyStand does not modify cohortData. It works on a copy and only produces a raster.
##################################################################
classifyStand <- function(
    cohortData, 
    pixelGroupMap, 
    jackPineSp = c("Pinu_ban"), 
    larchSp = c("Lari"), 
    spruceSp = c("Pice"), 
    drainageMap = NULL, 
    classificationTablePath = NULL,
    otherConiferSp = c("Abie", "Calc", "Cham", "Juni", "Pinu", "Pseu", "Sequ", "Taxu", "Thuj", "Tsug")
) {
  
  saveClassSummaryTable <- TRUE
  labels = c("deci", "mixed", "conimix", "jackpine", "larch", "spruce")
  levels = c(1L, 2L, 3L, 4L, 5L, 6L)
  #colors <- c("#B22222", "#8B4513", "#225522", "#ADFF2F", "#0DFF2F", "#228B22")
  colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")
  
  # Convert species names to factors ids
  jackPineFact <- getCohortSpeciesFactors(cohortData, jackPineSp, "jackPineSp")
  larchFact <- getCohortSpeciesFactors(cohortData, larchSp, "larchSp")
  spruceFact <- getCohortSpeciesFactors(cohortData, spruceSp, "spruceSp")
  otherConiferFact <- getCohortSpeciesFactors(cohortData, otherConiferSp, "otherConiferSp")

  # Work on a copy holding only the columns used here. Selecting columns creates a new table
  # (a plain assignment of a data.table would only add a second name for the same object), so
  # the speciesNb column added below stays local to this function. Any speciesNb column in
  # the input is not copied, and as.data.table() also accepts a plain data.frame.
  cd <- data.table::as.data.table(cohortData)[, .(pixelGroup, speciesCode, B)]

  # First match wins: named subtypes take precedence over other conifers.
  # In particular, Pinu_ban also matches Pinu but its biomass is counted once.
  cd[, speciesNb := fcase(
    speciesCode %in% jackPineFact, 1, # jackpine
    speciesCode %in% larchFact,    2, # larch
    speciesCode %in% spruceFact,   3, # spruce
    speciesCode %in% otherConiferFact, 4, # residual conifers only
    default =                    5  # other
  )]
  
  # Aggregate the biomass for each pixelGroup 
  #data.table::setkeyv(cd, cols = "pixelGroup") # Sort and set the key for faster processing
  cdWithForestClass <- cd[
    , .(
      totB      = sum(B),
      jackPineTotB = sum(B[speciesNb == 1]), # jackpine
      larchTotB    = sum(B[speciesNb == 2]), # larch
      spruceTotB   = sum(B[speciesNb == 3]), # spruce
      otherConiferTotB = sum(B[speciesNb == 4])
    ),
    by = pixelGroup
  ]
  # Mutually exclusive groups make this total safe even with overlapping prefixes.
  cdWithForestClass[, coniferTotB := jackPineTotB + larchTotB + spruceTotB + otherConiferTotB]
  
  # Determine the stand class based on relative biomass
  dominancePct <- 2/3 # Set a dominance threshold

  # Keep subtype thresholds first. Deciduous is the non-conifer share (everything outside the
  # four conifer groups) and, like every other dominance rule, its boundary is inclusive:
  # Appendix S3 of Greuel et al. (2021, after Hart et al. 2019) defines deciduous as >= 66%
  # broadleaf species.
  cdWithForestClass[, WB_HartJohnstoneForestClasses := fcase(
    (jackPineTotB / totB) >= dominancePct,                                 match("jackpine", labels),
    (larchTotB / totB)    >= 0.75,                                         match("larch", labels),
    (spruceTotB / totB)   >= dominancePct,                                 match("spruce", labels),
    (coniferTotB / totB) >= dominancePct,                                 match("conimix", labels),
    (1 - coniferTotB / totB) >= dominancePct,                              match("deci", labels),
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
                 otherConiferPct = as.integer(otherConiferTotB / totB * 100),
                 coniferPct = as.integer(coniferTotB / totB * 100),
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
    # Class 6 (wd_spruce) holds all spruce at first. Spruce on poorly drained ground
    # (drainageMap == 1, as coded by WB_VegBasedDrainage) is moved to class 7 (pd_spruce);
    # spruce on well drained ground (2) stays in class 6.
    WB_HartJohnstoneForestClassesMap[WB_HartJohnstoneForestClassesMap == match("wd_spruce", labels) & drainageMap == 1] <- 7L
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


