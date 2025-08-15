##################################################################
# Match species values at the genus or at the species level
##################################################################
speciesGenusMatch <- function(vals, matches){
  # browser()
  sapply(vals, function(v) any(startsWith(v, matches)))
}
# speciesGenusMatch(c("Ab", "Ac", "Bd"), c("A"))
# speciesGenusMatch(c("Ab", "Ac", "Bd"), c("Ab", "B"))

##################################################################
# Sum biomass for row matching a vector of species or genus
##################################################################
sumRelBs <- function(DT, match) {
  DT[speciesGenusMatch(as.character(DT$speciesCode), match), relB] %>% sum()
}
# df <- data.table(speciesCode = c("Ab", "Ac", "Bd"), relB = c(1, 2, 3))
# sumRelBs(df, c("Ab", "B"))
# sumRelBs(df, c("Ac", "B"))

##################################################################
# Class cohort data in preparation to be reclassified as lichen 
# by another module.
##################################################################
classStand <- function(DT, jackPineSp, larchSp, spruceSp, pb = NULL){
  # set species percentages
  sprucePct <- sumRelBs(DT, spruceSp)
  jackPinePct <- sumRelBs(DT, jackPineSp)
  larchPct <- sumRelBs(DT, larchSp)
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
  
  if (!is.null(pb) && first(DT$pgid) %% 100 == 0){
    myTick(pb = pb)
  }
  return(type)
}

myTick <- function(pb, len = 100){
    pb$tick(len = len, 
            tokens = list(totint = as.integer(pb$.__enclos_env__$private$total),
                          curint = as.integer(pb$.__enclos_env__$private$current))
    )
}

##################################################################
# Class a whole cohort data table based on relative biomass
# For now classifyStand does not modify cohortData. It only produces a raster.
##################################################################
classifyStand <- function(cohortData, pixelGroupMap, jackPineSp, larchSp, spruceSp, time = 0) {
  saveClassSummaryTable <- TRUE
  levels = c(1, 2, 3, 4, 5, 6)
  labels = c("jackpine", "larch", "spruce", "conimix", "deci", "mixed")
  colors <- c("#ADFF2F", "#0DFF2F", "#228B22", "#225522", "#B22222", "#8B4513")

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
  unique_cohortDataWithB[, pgid := .GRP, by = pixelGroup]
  nbGroup <- uniqueN(unique_cohortDataWithB$pgid)
  
  message(nbGroup, " groups to process...")

  # create the progress bar
  pb <- progress_bar$new(
    format = "Classified :curint/:totint groups. :percent done. Elapsed: :elapsedfull. ETA: :eta",
    total = nbGroup, # rounded to the nearest 100 ticks to get a final 100% 
    clear = FALSE, width = 80
  )
  
  # assign a class to every pixelGroup
  unique_cohortDataWithB[, standClass:= classStand(.SD, jackPineSp, larchSp, spruceSp, pb), by = pixelGroup, .SDcols = c("speciesCode", "relB", "pgid")]

  # display the last itetation of the progress bar if it was not
  if (nbGroup %% 100 != 0) {
    myTick(pb = pb,len = nbGroup %% 100)
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


