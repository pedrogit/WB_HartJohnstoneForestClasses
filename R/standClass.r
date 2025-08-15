##################################################################
# Match species values at the genus or at the species level
speciesGenusMatch <- function(vals, matches){
  # browser()
  sapply(vals, function(v) any(startsWith(v, matches)))
}
# speciesGenusMatch(c("Ab", "Ac", "Bd"), c("A"))
# speciesGenusMatch(c("Ab", "Ac", "Bd"), c("Ab", "B"))

##################################################################
# Sum biomass for row matching a vector of species or genus
sumRelBs <- function(DT, match) {
  DT[speciesGenusMatch(as.character(DT$speciesCode), match), relB] %>% sum()
}
# df <- data.table(speciesCode = c("Ab", "Ac", "Bd"), relB = c(1, 2, 3))
# sumRelBs(df, c("Ab", "B"))
# sumRelBs(df, c("Ac", "B"))

##################################################################
# Class cohort data in preparation to be reclassified as lichen 
# by another module.
classStand <- function(DT, jackPineSp, larchSp, spruceSp, pb = NULL){
# browser()  
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
    pb$tick(100)
  }
  return(type)
}

##################################################################
# Build a string from many cohort rows to ease comparison with 
# classification
classSummary <- function(DT){
  # browser()
  paste0(DT$speciesCode, "(", round(DT$relB, 2), ")", collapse = " ")
}

##################################################################
# Class a whole cohort data table based on relative biomass
classifyStand <- function(cohortData, pixelGroupMap, jackPineSp, larchSp, spruceSp) {
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
# browser()
  pb <- progress_bar$new(
    format = "Processed :current groups out of :total. :percent done. Time elapsed: :elapsedfull. ETA: :eta",
    total = nbGroup, # rounded to the nearest 100 ticks to get a final 100% 
    clear = FALSE, width = 80
  )
  unique_cohortDataWithB[, standClass:= classStand(.SD, jackPineSp, larchSp, spruceSp, pb), by = pixelGroup, .SDcols = c("speciesCode", "relB", "pgid")]
  #unique_cohortDataWithB[, ':='(classSum=classSummary(.SD), standClass=classStand(.SD, jackPineSp, larchSp, spruceSp)), by = pixelGroup, .SDcols = c("speciesCode", "relB")]
  if (nbGroup %% 100 != 0) {
    pb$tick(nbGroup %% 100)
  }

#browser()
  
  # Save a row for each pixelGroup
  if ("classSum" %in% colnames(unique_cohortDataWithB)) {
    fname <- file.path(outputPath(sim), paste0("classStand_", sprintf("%03d", time(sim)), ".csv"))
    grouped <- unique_cohortDataWithB[, lapply(.SD, first), by = pixelGroup, 
                                      .SDcols = c("sumB", "classSum", "standClass")]
    write.csv(grouped, fname)
  }
  
  # Rasterize
  # Create a reduced list of types per pixelGroups to be rasterized
  cohortDataRD <- unique_cohortDataWithB[, list(standClass = unique(standClass)), by = "pixelGroup"]
  standClassRast <- SpaDES.tools::rasterizeReduced(reduced = cohortDataRD,
                                                fullRaster = pixelGroupMap,
                                                mapcode = "pixelGroup", 
                                                newRasterCols ="standClass")
  levels(standClassRast) <- data.frame(ID = levels, class = labels)
  coltab(standClassRast) <- cbind(ID = levels, col = colors)
  return(standClassRast)
}


