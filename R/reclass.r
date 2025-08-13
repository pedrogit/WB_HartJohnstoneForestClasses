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
# Reclass cohort data in preparation to be reclassified as lichen 
# by another module.
reclassSDForLichen <- function(DT, jackPineSp, larchSp, spruceSp){
  sprucePct <- sumRelBs(DT, spruceSp)
  jackPinePct <- sumRelBs(DT, jackPineSp)
  larchPct <- sumRelBs(DT, larchSp)
  coniPct <- sprucePct + jackPinePct + larchPct
  
  # For now decideous percentage is everything not a conifer
  deciPct <- 1 - coniPct
  
  dominancePct <- 2/3;
  type <- ifelse(jackPinePct >= dominancePct, "jackpine",
          ifelse(larchPct >= 0.75, "larch",
          ifelse(sprucePct >= dominancePct, "spruce",
          ifelse(coniPct >= dominancePct, "conimix",
          ifelse(deciPct > dominancePct, "deci", "mixed")))))
  
  return(type)
}

##################################################################
# Build a string from many cohort rows to ease comparison with 
# reclassification
vegSummary <- function(DT){
  # browser()
  paste0(DT$speciesCode, "(", round(DT$relB, 2), ")", collapse = " ")
}

##################################################################
# Reclass a whole cohort data table based on biomass
reclassCohortForLichen <- function(cohortData, pixelGroupMap, 
                                   jackPineSp, larchSp, spruceSp) {
  # Add the sum of biomass and the relative biomass per pixelGroup & speciesCode
  cohortDataWithB <- cohortData[, sumB := sum(B), by = .(pixelGroup)]
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
  
  unique_cohortDataWithB[, vegClass:= reclassSDForLichen(.SD, jackPineSp, larchSp, spruceSp), by = pixelGroup, .SDcols = c("speciesCode", "relB")]
  unique_cohortDataWithB[, ':='(vegSum=vegSummary(.SD), vegClass= reclassSDForLichen(.SD, jackPineSp, larchSp, spruceSp)), by = pixelGroup, .SDcols = c("speciesCode", "relB")]
  
  # browser()
  
  return(unique_cohortDataWithB)
}

##################################################################
# Convert cohort data table to raster
cohortDataToRaster <- function(cohortData, pixelGroupMap){
  # Convert the types to factors (to be rasterized)
  cohortData$vegClass <- as.factor(cohortData$vegClass)
  
  # Create a reduced list of types per pixelGroups
  cohortData <- cohortData[, list(vegClass = unique(vegClass)), by = "pixelGroup"]
  vegTypesRas <- SpaDES.tools::rasterizeReduced(reduced = cohortData,
                                                fullRaster = pixelGroupMap,
                                                mapcode = "pixelGroup", 
                                                newRasterCols ="vegClass")
  return (vegTypesRas)
}
