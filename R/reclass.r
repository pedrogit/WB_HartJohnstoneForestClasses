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
reclassSDForLichen <- function(DT, jackPineSp, larchSp, spruceSp, pb = NULL){
# browser()  
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
  
  if (!is.null(pb) && first(DT$pgid) %% 100 == 0){
    pb$tick(100)
  }
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
# Reclass a whole cohort data table based on relative biomass
reclassCohortForLichen <- function(cohortData, jackPineSp, larchSp, spruceSp) {
  start_time <- Sys.time()
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
# browser()
  
if (TRUE){
  # DT[ , .SD, by = ...] method
  unique_cohortDataWithB[, pgid := .GRP, by = pixelGroup]
  nbGroup <- uniqueN(unique_cohortDataWithB$pgid)
# browser()
  pb <- progress_bar$new(
    format = "Processed :current groups out of :total. :percent done. Time elapsed: :elapsedfull. ETA: :eta",
    total = nbGroup, # rounded to the nearest 100 ticks to get a final 100% 
    clear = FALSE, width = 80
  )
  unique_cohortDataWithB[, vegClass:= reclassSDForLichen(.SD, jackPineSp, larchSp, spruceSp, pb), by = pixelGroup, .SDcols = c("speciesCode", "relB", "pgid")]
  #unique_cohortDataWithB[, ':='(vegSum=vegSummary(.SD), vegClass= reclassSDForLichen(.SD, jackPineSp, larchSp, spruceSp)), by = pixelGroup, .SDcols = c("speciesCode", "relB")]
  pb$tick(nbGroup %% 100)
} else {
  # group by group method
  # make a list of all unique pixel groups
  groups <- unique(unique_cohortDataWithB$pixelGroup)
  # count them
  n_groups <- length(groups)
  start_time <- Sys.time()
  # init a progress bar
  # pb <- txtProgressBar(min = 0, max = n_groups, style = 3)
  
  # create a vector of results
  res_list <- vector("list", n_groups)
  
  # n_cores <- detectCores() - 1
  
  # loop over the groups
  for (i in seq_along(groups)) {
    # get the current group
    g <- groups[i]
    # create a subset data table with it
    subDT <- unique_cohortDataWithB[pixelGroup == g, .SD, .SDcols = c("speciesCode", "relB")]
    
    # class the group into the right vegClass
    res_list[[i]] <- data.table(pixelGroup = g, vegClass = reclassSDForLichen(subDT, jackPineSp, larchSp, spruceSp))
    
    # Prepare data for the progress bar
    if (i %% 100 == 0 || i == n_groups){
      elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
      percent <- round(i / n_groups * 100, 1)
      eta <- round(elapsed * (n_groups - i) / i, 0)
      msg <- sprintf(
        "Processed %d groups out of %d. %s%% done. Time elapsed: %ds. ETA: %ds.",
        i, n_groups, percent, round(elapsed), eta
      )

      cat("\r", msg)
      flush.console()
      #setTxtProgressBar(pb, i)
    }
  }
  
  #close(pb)
  unique_cohortDataWithB <- rbindlist(res_list)
}
  
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
