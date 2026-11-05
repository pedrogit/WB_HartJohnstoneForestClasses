# Tests added for the original module run together with a Biomass_core stand-in. The stand-in
# (fixtures/Biomass_core/Biomass_core.R) is a synthetic module, not the real Biomass_core: its
# update event, at priority 7 like the real succession update, changes the species of every
# cohort (to Popu_tre at the first update, then to Lari_lar), so the class at each time shows
# whether the classification ran after the update. It also publishes successionTimestep.
# Shared helpers are in helper-simulation.R.

# Builds a simulation with the stand-in Biomass_core and the original module, both loaded from
# a private module folder made for this test.
makeCoupledSim <- function(name, successionTimestep, moduleParams = list(),
                           biomassParams = list(),
                           times = list(start = 3, end = 7, timeunit = "year"),
                           objects = list(
                             cohortData = data.table(pixelGroup = 1L, speciesCode = factor("Pice_mar"),
                                                     B = 100),
                             pixelGroupMap = rast(nrows = 1, ncols = 1, vals = 1))) {
  paths <- simPaths(name, clean = TRUE)
  modulePath <- file.path(scratch, name, "modules")
  paths$modulePath <- modulePath
  copyModuleInto(modulePath)
  stopifnot(file.copy(file.path(moduleDir, "tests", "testthat", "fixtures", "Biomass_core"),
                      modulePath, recursive = TRUE))
  SpaDES.core::simInit(
    modules = c("Biomass_core", moduleName), times = times,
    params = list(
      Biomass_core = c(list(successionTimestep = successionTimestep), biomassParams),
      WB_HartJohnstoneForestClasses = c(
        list(useDrainage = FALSE, .saveInitialTime = times$start), moduleParams)),
    objects = objects, paths = paths)
}

# Class value stored in each TIFF saved by the module, named by file.
savedClasses <- function(name) {
  files <- sort(list.files(simPaths(name)$outputPath, pattern = "\\.tif$", full.names = TRUE))
  setNames(vapply(files, function(f) classValues(rast(f))[1], numeric(1)), basename(files))
}

test_that("classification follows each succession update, for different intervals", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    # interval 1: updates at 4..7; interval 2: updates at 5 and 7.
    # The stand-in turns the stand into poplar at the first update (class 1) and into larch
    # afterwards (class 5); at the start it is spruce (class 6).
    expected <- list(
      "1" = list(times = 3:7, classes = c(6, 1, 5, 5, 5)),
      "2" = list(times = c(3, 5, 7), classes = c(6, 1, 5)))
    for (interval in names(expected)) {
      name <- paste0("coupled-interval-", interval)
      sim <- SpaDES.core::spades(makeCoupledSim(name, as.numeric(interval)))
      want <- expected[[interval]]
      expect_equal(moduleEvents(sim, "reComputeHJForestClassesMap")$eventTime, want$times,
                   info = interval)
      saved <- savedClasses(name)
      expect_equal(unname(saved), want$classes, info = interval)
      expect_equal(names(saved),
                   sprintf("WB_HartJohnstoneForestClasses_%03d.tif", want$times), info = interval)
      # The map held by the simulation at the end is the one of the last classification.
      expect_equal(classValues(sim$WB_HartJohnstoneForestClassesMap), tail(want$classes, 1))
    }
  })
})

test_that("within one time step the order is update, classification, then save", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeCoupledSim("coupled-order", 2))
    done <- as.data.frame(SpaDES.core::completed(sim))
    # Events at time 5 in the order they ran.
    at5 <- done[done$eventTime == 5 &
                  done$eventType %in% c("update", "reComputeHJForestClassesMap", "save"), ]
    expect_equal(at5$eventType, c("update", "reComputeHJForestClassesMap", "save"))
    expect_equal(at5$eventPriority, c(7, 8.5, 9))
  })
})

test_that("an explicit interval equal to the core interval is accepted, a different one stops", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeCoupledSim(
      "coupled-match", 2, moduleParams = list(WB_HJForestClassesTimeStep = 2)))
    expect_equal(moduleEvents(sim, "reComputeHJForestClassesMap")$eventTime, c(3, 5, 7))
    expect_error(SpaDES.core::spades(makeCoupledSim(
      "coupled-mismatch", 2, moduleParams = list(WB_HJForestClassesTimeStep = 3))),
      "must match Biomass_core successionTimestep")
  })
})

test_that("the time-zero classification waits for initial growth and mortality", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    # Two groups: a living spruce group and a group with zero biomass. When the stand-in
    # is asked to remove zero-biomass cohorts at time 0, it does so at priority 6 and
    # publishes its summary at 8.25, both before the classification at 8.5.
    objects <- list(
      cohortData = data.table(pixelGroup = 1:2, speciesCode = factor(c("Pice_mar", "Pinu_con")),
                              age = c(10L, 0L), B = c(100, 0)),
      pixelGroupMap = rast(nrows = 1, ncols = 2, vals = 1:2))
    sim <- SpaDES.core::spades(makeCoupledSim(
      "coupled-time-zero", 2, biomassParams = list(removeInitialZeroBiomass = TRUE),
      times = list(start = 0, end = 2, timeunit = "year"), objects = objects))
    done <- as.data.frame(SpaDES.core::completed(sim))
    atZero <- done[done$eventTime == 0, ]
    classification <- which(atZero$moduleName == moduleName &
                              atZero$eventType == "reComputeHJForestClassesMap")
    expect_length(classification, 1L)
    expect_equal(atZero$eventPriority[classification], 8.5)
    expect_lt(which(atZero$eventType == "mortalityAndGrowth"), classification)
    expect_lt(which(atZero$eventType == "summaryBGM"), classification)
    # The zero-biomass group is gone from the data and its cells were set to 0 in the map.
    expect_false(2L %in% sim$cohortData$pixelGroup)
    expect_equal(classValues(sim$pixelGroupMap), c(1, 0))
    # The time-0 TIFF holds spruce for the living group and no class for the removed one.
    first <- rast(file.path(simPaths("coupled-time-zero")$outputPath,
                            "WB_HartJohnstoneForestClasses_000.tif"))
    expect_equal(classValues(first)[1], 6)
    expect_true(is.na(classValues(first)[2]))
  })
})
