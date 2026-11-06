# Tests added for the module level behaviour of the original module: parameters, event
# scheduling and optional outputs, run through a real simInit()/spades() call. Drainage is
# switched off in these simulations (useDrainage = FALSE); it is covered in
# test-module-drainage.R. Shared helpers are in helper-simulation.R.

# Four pixel groups, one per class when the species prefixes below are used.
fourGroupInputs <- function() list(
  cohortData = data.table(pixelGroup = 1:4, speciesCode = factor(c("Jack", "Lar", "Spr", "Other")),
                          B = 100),
  pixelGroupMap = rast(nrows = 1, ncols = 4, vals = 1:4))

# Builds a simulation of the module with the given parameters.
makeSim <- function(name, params = list(), times = list(start = 3, end = 7, timeunit = "year")) {
  SpaDES.core::simInit(
    modules = moduleName, times = times,
    params = list(WB_HartJohnstoneForestClasses = c(list(useDrainage = FALSE), params)),
    objects = fourGroupInputs(), paths = simPaths(name, clean = TRUE))
}

test_that("parameter names and defaults are as documented", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- makeSim("params")
    p <- SpaDES.core::params(sim)[[moduleName]]
    # The time and interval parameters default to NA, which SpaDES stores as logical NA.
    naParameters <- c(".plotInitialTime", ".plotInterval", ".saveInitialTime", ".saveInterval",
                      "WB_HJForestClassesTimeStep")
    expected <- list(.plots = "screen",
      .useCache = FALSE, jackPineSp = "Pinu_ban", larchSp = "Lari", spruceSp = "Pice",
      otherConiferSp = c("Abie", "Calc", "Cham", "Juni", "Pinu", "Pseu", "Sequ", "Taxu", "Thuj", "Tsug"),
      writeClassificationCsv = FALSE)
    for (name in names(expected)) expect_equal(p[[name]], expected[[name]], info = name)
    for (name in naParameters) expect_true(is.na(p[[name]]), info = name)
    expect_true("useDrainage" %in% names(p))
  })
})

test_that("by default the module classifies at the start only and writes no file", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    # With a standalone run the interval is 10 years, so a 3 to 7 run classifies only at 3.
    sim <- SpaDES.core::spades(makeSim("defaults"))
    expect_equal(moduleEvents(sim, "reComputeHJForestClassesMap")$eventTime, 3)
    expect_equal(moduleEvents(sim, "reComputeHJForestClassesMap")$eventPriority, 8.5)
    expect_length(list.files(simPaths("defaults")$outputPath), 0)
    expect_equal(nrow(moduleEvents(sim, "save")), 0L)
    # Default species prefixes: Jack/Lar/Spr/Other do not match, so everything is deciduous.
    expect_equal(classValues(sim$WB_HartJohnstoneForestClassesMap), rep(1, 4))
    expect_identical(names(sim$WB_HartJohnstoneForestClassesMap), "standtype")
  })
})

test_that("an explicit interval schedules classification and save events independently", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeSim("events", list(
      WB_HJForestClassesTimeStep = 2, writeClassificationCsv = TRUE, jackPineSp = "Jack",
      larchSp = "Lar", spruceSp = "Spr", otherConiferSp = "Other",
      .saveInitialTime = 4, .saveInterval = 3)))
    expect_equal(classValues(sim$WB_HartJohnstoneForestClassesMap), c(4, 5, 6, 3))
    # Classification runs at the start and then every 2 years; saves are on their own schedule.
    expect_equal(moduleEvents(sim, "reComputeHJForestClassesMap")$eventTime, c(3, 5, 7))
    saves <- moduleEvents(sim, "save")
    expect_equal(saves$eventTime, c(4, 7))
    expect_true(all(saves$eventPriority == 9))
    outputs <- list.files(simPaths("events")$outputPath)
    expect_equal(sort(grep("\\.csv$", outputs, value = TRUE)),
                 paste0("WB_HartJohnstoneForestClasses_", c("003", "005", "007"), ".csv"))
    expect_equal(sort(grep("\\.tif$", outputs, value = TRUE)),
                 paste0("WB_HartJohnstoneForestClasses_", c("004", "007"), ".tif"))
  })
})

test_that("the saved TIFF holds the class codes at the save time", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeSim("tiff", list(
      jackPineSp = "Jack", larchSp = "Lar", spruceSp = "Spr", otherConiferSp = "Other",
      .saveInitialTime = 3, .saveInterval = 100)))
    tif <- file.path(simPaths("tiff")$outputPath, "WB_HartJohnstoneForestClasses_003.tif")
    expect_true(file.exists(tif))
    expect_equal(classValues(rast(tif)), c(4, 5, 6, 3))
    expect_equal(classValues(rast(tif)), classValues(sim$WB_HartJohnstoneForestClassesMap))
  })
})

test_that("writeClassificationCsv is off by default and writes one file per classification", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    off <- makeSim("csv-off", list(WB_HJForestClassesTimeStep = 2))
    invisible(SpaDES.core::spades(off))
    expect_length(list.files(simPaths("csv-off")$outputPath, pattern = "\\.csv$"), 0)
    on <- makeSim("csv-on", list(WB_HJForestClassesTimeStep = 2, writeClassificationCsv = TRUE,
                                 jackPineSp = "Jack"))
    invisible(SpaDES.core::spades(on))
    files <- list.files(simPaths("csv-on")$outputPath, pattern = "\\.csv$", full.names = TRUE)
    expect_equal(basename(files), paste0("WB_HartJohnstoneForestClasses_", c("003", "005", "007"), ".csv"))
    # Group 1 is Jack: 100% jack pine, class 4.
    expect_equal(read.csv(files[1], check.names = FALSE)$classInt[1], 4L)
  })
})

test_that("invalid or mismatched intervals stop the simulation", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    expect_error(SpaDES.core::spades(makeSim("interval-zero", list(WB_HJForestClassesTimeStep = 0))),
                 "finite and positive")
    expect_error(SpaDES.core::spades(makeSim("interval-negative", list(WB_HJForestClassesTimeStep = -1))),
                 "finite and positive")
    # An explicit interval must equal the Biomass_core succession timestep when it is present.
    mismatch <- makeSim("interval-mismatch", list(WB_HJForestClassesTimeStep = 3))
    SpaDES.core::params(mismatch)$Biomass_core <- list(successionTimestep = 2)
    expect_error(SpaDES.core::spades(mismatch), "must match")
  })
})

test_that("an NA interval follows the Biomass_core succession timestep", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- makeSim("interval-follows-core")
    SpaDES.core::params(sim)$Biomass_core <- list(successionTimestep = 2)
    sim <- SpaDES.core::spades(sim)
    expect_equal(moduleEvents(sim, "reComputeHJForestClassesMap")$eventTime, c(3, 5, 7))
  })
})

test_that("an existing TIFF is overwritten by a later run (original behaviour)", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    params <- list(.saveInitialTime = 3, .saveInterval = 100, jackPineSp = "Jack")
    # Two simulations share one output folder: it is cleaned once here and not between runs.
    paths <- simPaths("overwrite", clean = TRUE)
    build <- function() SpaDES.core::simInit(
      modules = moduleName, times = list(start = 3, end = 7, timeunit = "year"),
      params = list(WB_HartJohnstoneForestClasses = c(list(useDrainage = FALSE), params)),
      objects = fourGroupInputs(), paths = paths)
    tif <- file.path(paths$outputPath, "WB_HartJohnstoneForestClasses_003.tif")
    invisible(SpaDES.core::spades(build()))
    expect_true(file.exists(tif))
    # Replace the file with stale text; the second run must replace it with a valid raster.
    writeLines("stale", tif)
    expect_no_error(SpaDES.core::spades(build()))
    expect_equal(classValues(rast(tif))[1], 4)
  })
})

test_that("missing inputs do not produce a result silently", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    # The original module generates random inputs when none are supplied, using helper
    # functions that are not part of the module folder. Standalone, this currently stops
    # with an error (raised while the simulation is built); only the fact that it does not
    # succeed silently is asserted here.
    expect_error(SpaDES.core::spades(SpaDES.core::simInit(
      modules = moduleName, times = list(start = 0, end = 0, timeunit = "year"),
      params = list(WB_HartJohnstoneForestClasses = list(useDrainage = FALSE)),
      paths = simPaths("missing-inputs", clean = TRUE))))
  })
})

# The stamp used in output file names: whole times are zero-padded, fractional times keep
# their fraction.
test_that("formatSimulationTime keeps whole-number names and keeps fractions", {
  expect_equal(formatSimulationTime(3), "003")
  expect_equal(formatSimulationTime(0), "000")
  expect_equal(formatSimulationTime(100), "100")
  expect_equal(formatSimulationTime(1234), "1234")
  expect_equal(formatSimulationTime(2.5), "002.5")
  expect_equal(formatSimulationTime(0.25), "000.25")
  # Floating point noise is not written into the name.
  expect_equal(formatSimulationTime(2.1), "002.1")
  expect_equal(formatSimulationTime(0.1 + 0.2), "000.3")
  # Names of different times sort in time order.
  stamps <- vapply(c(0.5, 2, 10, 2.5), formatSimulationTime, character(1))
  expect_equal(stamps[order(stamps)], c("000.5", "002", "002.5", "010"))
  expect_error(formatSimulationTime(NA_real_), "single finite")
  expect_error(formatSimulationTime(-1), "single finite")
  expect_error(formatSimulationTime(c(1, 2)), "single finite")
  expect_error(formatSimulationTime("3"), "single finite")
})

test_that("fractional simulation times give fractional TIFF and CSV names", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeSim("fractional", list(
      WB_HJForestClassesTimeStep = 1.5, writeClassificationCsv = TRUE, jackPineSp = "Jack",
      larchSp = "Lar", spruceSp = "Spr", otherConiferSp = "Other",
      .saveInitialTime = 0.5, .saveInterval = 2),
      times = list(start = 0.5, end = 4, timeunit = "year")))
    expect_equal(moduleEvents(sim, "reComputeHJForestClassesMap")$eventTime, c(0.5, 2, 3.5))
    outputs <- list.files(simPaths("fractional")$outputPath)
    expect_equal(sort(grep("\\.csv$", outputs, value = TRUE)),
                 paste0("WB_HartJohnstoneForestClasses_", c("000.5", "002", "003.5"), ".csv"))
    expect_equal(sort(grep("\\.tif$", outputs, value = TRUE)),
                 paste0("WB_HartJohnstoneForestClasses_", c("000.5", "002.5"), ".tif"))
  })
})
