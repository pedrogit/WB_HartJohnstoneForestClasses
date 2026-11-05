# Tests added for the .useCache parameter. The original module never reads .useCache itself:
# it is a standard SpaDES parameter, and SpaDES caches the module's events through the
# reproducible package when it is TRUE. These tests pin what that does to this module
# (results, and the CSV file that a cached event does not rewrite). Shared helpers are in
# helper-simulation.R.

# Builds a one-time-step simulation that writes the CSV, so cached and uncached events differ
# in their side effects. `clean = TRUE` empties the folder (and so the cache) first.
makeCacheSim <- function(name, useCache, objects = spruceInputs(), clean = TRUE) {
  SpaDES.core::simInit(
    modules = moduleName, times = list(start = 0, end = 0, timeunit = "year"),
    params = list(WB_HartJohnstoneForestClasses = list(
      useDrainage = FALSE, .useCache = useCache, writeClassificationCsv = TRUE)),
    objects = objects, paths = simPaths(name, clean = clean))
}

# Number of entries in the cache folder of a simulation.
cacheEntries <- function(name) {
  nrow(reproducible::showCache(simPaths(name)$cachePath))
}

csvFiles <- function(name) {
  list.files(simPaths(name)$outputPath, pattern = "\\.csv$", full.names = TRUE)
}

test_that(".useCache defaults to FALSE and nothing is cached", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("reproducible")
  withSpades({
    sim <- SpaDES.core::spades(makeCacheSim("cache-default", useCache = FALSE))
    expect_false(SpaDES.core::params(sim)[[moduleName]]$.useCache)
    expect_equal(cacheEntries("cache-default"), 0L)
    expect_equal(classValues(sim$WB_HartJohnstoneForestClassesMap), c(6, 6, 4, 1))
  })
})

test_that("without caching a repeated run rewrites the CSV", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("reproducible")
  withSpades({
    invisible(SpaDES.core::spades(makeCacheSim("cache-off", FALSE)))
    expect_length(csvFiles("cache-off"), 1L)
    # Delete the file, then repeat the identical run in the same folders.
    unlink(csvFiles("cache-off"))
    invisible(SpaDES.core::spades(makeCacheSim("cache-off", FALSE, clean = FALSE)))
    expect_length(csvFiles("cache-off"), 1L)
  })
})

test_that(".useCache = TRUE gives the same map as no caching and fills the cache", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("reproducible")
  withSpades({
    uncached <- SpaDES.core::spades(makeCacheSim("cache-same-off", FALSE))
    cached <- SpaDES.core::spades(makeCacheSim("cache-same-on", TRUE))
    expect_gt(cacheEntries("cache-same-on"), 0L)
    expect_equal(classValues(cached$WB_HartJohnstoneForestClassesMap),
                 classValues(uncached$WB_HartJohnstoneForestClassesMap))
    expect_identical(levels(cached$WB_HartJohnstoneForestClassesMap)[[1]],
                     levels(uncached$WB_HartJohnstoneForestClassesMap)[[1]])
    expect_identical(names(cached$WB_HartJohnstoneForestClassesMap), "standtype")
  })
})

test_that("a cache hit returns the same map but does not rewrite the CSV", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("reproducible")
  withSpades({
    first <- SpaDES.core::spades(makeCacheSim("cache-hit", TRUE))
    expect_length(csvFiles("cache-hit"), 1L)
    unlink(csvFiles("cache-hit"))
    # Identical inputs and parameters in the same cache: the event is answered from the cache.
    # A cached event does not run its code, so the CSV is not written again. This is a side
    # effect of caching, documented here so that it is not a surprise.
    second <- SpaDES.core::spades(makeCacheSim("cache-hit", TRUE, clean = FALSE))
    expect_equal(classValues(second$WB_HartJohnstoneForestClassesMap),
                 classValues(first$WB_HartJohnstoneForestClassesMap))
    expect_length(csvFiles("cache-hit"), 0L)
  })
})

test_that("changed inputs invalidate the cache", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("reproducible")
  withSpades({
    first <- SpaDES.core::spades(makeCacheSim("cache-changed", TRUE))
    expect_equal(classValues(first$WB_HartJohnstoneForestClassesMap), c(6, 6, 4, 1))
    # Same folders and cache, different cohorts: every pixel group becomes poplar (deciduous).
    changed <- spruceInputs()
    changed$cohortData$speciesCode <- factor(rep("Popu_tre", 4))
    second <- SpaDES.core::spades(makeCacheSim("cache-changed", TRUE, changed, clean = FALSE))
    expect_equal(classValues(second$WB_HartJohnstoneForestClassesMap), rep(1, 4))
  })
})
