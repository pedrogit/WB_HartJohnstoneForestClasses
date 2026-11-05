# Tests added for the drainage refinement as seen through the module (useDrainage and the
# WB_VegBasedDrainageMap input), run through a real simInit()/spades() call. The refinement
# itself is also tested directly on classifyStand() in test-drainage-and-csv.R. Shared helpers
# are in helper-simulation.R.
#
# The labels of classes 6 and 7 are deliberately not asserted: the original has known
# inverted drainage labels (a flaw that is intentionally left alone).

# Builds a simulation with the given drainage parameter and optional drainage map input.
makeDrainageSim <- function(name, useDrainage, withMap) {
  objects <- spruceInputs()
  if (withMap) objects$WB_VegBasedDrainageMap <- drainageMapFor()
  params <- list(WB_HartJohnstoneForestClasses = if (is.null(useDrainage)) list() else
    list(useDrainage = useDrainage))
  SpaDES.core::simInit(
    modules = moduleName, times = list(start = 0, end = 0, timeunit = "year"),
    params = params, objects = objects, paths = simPaths(name, clean = TRUE))
}

test_that("useDrainage defaults to TRUE", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- makeDrainageSim("drainage-default", useDrainage = NULL, withMap = TRUE)
    expect_true(SpaDES.core::params(sim)[[moduleName]]$useDrainage)
  })
})

test_that("useDrainage = TRUE with a supplied map splits the spruce class", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeDrainageSim("drainage-on", TRUE, withMap = TRUE))
    result <- sim$WB_HartJohnstoneForestClassesMap
    # Spruce cell on poorly drained ground (1) stays 6; on well drained ground (2) it becomes 7.
    # The jack pine and poplar cells are unchanged.
    expect_equal(classValues(result), c(6, 7, 4, 1))
    expect_equal(levels(result)[[1]]$ID, 1:7)
    expect_equal(nrow(coltab(result)[[1]]), 7L)
    expect_identical(names(result), "standtype")
  })
})

test_that("useDrainage = FALSE ignores a supplied drainage map", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeDrainageSim("drainage-off", FALSE, withMap = TRUE))
    result <- sim$WB_HartJohnstoneForestClassesMap
    # Both spruce cells keep class 6 and the legend has the six standard classes.
    expect_equal(classValues(result), c(6, 6, 4, 1))
    expect_equal(levels(result)[[1]]$ID, 1:6)
  })
})

test_that("the drainage result matches calling classifyStand() directly", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeDrainageSim("drainage-direct", TRUE, withMap = TRUE))
    inputs <- spruceInputs()
    direct <- classifyQuietly(inputs$cohortData, inputs$pixelGroupMap,
                              drainageMap = drainageMapFor())
    expect_equal(classValues(sim$WB_HartJohnstoneForestClassesMap), classValues(direct))
    expect_identical(levels(sim$WB_HartJohnstoneForestClassesMap)[[1]], levels(direct)[[1]])
  })
})

test_that("useDrainage = TRUE without a map currently fails (missing random-map helper)", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    # Init() is meant to generate a random drainage map when none is supplied, but it calls
    # getRandomCategoricalMap(), which is not defined in the module. This documents that the
    # simulation stops; if the helper is added, replace this test with one for the generated map.
    expect_error(
      SpaDES.core::spades(makeDrainageSim("drainage-no-map", TRUE, withMap = FALSE)),
      "getRandomCategoricalMap")
  })
})
