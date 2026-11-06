# Tests added for the classification rules, raster output and input handling of
# classifyStand(). They use hand-written expected values, so they check the rules rather than
# only the current output. Drainage is covered in test-drainage-and-csv.R.

test_that("six classes, geometry, legend and layer name", {
  map <- rast(nrows = 2, ncols = 4, vals = c(10, 20, 30, 40, 50, 60, 0, NA))
  cohorts <- data.table(
    pixelGroup = c(10, 20, 20, 30, 30, 40, 40, 50, 60),
    speciesCode = factor(c("Popu_tre", "Popu_tre", "Pice_mar", "Pice_mar",
                           "Abie_bal", "Pinu_ban", "Pinu_con", "Lari_lar", "Pice_gla")),
    B = c(100, 50, 50, 50, 50, 70, 30, 100, 100)
  )
  result <- classifyQuietly(cohorts, map)
  # Groups 10..60 give classes 1..6; group id 0 has no cohorts and the last cell is NA.
  expect_equal(classValues(result), c(1:6, NA, NA))
  expect_true(compareGeom(map, result))
  expect_identical(levels(result)[[1]], data.frame(
    ID = 1:6, class = c("deci", "mixed", "conimix", "jackpine", "larch", "spruce")))
  expect_equal(
    unname(as.matrix(coltab(result)[[1]][, 2:4])),
    unname(t(col2rgb(c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")))))
  expect_identical(names(result), "standtype")
  expect_identical(terra::varnames(result), "standtype")
})

test_that("default rules aggregate ages and keep jack pine separate from other pines", {
  map <- rast(nrows = 1, ncols = 8, vals = 1:8)
  cohorts <- data.table(
    pixelGroup = c(1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8),
    speciesCode = factor(c(
      "Popu_tre", "Popu_tre", "Pice_mar", "Pinu_ban", "Pinu_con",
      "Lari_lar", "Popu_tre", "Abie_bal", "unknown",
      "Pinu_ban", "Pinu_ban", "Pice_mar", "Pice_gla", NA)),
    age = seq_len(14),
    B = c(34, 34, 32, 40, 60, 70, 30, 100, 100, 40, 60, 40, 60, 100)
  )
  # Group 2: Pinu_ban 40 + Pinu_con 60 -> only 40% jack pine, but 100% conifer -> conimix.
  # Group 8 has an NA species code, which is a non-conifer -> deci.
  expect_equal(classValues(classifyQuietly(cohorts, map)), c(1, 3, 3, 3, 1, 4, 6, 1))
})

test_that("two-thirds and 75 percent thresholds sit on the inclusive side", {
  map <- rast(nrows = 1, ncols = 3, vals = 1:3)
  epsilon <- 1e-8
  # Each case: shares just below, exactly at and just above the threshold.
  cases <- list(
    list(species = "Pinu_ban", threshold = 2/3, expected = c(2, 4, 4)),
    list(species = "Lari_lar", threshold = 0.75, expected = c(3, 5, 5)),
    list(species = "Pice_mar", threshold = 2/3, expected = c(2, 6, 6)),
    list(species = "Abie_bal", threshold = 2/3, expected = c(2, 3, 3))
  )
  for (case in cases) {
    share <- case$threshold + c(-epsilon, 0, epsilon)
    d <- data.table(
      pixelGroup = rep(1:3, each = 2),
      speciesCode = factor(rep(c(case$species, "Popu_tre"), 3)),
      B = as.vector(rbind(share, 1 - share)))
    expect_equal(classValues(classifyQuietly(d, map)), case$expected, info = case$species)
  }
  # The deciduous test is inclusive too (>= 2/3, Appendix S3: >= 66% broadleaf) and is
  # computed as 1 - conifer share. The middle case sits exactly on the boundary: deci, not mixed.
  conifer <- (1 - 2/3) + c(epsilon, 0, -epsilon)
  d <- data.table(
    pixelGroup = rep(1:3, each = 2),
    speciesCode = factor(rep(c("Abie_bal", "Popu_tre"), 3)),
    B = as.vector(rbind(conifer, 1 - conifer)))
  expect_equal(classValues(classifyQuietly(d, map)), c(2, 1, 1))
})

test_that("rule priority: jack pine, larch, spruce, conifer mix, deciduous, mixed", {
  map <- rast(nrows = 1, ncols = 4, vals = 1:4)
  d <- data.table(
    pixelGroup = c(1, 1, 2, 2, 3, 3, 4, 4),
    speciesCode = factor(c("Pinu_ban", "Lari_lar", "Lari_lar", "Pice_mar",
                           "Pice_mar", "Abie_bal", "Popu_tre", "Pice_mar")),
    B = c(70, 30, 80, 20, 70, 30, 50, 50)
  )
  # 1: jack pine 70% beats everything; 2: larch 80%; 3: spruce 70%; 4: 50% conifer -> mixed.
  expect_equal(classValues(classifyQuietly(d, map)), c(4, 5, 6, 2))
})

test_that("zero ids with cohorts are classified; ids without cohorts and NA stay NA", {
  map <- rast(nrows = 1, ncols = 5, vals = c(0, 1, 2, 3, NA))
  d <- data.table(
    pixelGroup = c(0L, 1L, 2L, 99L),
    speciesCode = factor(c("Pinu_ban", "Pice_mar", "unknown", "unused")),
    B = c(100, 0, 100, 100))
  # Group 1 has zero total biomass: 0/0 is NaN, every test fails, so it is mixed (2).
  expect_equal(classValues(classifyQuietly(d, map)), c(4, 2, 1, NA, NA))
  # Without the cohorts of group 0, its cell has no class.
  expect_equal(classValues(classifyQuietly(d[-1], map)), c(NA, 2, 1, NA, NA))
})

test_that("empty cohorts give an all-NA raster", {
  empty <- data.table(pixelGroup = integer(), speciesCode = factor(), B = numeric())
  map <- rast(nrows = 1, ncols = 5, vals = c(0, 1, 2, 3, NA))
  expect_equal(classValues(classifyQuietly(empty, map)), rep(NA_real_, 5))
})

test_that("classification is repeatable and independent of row order", {
  map <- rast(nrows = 1, ncols = 6, vals = 1:6)
  d <- data.table(
    pixelGroup = rep(1:6, each = 2),
    speciesCode = factor(rep(c("Pinu_ban", "Popu_tre"), 6)),
    B = c(90, 10, 60, 40, 10, 90, 40, 60, 20, 80, 70, 30))
  first <- classValues(classifyQuietly(copy(d), map))
  # classifyStand() works on its own copy, so calling it again on the same table gives the
  # same result (the table is also checked for modification in a test below).
  reused <- copy(d)
  invisible(classifyQuietly(reused, map))
  expect_equal(classValues(classifyQuietly(reused, map)), first)
  # Shuffling the cohort rows must not change the classes.
  expect_equal(classValues(classifyQuietly(d[c(12:1)], map)), first)
})

# classifyStand() works on its own copy of the cohort columns it needs, so the table it
# receives keeps its columns, values, key and memory address.
test_that("classifyStand() does not modify the cohortData it receives", {
  map <- rast(nrows = 1, ncols = 4, vals = 1:4)
  d <- data.table(
    pixelGroup = 1:4, speciesCode = factor(c("Pinu_ban", "Lari_lar", "Pice_mar", "Popu_tre")),
    B = 100, age = c(10L, 20L, 30L, 40L))
  setkey(d, pixelGroup)
  before <- copy(d)
  addressBefore <- data.table::address(d)
  invisible(classifyQuietly(d, map))
  # Same columns, values, order, key and object: nothing was added, changed or reallocated.
  expect_identical(names(d), names(before))
  expect_identical(d, before)
  expect_identical(key(d), "pixelGroup")
  expect_identical(data.table::address(d), addressBefore)
  expect_false("speciesNb" %in% names(d))
})

test_that("an existing speciesNb column is left as it is and does not affect the result", {
  map <- rast(nrows = 1, ncols = 4, vals = 1:4)
  d <- data.table(
    pixelGroup = 1:4, speciesCode = factor(c("Pinu_ban", "Lari_lar", "Pice_mar", "Popu_tre")),
    B = 100)
  clean <- classValues(classifyQuietly(d, map))
  withNb <- copy(d)
  # A stale column with values that would change the classes if it were used.
  withNb[, speciesNb := c(5, 5, 5, 5)]
  before <- copy(withNb)
  expect_equal(classValues(classifyQuietly(withNb, map)), clean)
  expect_identical(withNb, before)
})

test_that("a plain data.frame is accepted and left unchanged", {
  map <- rast(nrows = 1, ncols = 2, vals = 1:2)
  df <- data.frame(pixelGroup = 1:2, speciesCode = factor(c("Pinu_ban", "Popu_tre")), B = 100)
  expect_equal(classValues(classifyQuietly(df, map)), c(4, 1))
  expect_identical(names(df), c("pixelGroup", "speciesCode", "B"))
})

test_that("the cohortData of a simulation is not modified by the module", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    inputs <- spruceInputs()
    before <- copy(inputs$cohortData)
    sim <- SpaDES.core::spades(SpaDES.core::simInit(
      modules = moduleName, times = list(start = 0, end = 0, timeunit = "year"),
      params = list(WB_HartJohnstoneForestClasses = list(useDrainage = FALSE)),
      objects = inputs, paths = simPaths("cohortdata-untouched", clean = TRUE)))
    expect_identical(sim$cohortData, before)
    expect_false("speciesNb" %in% names(sim$cohortData))
  })
})

test_that("custom species prefixes drive the groups, including empty vectors", {
  map <- rast(nrows = 1, ncols = 4, vals = 1:4)
  d <- data.table(
    pixelGroup = 1:4,
    speciesCode = factor(c("Pinu_ban", "Lari_lar", "Pice_mar", "Abie_bal")), B = 100)
  expect_equal(classValues(classifyQuietly(d, map)), c(4, 5, 6, 3))
  # Empty vectors match nothing, so all four groups fall to the non-conifer residual (deci).
  expect_equal(classValues(classifyQuietly(
    d, map, jackPineSp = character(), larchSp = character(),
    spruceSp = character(), otherConiferSp = character())), rep(1, 4))
  # Using the whole pine genus as jack pine.
  expect_equal(classValues(classifyQuietly(
    data.table(pixelGroup = 1L, speciesCode = factor("Pinu_con"), B = 100),
    rast(nrows = 1, ncols = 1, vals = 1), jackPineSp = "Pinu")), 4)
})

test_that("invalid inputs are rejected", {
  forest <- rast(nrows = 1, ncols = 1, vals = 1)
  d <- data.table(pixelGroup = 1L, speciesCode = factor("Pice_mar"), B = 100)
  asCharacter <- copy(d)
  asCharacter$speciesCode <- as.character(asCharacter$speciesCode)
  expect_error(classifyQuietly(asCharacter, forest), "factor")
  expect_error(classifyQuietly(d, NULL), "Raster or SpatRaster")
  # Every species parameter validates its prefixes.
  for (parameter in c("jackPineSp", "larchSp", "spruceSp", "otherConiferSp")) {
    expect_error(do.call(classifyQuietly, c(list(d, forest), setNames(list(NA_character_), parameter))),
                 parameter)
  }
})
