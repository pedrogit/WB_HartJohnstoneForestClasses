# Tests added for the classification summary CSV and the optional drainage refinement of
# classifyStand().

test_that("the CSV has the expected columns, rows and truncated percentages", {
  map <- rast(nrows = 1, ncols = 3, vals = 1:3)
  d <- data.table(
    pixelGroup = c(1, 1, 2, 3, 3),
    speciesCode = factor(c("Pinu_ban", "Pinu_con", "Lari_lar", "Pice_mar", "Popu_tre")),
    B = c(699, 300, 100, 40, 60))
  path <- file.path(scratch, "csv", "summary.csv")
  # The folder does not exist yet: classifyStand() is expected to create it.
  unlink(dirname(path), recursive = TRUE)
  expect_false(dir.exists(dirname(path)))
  invisible(classifyQuietly(d, map, classificationTablePath = path))
  expect_true(file.exists(path))
  summary <- read.csv(path, check.names = FALSE)
  # The first, unnamed column is the row number written by write.csv().
  expect_identical(names(summary), c("", "pixelGroup", "B", "jackPinePct", "larchPct",
                                     "sprucePct", "otherConiferPct", "coniferPct",
                                     "classInt", "classText"))
  expect_equal(summary$pixelGroup, 1:3)
  expect_equal(summary$B, c(999, 100, 100))
  # 699/999 = 69.97% -> 69 (truncated; rounding would give 70) and 300/999 = 30.03% -> 30.
  # Both pines are conifers, so the conifer share is exactly 100%.
  expect_equal(summary$jackPinePct[1], 69L)
  expect_equal(summary$otherConiferPct[1], 30L)
  expect_equal(summary$coniferPct[1], 100L)
  # Group 3 is 40% spruce and 60% deciduous: neither dominates, so it is mixed.
  expect_equal(summary$sprucePct[3], 40L)
  expect_equal(summary$classInt, c(4L, 5L, 2L))
  expect_equal(summary$classText, c("jackpine", "larch", "mixed"))
})

test_that("no CSV is written when no path is given", {
  before <- list.files(scratch, recursive = TRUE)
  invisible(classifyQuietly(
    data.table(pixelGroup = 1L, speciesCode = factor("Pice_mar"), B = 100),
    rast(nrows = 1, ncols = 1, vals = 1)))
  expect_identical(list.files(scratch, recursive = TRUE), before)
})

test_that("drainage only splits the spruce class and leaves other cells unchanged", {
  map <- rast(nrows = 1, ncols = 4, vals = 1:4)
  d <- data.table(
    pixelGroup = 1:4,
    speciesCode = factor(c("Pice_mar", "Pice_mar", "Pinu_ban", "Popu_tre")), B = 100)
  drainage <- rast(nrows = 1, ncols = 4, vals = c(1, 2, 2, 1))
  without <- classValues(classifyQuietly(d, map))
  expect_equal(without, c(6, 6, 4, 1))
  with <- classifyQuietly(d, map, drainageMap = drainage)
  # WB_VegBasedDrainage codes 1 = poorly drained and 2 = well drained.
  # Cells that are not spruce keep their class, and well-drained spruce (2) stays class 6.
  expect_equal(classValues(with)[c(2, 3, 4)], c(6, 4, 1))
  # Poorly-drained spruce (1) moves to the extra class 7.
  expect_equal(classValues(with)[1], 7)
  # The legend gains a seventh class with its own colour, and the labels match the drainage coding.
  expect_equal(levels(with)[[1]]$ID, 1:7)
  expect_equal(levels(with)[[1]]$class[6:7], c("wd_spruce", "pd_spruce"))
  expect_equal(nrow(coltab(with)[[1]]), 7L)
  expect_identical(names(with), "standtype")
})

test_that("an all-NA or missing drainage map is ignored", {
  map <- rast(nrows = 1, ncols = 2, vals = 1:2)
  d <- data.table(pixelGroup = 1:2, speciesCode = factor(c("Pice_mar", "Popu_tre")), B = 100)
  noDrainage <- classifyQuietly(d, map)
  allNa <- classifyQuietly(d, map, drainageMap = init(map, NA_real_))
  expect_equal(classValues(allNa), classValues(noDrainage))
  expect_equal(nrow(levels(allNa)[[1]]), 6L)
})
