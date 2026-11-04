library(data.table)
library(terra)
library(testthat)

classify_example <- function(species, biomass, ...) {
  cohorts <- data.table(
    pixelGroup = 1L,
    speciesCode = factor(species),
    B = biomass
  )
  map <- rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  values(map) <- 1L
  result <- suppressMessages(classifyStand(cohorts, map, ...))
  as.integer(values(result)[1])
}

test_that("additional conifers retain distinct subtype precedence", {
  expect_identical(classify_example("Abie_las", 100), 3L)
  expect_identical(classify_example("Pinu_con", 100), 3L)
  expect_identical(classify_example("Pinu_ban", 100), 4L)
  expect_identical(classify_example("Lari_lar", 100), 5L)
  expect_identical(classify_example("Pice_mar", 100), 6L)
  expect_identical(classify_example("Popu_tre", 100), 1L)
  expect_identical(classify_example(c("Pice_mar", "Abie_las"), c(60, 40)), 3L)
})

test_that("overlapping prefixes count biomass only once", {
  expect_identical(
    classify_example(c("Pinu_ban", "Abie_las", "Popu_tre"), c(40, 20, 40)),
    2L
  )
  expect_identical(
    classify_example(
      c("Pinu_con", "Popu_tre"),
      c(40, 60),
      otherConiferSp = c("Pinu_", "Pinu_con", "Pinu_")
    ),
    2L
  )
})

test_that("custom and empty additional conifer vectors are respected", {
  expect_identical(
    classify_example("Abie_las", 100, otherConiferSp = character(0)),
    1L
  )
  expect_identical(
    classify_example("Pinu_ban", 100, otherConiferSp = character(0)),
    4L
  )
  expect_identical(
    classify_example("Abie_las", 100, otherConiferSp = "ABIE_"),
    3L
  )
  expect_identical(
    classify_example("Abie_las", 100, otherConiferSp = "Abie_."),
    1L
  )
  expect_identical(classify_example(c("Lari_lar", "Popu_tre"), c(75, 25)), 5L)
  expect_identical(classify_example(c("Lari_lar", "Popu_tre"), c(70, 30)), 3L)
})

test_that("invalid additional conifer prefixes fail before classification", {
  for (prefixes in list(1, NA_character_, "", " ", " Abie_")) {
    error <- tryCatch(
      classify_example("Abie_las", 100, otherConiferSp = prefixes),
      error = identity
    )
    expect_s3_class(error, "error")
    expect_match(conditionMessage(error), "otherConiferSp")
  }
})
