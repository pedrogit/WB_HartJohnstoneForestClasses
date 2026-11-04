test_that("genus defaults omit underscores without changing standard code matches", {
  prefixes <- eval(formals(classifyStand)$otherConiferSp)
  expect_identical(
    prefixes,
    c(
      "Abie",
      "Calc",
      "Cham",
      "Juni",
      "Pinu",
      "Pseu",
      "Sequ",
      "Taxu",
      "Thuj",
      "Tsug"
    )
  )
  cohorts <- data.table(speciesCode = factor(paste0(prefixes, "_example")))
  expect_identical(
    getCohortSpeciesFactors(cohorts, prefixes),
    getCohortSpeciesFactors(cohorts, paste0(prefixes, "_"))
  )
  expect_identical(eval(formals(classifyStand)$jackPineSp), "Pinu_ban")
})

test_that("species matching is literal, anchored and case insensitive", {
  cohorts <- data.table(
    speciesCode = factor(c(
      "Pice_mar",
      "PICE_gla",
      "XPice_mar",
      "Pice.*literal",
      "Pinu_ban",
      NA
    ))
  )
  matched <- function(prefixes) {
    as.character(getCohortSpeciesFactors(cohorts, prefixes))
  }
  expect_identical(matched("pice"), c("Pice_mar", "PICE_gla", "Pice.*literal"))
  expect_identical(matched("Pice.*"), "Pice.*literal")
  expect_identical(matched("^Pice"), character(0))
  expect_identical(matched("Pice|Pinu"), character(0))
  expect_identical(matched(character(0)), character(0))
  expect_identical(matched(c("Pice", "Pice_", "Pice")), matched("Pice"))
})

test_that("every species parameter shares literal matching and validation", {
  cohorts <- data.table(
    pixelGroup = 1L,
    speciesCode = factor("Pice_mar"),
    B = 100
  )
  map <- rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  values(map) <- 1L
  parameters <- c("jackPineSp", "larchSp", "spruceSp", "otherConiferSp")
  empty <- setNames(rep(list(character(0)), 4), parameters)
  classify <- function(groups) {
    result <- suppressMessages(do.call(
      classifyStand,
      c(
        list(cohortData = copy(cohorts), pixelGroupMap = map),
        groups
      )
    ))
    as.integer(values(result)[1])
  }
  expect_identical(classify(empty), 1L)
  for (i in seq_along(parameters)) {
    groups <- empty
    groups[[i]] <- "pICE"
    expect_identical(classify(groups), c(4L, 5L, 6L, 3L)[i])
    groups[[i]] <- "Pice.*"
    expect_identical(classify(groups), 1L)
    for (invalid in list(1, NA_character_, "", " ", " Pice", "Pice ")) {
      groups[[i]] <- invalid
      error <- tryCatch(classify(groups), error = identity)
      expect_s3_class(error, "error")
      expect_match(conditionMessage(error), parameters[i], fixed = TRUE)
    }
  }
})
