# Golden-output regression test. The fixture (fixtures/golden-snapshot.rds, rebuilt with
# tests/make-golden-fixture.R) holds windows of real simulation snapshots (years 0, 50 and
# 100) together with the classifier's output on them for many parameter sets, relabelled
# species and a drainage map. Any code change that alters the result on identical input fails
# here. The expected values were produced by the module itself, so this guards against
# regressions; it does not validate the classification rules.

fixturePath <- file.path(moduleDir, "tests", "testthat", "fixtures", "golden-snapshot.rds")

test_that("every golden run still classifies exactly as recorded", {
  skip_if_not(file.exists(fixturePath), "golden fixture is missing")
  fixture <- readRDS(fixturePath)
  expect_gte(length(fixture$runs), 40L)

  for (run in fixture$runs) {
    result <- runGoldenCase(fixture, run, classifyQuietly)
    # Identical, not merely equal: class codes must not drift at all.
    expect_identical(result$values, run$expected$values, info = run$label)
    expect_identical(result$nLevels, run$expected$nLevels, info = run$label)
    # For the runs that kept the CSV text, compare line by line first: a failure then shows
    # the lines that changed. The hash covers every run, including formatting changes.
    if (!is.null(run$csvLines)) {
      expect_identical(result$csvLines, run$csvLines, info = run$label)
    }
    expect_identical(result$csvHash, run$expected$csvHash, info = run$label)
  }
})

test_that("the golden runs together exercise every class and the drainage split", {
  skip_if_not(file.exists(fixturePath), "golden fixture is missing")
  fixture <- readRDS(fixturePath)
  # Guard against a fixture that became trivial: all six classes must occur somewhere, and
  # class 7 (the drainage split of spruce) must occur in the drainage runs.
  allClasses <- unique(unlist(lapply(fixture$runs, function(run) run$expected$values)))
  expect_true(all(1:6 %in% allClasses))
  drainageClasses <- unique(unlist(lapply(
    Filter(function(run) isTRUE(run$drainage), fixture$runs),
    function(run) run$expected$values)))
  expect_true(7 %in% drainageClasses)
  # Both kinds of window and all three snapshot years are represented.
  sources <- lapply(fixture$windows, `[[`, "source")
  expect_setequal(vapply(sources, function(s) s$year, numeric(1)), c(0, 50, 100))
  expect_setequal(vapply(sources, function(s) s$kind, character(1)), c("diverse", "spruceRich"))
})
