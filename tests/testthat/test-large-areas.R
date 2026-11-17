# Real-module end-to-end coverage: slow, network-enabled, and excluded from routine runs.
test_that("the ten real large-area simulations complete and produce outputs", {
  # Skip before loading the harness or creating directories; opt-in is explicit per session/job.
  skip_if(tolower(Sys.getenv("HART_RUN_LARGE_TESTS")) != "true",
          "Set HART_RUN_LARGE_TESTS=true to run the slow, network-enabled batch.")
  # moduleDir is provided by helper-module.R; use the same project layout as the normal runner.
  project <- getOption("tenAreas.projectRoot", dirname(dirname(moduleDir)))
  # Definitions load privately and do not invoke the directly runnable entry point.
  harness <- new.env(parent = globalenv())
  sys.source(file.path(moduleDir, "tests", "large-area-functions.R"), envir = harness)
  # Retain batch diagnostics outside the disposable directories used by small synthetic tests.
  scratchRoot <- getOption("tenAreas.scratchRoot", file.path(project, "scratch", "ten_area_tests"))
  dir.create(scratchRoot, recursive = TRUE, showWarnings = FALSE)
  # Authentication and local input prerequisites are enforced by the harness, not skipped here.
  result <- harness$runTenLargeAreas(project, scratchRoot)
  expect_true(harness$validateTenLargeAreas(result))
})
