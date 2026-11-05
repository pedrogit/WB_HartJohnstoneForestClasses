# Runs the original module's tests from any working directory, with the project package
# library. Usage: source("tests/run-tests.R")
# Set HART_TEST_OUTPUT_DIR to a disposable folder for test outputs; otherwise a session
# temporary folder is used.
local({
  sourceFiles <- Filter(function(path) is.character(path) && length(path) == 1L,
                        lapply(sys.frames(), function(frame) frame$ofile))
  if (!length(sourceFiles)) stop("Run this file with source().")
  testDir <- normalizePath(dirname(tail(sourceFiles, 1L)[[1L]]), winslash = "/")
  module <- dirname(testDir)
  project <- dirname(dirname(module))
  # Use the project's offline package library first.
  old <- .libPaths()
  on.exit(.libPaths(old))
  .libPaths(c(file.path(project, "packages", "4.5"), old))
  library(testthat)
  testthat::test_dir(file.path(testDir, "testthat"), stop_on_failure = TRUE)
})
