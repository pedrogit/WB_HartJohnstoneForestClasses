# Loads the module code so the tests run without simInit, from any working directory.
# Added: the original had no loader, its tests assumed classifyStand() was already defined.
moduleDir <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
sys.source(file.path(moduleDir, "R", "WB_HartJohnstoneForestClasses.r"), envir = environment())
library(data.table)
library(terra)

# Disposable directory for test outputs. Set HART_TEST_OUTPUT_DIR to choose it; otherwise a
# session temporary directory is used.
scratch <- Sys.getenv("HART_TEST_OUTPUT_DIR")
if (!nzchar(scratch)) scratch <- file.path(tempdir(), "original-module-tests")
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)

# classifyStand() with its console messages silenced, to keep test output readable.
classifyQuietly <- function(...) suppressMessages(classifyStand(...))

# Class codes of a classified raster as a plain vector.
classValues <- function(result) as.vector(values(result))
