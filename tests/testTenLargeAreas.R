# Source this entry point to run all ten areas. See README-large-areas.md for prerequisites.
# This launches the batch directly; HART_RUN_LARGE_TESTS controls only the testthat wrapper.
# Authenticate with googledrive::drive_auth() first. Input downloads may be large.
# Sampling, simulation, and output validation are defined in large-area-functions.R.

# Keep runner variables and loaded helper functions out of the global environment.
local({
  # Locate this sourced file rather than relying on the console's working directory.
  # The last source frame identifies this entry point even when another script sources it.
  sourceFiles <- Filter(function(path) is.character(path) && length(path) == 1L,
                        lapply(sys.frames(), function(frame) frame$ofile))
  if (!length(sourceFiles)) stop("Run this file with source().")
  testDir <- normalizePath(dirname(tail(sourceFiles, 1L)[[1L]]), winslash = "/")
  # Default layout: <project>/modules/<module>/tests. An option supports other layouts.
  project <- getOption("tenAreas.projectRoot", dirname(dirname(dirname(testDir))))
  # Prefer installed project packages for this run, then restore the caller's library paths.
  oldLibrary <- .libPaths()
  on.exit(.libPaths(oldLibrary), add = TRUE)
  .libPaths(c(file.path(project, "packages", "4.5"), oldLibrary))
  # Loading definitions alone does not start simulations or authenticate with Google Drive.
  sys.source(file.path(testDir, "large-area-functions.R"), envir = environment())
  # Scratch holds intermediate files; results and caches use separate project directories.
  # Set options(tenAreas.scratchRoot = "...") to use a different scratch location.
  scratch <- getOption("tenAreas.scratchRoot", file.path(project, "scratch", "ten_area_tests"))
  dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
  # Run all ten areas, including the default replacement for area 2. Outcomes are recorded
  # in results.csv; validation raises an error if any area fails or required outputs are invalid.
  runTenLargeAreas(project, scratch)
})
