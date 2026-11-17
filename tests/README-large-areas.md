# Large-area end-to-end tests

Routine `source("tests/run-tests.R")` runs the fast suite and skips the ten-area batch.
To include the batch, set `Sys.setenv(HART_RUN_LARGE_TESTS = "true")` before sourcing
that runner. Unset the variable afterward with `Sys.unsetenv("HART_RUN_LARGE_TESTS")`.

For the batch alone, source `tests/testTenLargeAreas.R`. Paths are resolved relative
to the sourced script, not the working directory. Load `large-area-functions.R`
instead when only function definitions are wanted; it never starts simulations.

Prerequisites: the existing project package library, all four local modules, local
SCANFI species-presence and wetland rasters, and a Google Drive token authorized via
`googledrive::drive_auth()` in the current session. No packages are installed.
The batch can download large input files and boundaries; network access and enough
disk space are required. Shared inputs, outputs, and cache must be writable.
Outputs and cache are retained under the project; scratch defaults to
`scratch/ten_area_tests`. Override with options `tenAreas.projectRoot` and
`tenAreas.scratchRoot`. Expect a lengthy run of ten 100-year simulations.

The default seeds use the successful replacement for area 2. No separate regression
case or polygon artifact for the discarded area is created. Existing historical
outputs are not deleted.

Both entry points fail if any area fails, after recording all area outcomes in
`results.csv`. Validation checks distinct area IDs, nonempty area histories,
classification years against completed events (including years 0 and 100), the final
summary event, and required CSV, validation RDS, and PNG artifacts.
