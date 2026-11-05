# Helpers shared by the simulation tests (test-module-events.R, test-module-drainage.R,
# test-module-cache.R and test-module-coupling.R). Added because those files need the same
# simInit()/spades() plumbing; testthat only shares what lives in helper files.

moduleName <- "WB_HartJohnstoneForestClasses"

# Folders for one simulation, kept under the disposable test directory. Pass clean = TRUE
# once, when building the simulation, to start from an empty folder; later calls only
# return the same paths so the outputs can be read back.
simPaths <- function(name, clean = FALSE, modulePath = dirname(moduleDir)) {
  root <- file.path(scratch, name)
  if (clean) unlink(root, recursive = TRUE)
  list(modulePath = modulePath, inputPath = file.path(root, "inputs"),
       outputPath = file.path(root, "outputs"), cachePath = file.path(root, "cache"))
}

# Runs code with SpaDES package installation and downloads switched off (offline-safe).
withSpades <- function(code) {
  oldOptions <- options(spades.useRequire = FALSE, Require.install = FALSE,
                        Require.offlineMode = TRUE)
  on.exit(options(oldOptions))
  force(code)
}

# Events of this module that were completed, as a data frame (optionally of one type).
moduleEvents <- function(sim, type = NULL) {
  done <- as.data.frame(SpaDES.core::completed(sim))
  done <- done[done$moduleName == moduleName, ]
  if (!is.null(type)) done <- done[done$eventType == type, ]
  done
}

# Four pixel groups holding spruce, spruce, jack pine and poplar: with the default species
# prefixes they classify as 6, 6, 4 and 1 (spruce, spruce, jackpine, deci).
spruceInputs <- function() list(
  cohortData = data.table(pixelGroup = 1:4,
                          speciesCode = factor(c("Pice_mar", "Pice_mar", "Pinu_ban", "Popu_tre")),
                          B = 100),
  pixelGroupMap = rast(nrows = 1, ncols = 4, vals = 1:4))

# A drainage map for spruceInputs(): poorly drained (1) then well drained (2) for the two
# spruce cells. The original module turns spruce cells with drainage 2 into class 7.
drainageMapFor <- function() rast(nrows = 1, ncols = 4, vals = c(1, 2, 2, 1))

# Copies the module's code (not its .git folder or tests) into `modulePath`, so that a
# simulation can load it next to another module such as the synthetic Biomass_core.
copyModuleInto <- function(modulePath) {
  target <- file.path(modulePath, moduleName)
  dir.create(file.path(target, "R"), recursive = TRUE, showWarnings = FALSE)
  copied <- c(
    file.copy(file.path(moduleDir, paste0(moduleName, ".R")), target, overwrite = TRUE),
    file.copy(list.files(file.path(moduleDir, "R"), full.names = TRUE),
              file.path(target, "R"), overwrite = TRUE))
  stopifnot(all(copied))
  invisible(target)
}
