# Helpers shared by the golden regression test and by tests/make-golden-fixture.R, so that the
# fixture is built and checked by exactly the same code.
#
# A golden "window" is a small piece of a real snapshot (cohort rows plus the pixel group ids
# of a size x size block of cells). A golden "run" is one classification of a window with a
# given set of parameters; it optionally relabels species and optionally uses a drainage map.

# Builds the classifier inputs of one run: cohort table, pixel group raster and drainage map.
goldenInputs <- function(fixture, run) {
  window <- fixture$windows[[run$window]]
  size <- window$size
  cohorts <- copy(window$cohortData)

  # Relabelling swaps species codes (e.g. Pinu_con -> Pinu_ban). The snapshots hold no larch
  # or jack pine, so this is how real biomass shares reach the larch and jack pine rules.
  if (!is.null(run$relabel)) {
    codes <- as.character(cohorts$speciesCode)
    hit <- codes %in% names(run$relabel)
    codes[hit] <- unname(run$relabel[codes[hit]])
    cohorts[, speciesCode := factor(codes)]
  }

  # The window raster has no georeference: only its ids and shape matter to the classifier.
  map <- rast(nrows = size, ncols = size, xmin = 0, xmax = size, ymin = 0, ymax = size,
              vals = window$pixelGroupValues)

  # The drainage map alternates 1 (poorly drained) and 2 (well drained) from cell to cell, so
  # that spruce cells of both kinds always occur.
  drainage <- NULL
  if (isTRUE(run$drainage)) {
    drainage <- rast(map)
    values(drainage) <- rep(1:2, length.out = ncell(map))
  }
  list(cohorts = cohorts, map = map, drainage = drainage)
}

# Runs one golden run through `classifier` (a function with the classifyStand() interface) and
# returns what is frozen: class values, number of legend classes, and the CSV (hash and lines).
runGoldenCase <- function(fixture, run, classifier) {
  inputs <- goldenInputs(fixture, run)
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  arguments <- c(
    list(cohortData = inputs$cohorts, pixelGroupMap = inputs$map, classificationTablePath = csv),
    run$parameters)
  if (!is.null(inputs$drainage)) arguments$drainageMap <- inputs$drainage
  result <- do.call(classifier, arguments)
  list(values = as.vector(values(result)),
       nLevels = nrow(levels(result)[[1]]),
       csvHash = unname(tools::md5sum(csv)),
       csvLines = readLines(csv))
}
