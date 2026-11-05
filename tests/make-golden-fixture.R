# Development helper (not run by the test suite): rebuilds the golden fixture used by
# tests/testthat/test-golden-snapshot.R from real simulation snapshots.
#
# The fixture freezes the classifier's current output on many real-data cases, so that any
# later code change that alters results on identical input fails a test. The expected values
# are produced by the module code itself: they guard against regressions, they are not an
# independent validation of the classification rules.
#
# What it contains
#  - Windows: for each snapshot year (0, 50, 100), the most class-diverse size x size window
#    and the most spruce-rich window.
#  - Runs on every window: default parameters, all pines as jack pine, spruce limited to
#    Pice_mar, no additional conifers, and default parameters with a drainage map.
#  - Relabelled runs on every window: lodgepole pine relabelled as jack pine, and spruce
#    relabelled as larch, because the snapshots contain no jack pine or larch.
#
# Usage (from a session that has data.table and terra):
#   source("tests/make-golden-fixture.R")
#   makeGoldenFixture(snapshotDir = "<...>/enlarged-snapshots-20261103-092051", moduleDir = ".")
makeGoldenFixture <- function(snapshotDir, moduleDir, years = c(0L, 50L, 100L), size = 30L) {
  suppressPackageStartupMessages({library(data.table); library(terra)})
  # Load the classifier and the shared golden helpers into a private environment so that
  # nothing leaks into the global one.
  env <- new.env(parent = globalenv())
  sys.source(file.path(moduleDir, "R", "WB_HartJohnstoneForestClasses.r"), env)
  sys.source(file.path(moduleDir, "tests", "testthat", "helper-golden.R"), env)
  classifier <- function(...) suppressMessages(env$classifyStand(...))

  windows <- list()
  for (year in years) {
    folder <- file.path(snapshotDir, sprintf("year_%03d", year))
    cohorts <- readRDS(file.path(folder, "cohortData.rds"))
    groups <- rast(file.path(folder, "pixelGroupMap.tif"))
    nr <- nrow(groups)
    nc <- ncol(groups)
    groupValues <- as.vector(values(groups))

    # Classify the whole map once (default parameters) to rank candidate windows by content.
    classes <- as.vector(values(classifier(copy(cohorts), groups)))

    # Candidate windows tile the map without overlap.
    corners <- expand.grid(row = seq(1L, nr - size + 1L, by = size),
                           col = seq(1L, nc - size + 1L, by = size))
    # Row-major cell numbers of the window that starts at (row, col).
    cellsOf <- function(row, col) {
      as.vector(t(outer((row + seq_len(size) - 2L) * nc, col + seq_len(size) - 1L, "+")))
    }
    score <- t(vapply(seq_len(nrow(corners)), function(i) {
      cls <- classes[cellsOf(corners$row[i], corners$col[i])]
      c(kinds = length(unique(cls[!is.na(cls)])), cells = sum(!is.na(cls)),
        spruce = sum(cls == 6, na.rm = TRUE))
    }, numeric(3)))

    # Most class-diverse window (ties: most classified cells), then the most spruce-rich of the
    # other windows that are mostly classified.
    diverse <- order(-score[, "kinds"], -score[, "cells"])[1]
    candidates <- setdiff(which(score[, "cells"] >= 600), diverse)
    spruceRich <- candidates[order(-score[candidates, "spruce"])[1]]

    picks <- c(diverse = diverse, spruceRich = spruceRich)
    # Loop over the names: a for loop over a named vector would drop them.
    for (kind in names(picks)) {
      pick <- picks[[kind]]
      cells <- cellsOf(corners$row[pick], corners$col[pick])
      window <- list(
        # Only the cohorts of the pixel groups present in the window, and only the columns the
        # classifier uses, to keep the fixture small.
        cohortData = cohorts[pixelGroup %in% unique(groupValues[cells][!is.na(groupValues[cells])]),
                             .(pixelGroup, speciesCode, B)],
        pixelGroupValues = groupValues[cells], size = size,
        source = list(snapshot = basename(snapshotDir), year = year, kind = kind,
                      firstRow = corners$row[pick], firstCol = corners$col[pick]))
      windows[[sprintf("year%03d-%s", year, kind)]] <- window
    }
  }

  parameterSets <- list(
    defaults = list(),
    allPine = list(jackPineSp = "Pinu"),
    spruceMarOnly = list(spruceSp = "Pice_mar"),
    noOtherConifer = list(otherConiferSp = character())
  )
  relabels <- list(
    pineToJack = c(Pinu_con = "Pinu_ban"),
    spruceToLarch = c(Pice_eng_gla = "Lari_lar", Pice_mar = "Lari_lar")
  )

  # Each run is computed with the same helper the test uses, then frozen.
  fixture <- list(windows = windows, runs = list())
  addRun <- function(window, name, parameters = list(), drainage = FALSE, relabel = NULL) {
    run <- list(label = paste(window, name, sep = " / "), window = window,
                parameters = parameters, drainage = drainage, relabel = relabel)
    result <- env$runGoldenCase(fixture, run, classifier)
    run$expected <- list(values = result$values, nLevels = result$nLevels,
                         csvHash = result$csvHash)
    # Keep the full CSV text of the default runs, so a failure shows which lines changed.
    if (name == "defaults") run$csvLines <- result$csvLines
    fixture$runs[[length(fixture$runs) + 1L]] <<- run
  }
  for (window in names(windows)) {
    for (name in names(parameterSets)) addRun(window, name, parameterSets[[name]])
    addRun(window, "defaultsWithDrainage", drainage = TRUE)
    for (name in names(relabels)) addRun(window, name, relabel = relabels[[name]])
  }
  fixture$built <- as.character(Sys.Date())

  target <- file.path(moduleDir, "tests", "testthat", "fixtures", "golden-snapshot.rds")
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  saveRDS(fixture, target, version = 2)
  invisible(target)
}
