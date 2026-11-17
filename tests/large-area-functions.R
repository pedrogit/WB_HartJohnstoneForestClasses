# Function definitions only; testTenLargeAreas.R is the runnable entry point.
# Natural Earth's generalized boundaries define the sampling region, not legal borders.
# projectRoot supplies the local modules, shared inputs, and persistent output/cache paths.
# scratchRoot must exist; each area gets its own intermediate-file directory beneath it.
# Returns output_directory and a ten-row results table only after batch validation succeeds.
# Area errors are recorded and the remaining areas run before the batch raises an error.
runTenLargeAreas <- function(projectRoot, scratchRoot, seed = 6847L) {
  # Disable both automatic mirror registration and any remap from an earlier LandR load.
  # Restore the caller's download configuration when the batch finishes.
  downloadOptions <- options(LandR.scanfiMirror = FALSE, reproducible.urlRemap = NULL,
    timeout = max(3600, getOption("timeout")))
  on.exit(options(downloadOptions), add = TRUE)
  library(terra)
  library(data.table)
  library(SpaDES.core)
  library(LandR)
  library(ggplot2)
  # Require prior authentication so unattended test runs cannot wait on a browser prompt.
  if (!googledrive::drive_has_token()) {
    stop("Authenticate with googledrive::drive_auth() before running the large-area tests.")
  }
  projectRoot <- normalizePath(projectRoot, winslash = "/", mustWork = TRUE)
  stopifnot(dir.exists(scratchRoot), length(seed) == 1L)
  boundaryDir <- file.path(projectRoot, "inputs", "NaturalEarth_admin1")
  dir.create(boundaryDir, recursive = TRUE, showWarnings = FALSE)
  boundaryFile <- file.path(boundaryDir, "sampling_jurisdictions.gpkg")
  boundaryURL <- "https://naturalearth.s3.amazonaws.com/10m_cultural/ne_10m_admin_1_states_provinces.zip"
  jurisdictions <- c("CA-BC", "CA-AB", "CA-YT", "CA-NT", "CA-SK")
  # Reuse the jurisdiction subset across batches; record provenance when first downloaded.
  if (!file.exists(boundaryFile)) {
    archive <- file.path(scratchRoot, "ne_10m_admin_1_states_provinces.zip")
    if (!file.exists(archive)) utils::download.file(boundaryURL, archive, mode = "wb")
    unpacked <- file.path(scratchRoot, "ne_10m_admin_1_states_provinces")
    utils::unzip(archive, exdir = unpacked)
    boundaries <- terra::vect(file.path(unpacked, "ne_10m_admin_1_states_provinces.shp"))
    stopifnot("iso_3166_2" %in% names(boundaries))
    boundaries <- boundaries[boundaries$iso_3166_2 %in% jurisdictions, ]
    stopifnot(nrow(boundaries) == 5L, setequal(boundaries$iso_3166_2, jurisdictions))
    terra::writeVector(boundaries, boundaryFile)
    writeLines(c(paste("Source:", boundaryURL),
      "Natural Earth Admin 1, 1:10 million; public domain; generalized boundaries.",
      paste("Retrieved:", Sys.Date()), paste("Archive MD5:", tools::md5sum(archive))),
      file.path(boundaryDir, "SOURCE.txt"))
  }
  boundaries <- terra::vect(boundaryFile)
  stopifnot(setequal(boundaries$iso_3166_2, jurisdictions))
  runRoot <- file.path(projectRoot, "outputs", paste0("ten_large_areas_", format(Sys.time(), "%Y%m%d-%H%M%S")))
  dir.create(runRoot, recursive = TRUE, showWarnings = FALSE)
  # Lambert azimuthal equal-area coordinates make uniform sampling area-weighted.
  # Each square has 10,000 km2 projected area; geodesic area is recorded separately.
  equalArea <- "+proj=laea +lat_0=60 +lon_0=-110 +datum=NAD83 +units=m +no_defs"
  region <- terra::aggregate(terra::project(boundaries, equalArea))
  terra::writeVector(region, file.path(runRoot, "sampling_region.gpkg"))
  # Sampling uses the union of jurisdictions, not equal quotas per province or territory.
  set.seed(seed)
  areas <- NULL
  attempts <- 0L
  while (is.null(areas) || nrow(areas) < 10L) {
    attempts <- attempts + 1L
    if (attempts > 10000L) stop("Could not select ten fully contained, non-overlapping areas.")
    point <- terra::spatSample(region, size = 1L, method = "random")
    xy <- terra::crds(point)[1L, ]
    square <- terra::as.polygons(terra::ext(xy[1L] - 50000, xy[1L] + 50000,
                                         xy[2L] - 50000, xy[2L] + 50000), crs = equalArea)
    # Reject border-crossing and overlapping squares, but do not select for forest coverage.
    # Non-forest or unavailable-input outcomes are retained as test results.
    if (!isTRUE(terra::relate(square, region, "within")[1L, 1L])) next
    if (!is.null(areas) && any(terra::relate(square, areas, "intersects"))) next
    square$area_id <- if (is.null(areas)) 1L else nrow(areas) + 1L
    areas <- if (is.null(areas)) square else rbind(areas, square)
  }
  # Only the default sample substitutes area 2; custom seeds retain their original ten draws.
  # Draw against all original squares to preserve the other nine and avoid overlap.
  replacement <- NULL
  if (identical(as.integer(seed), 6847L)) {
    originalAreas <- areas
    set.seed(7931L)
    replacementAttempts <- 0L
    repeat {
      replacementAttempts <- replacementAttempts + 1L
      if (replacementAttempts > 10000L) stop("Could not sample a replacement area.")
      point <- terra::spatSample(region, size = 1L, method = "random")
      xy <- terra::crds(point)[1L, ]
      square <- terra::as.polygons(terra::ext(xy[1L] - 50000, xy[1L] + 50000,
                                           xy[2L] - 50000, xy[2L] + 50000), crs = equalArea)
      if (!isTRUE(terra::relate(square, region, "within")[1L, 1L])) next
      if (any(terra::relate(square, originalAreas, "intersects"))) next
      break
    }
    square$area_id <- 2L
    areas <- rbind(originalAreas[1L, ], square, originalAreas[3:10, ])
    # Record the substitution because this default batch is not an unmodified random sample.
    replacement <- list(area_id = 2L, seed = 7931L, attempts = replacementAttempts,
      reason = "Original area failed cohort initiation; replacement completed a prior 100-year run")
    saveRDS(replacement, file.path(runRoot, "area_02_replacement.rds"))
  }
  terra::writeVector(areas, file.path(runRoot, "selected_areas.gpkg"))
  centres <- terra::crds(terra::project(terra::centroids(areas), "EPSG:4326"))
  # Write the initial manifest before simulations so interrupted batches retain their selection.
  manifest <- data.frame(area_id = areas$area_id, longitude = centres[, 1L], latitude = centres[, 2L],
    projected_km2 = 10000, geodesic_km2 = terra::expanse(areas, unit = "km"),
    status = "pending", elapsed_seconds = NA_real_, error = "", stringsAsFactors = FALSE)
  write.csv(manifest, file.path(runRoot, "results.csv"), row.names = FALSE)
  saveRDS(list(seed = seed, boundary_source = boundaryURL, jurisdictions = jurisdictions,
    replacement = replacement,
    region_km2 = sum(terra::expanse(region, unit = "km")), attempts = attempts,
    sampling = "Uniform centre sampling in an equal-area projection; reject squares outside union or overlapping prior squares",
    duration = c(start = 0, end = 100)), file.path(runRoot, "protocol.rds"))

  # Evaluate only configuration arguments, never the setup script's installation/bootstrap code.
  expressions <- parse(file.path(projectRoot, "LichenBiomassFullAIProject", "setup_biomass_core_project.R"))
  setup <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
                    identical(x[[2L]], as.name("out")), as.list(expressions))
  stopifnot(length(setup) == 1L)
  configuration <- setup[[1L]][[3L]]
  parameters <- eval(configuration$params)
  # Fit from local inputs rather than cloud-cached statistics; retain final classifier summaries.
  parameters$Biomass_borealDataPrep <- modifyList(parameters$Biomass_borealDataPrep %||% list(),
    list(.useCloud = FALSE, useCloudCacheForStats = FALSE))
  parameters$WB_HartJohnstoneForestClasses$plotAndSaveFinalSummary <- TRUE
  times <- eval(configuration$times)
  modules <- c("Biomass_speciesData", "Biomass_borealDataPrep", "Biomass_core", "WB_HartJohnstoneForestClasses")
  oldOptions <- options(spades.useRequire = FALSE, Require.install = FALSE,
    Require.offlineMode = TRUE, Require.noRemotes = TRUE,
    reproducible.interactiveOnDownloadFail = FALSE)
  on.exit(options(oldOptions), add = TRUE)
  # Input downloads are allowed; automatic package installation remains disabled.
  blockTransfer <- function(...) stop("Package installation disabled for this test.", call. = FALSE)
  oldGuard <- getOption("tenAreas.blockTransfer")
  options(tenAreas.blockTransfer = blockTransfer)
  on.exit(options(tenAreas.blockTransfer = oldGuard), add = TRUE)
  guarded <- list(utils = "install.packages")
  # Remove only traces installed here, even if a simulation fails during initialization.
  traced <- list()
  on.exit(for (item in traced) untrace(item$fn, where = asNamespace(item$pkg)), add = TRUE)
  for (pkg in names(guarded)) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    for (fn in guarded[[pkg]]) {
      if (!exists(fn, asNamespace(pkg), inherits = FALSE)) next
      if (inherits(get(fn, asNamespace(pkg)), "functionWithTrace")) stop("Existing transfer trace: ", pkg, "::", fn)
      trace(fn, where = asNamespace(pkg), tracer = quote(getOption("tenAreas.blockTransfer")()), print = FALSE)
      traced[[length(traced) + 1L]] <- list(pkg = pkg, fn = fn)
    }
  }
  # CWIM's helper otherwise uses its default remote URL despite an existing local raster.
  wetland <- file.path(projectRoot, "inputs", "canadian-wetland-inventory-v3a-classification.tif")
  stopifnot(file.exists(wetland))
  # This namespace override is temporary and restored when the batch exits.
  originalCWIM <- get("prepInputs_CWIM", asNamespace("LandR"))
  localCWIM <- originalCWIM
  formals(localCWIM)$url <- wetland
  assignInNamespace("prepInputs_CWIM", localCWIM, ns = "LandR")
  on.exit(assignInNamespace("prepInputs_CWIM", originalCWIM, ns = "LandR"), add = TRUE)
  oldTerra <- terra::terraOptions(print = FALSE)
  on.exit(terra::terraOptions(tempdir = oldTerra$tempdir), add = TRUE)
  inputPath <- file.path(projectRoot, "inputs")
  presence <- terra::rast(file.path(inputPath, "SpeciesPresentInCanadianForests_SCANFIv2_2025.tif"))
  # Preserve the manifest for audit. Input preparation may register new downloads;
  # this script does not explicitly revise checksum baselines for existing inputs.
  file.copy(file.path(inputPath, "CHECKSUMS.txt"), file.path(runRoot, "CHECKSUMS_before.txt"))
  modelCRS <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +datum=NAD83 +units=m +no_defs"
  for (i in seq_len(10L)) {
    areaName <- sprintf("area_%02d", i)
    output <- file.path(runRoot, areaName)
    scratch <- file.path(scratchRoot, basename(runRoot), areaName)
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
    terra::terraOptions(tempdir = scratch)
    manifest$status[i] <- "running"
    write.csv(manifest, file.path(runRoot, "results.csv"), row.names = FALSE)
    warnings <- character()
    started <- proc.time()[["elapsed"]]
    # Per-area output/cache paths prevent cross-run overwrites and isolate model refits.
    # Raw input files remain shared, and their checksums are not edited by this script.
    result <- tryCatch(withCallingHandlers({
      # Each area's random stream is independent of how far preceding simulations progressed.
      set.seed(seed + i)
      studyArea <- terra::project(areas[i, ], modelCRS)
      # The 30 km buffer supplies surrounding data for parameterization and species discovery.
      buffered <- terra::buffer(studyArea, width = 30000)
      species <- LandR::speciesInStudyArea(studyArea = buffered, speciesPresentRas = presence,
        dataSource = "SCANFI", dataYear = 2025, dPath = inputPath)
      if (!length(species$speciesList)) stop("No species available in local presence data.")
      sim <- SpaDES.core::simInit(modules = modules, times = times, params = parameters,
        objects = list(studyArea = studyArea, studyAreaLarge = buffered,
          studyArea_biomassParam = buffered, sppNameVector = species$speciesList),
        paths = list(modulePath = file.path(projectRoot, "modules"), inputPath = inputPath,
          outputPath = output, cachePath = file.path(projectRoot, "cache", basename(runRoot), areaName),
          scratchPath = scratch))
      sim <- suppressMessages(SpaDES.core::spades(sim))
      # Completed events establish what actually ran, rather than what was merely scheduled.
      events <- as.data.frame(SpaDES.core::completed(sim))
      final <- events[events$moduleName == "WB_HartJohnstoneForestClasses" &
                        events$eventType == "plotAndSaveFinalSummary", ]
      stopifnot(nrow(final) == 1L, final$eventTime == times$end)
      history <- sim$WB_HartJohnstoneForestClassesAreaHistory
      expectedYears <- events$eventTime[events$moduleName == "WB_HartJohnstoneForestClasses" &
        events$eventType == "reComputeHJForestClassesMap"]
      stopifnot(nrow(history) > 0L, length(expectedYears) > 0L,
        setequal(unique(history$year), expectedYears),
        all(history$area_ha >= 0), all(is.finite(history$area_ha)))
      # Persist compact validation artifacts without retaining the full simulation in memory.
      write.csv(history, file.path(output, "class_area_history.csv"), row.names = FALSE)
      write.csv(events, file.path(output, "completed_events.csv"), row.names = FALSE)
      saveRDS(list(map_dimensions = dim(sim$pixelGroupMap), cohort_rows = nrow(sim$cohortData),
        classification_years = sort(unique(history$year))), file.path(output, "validation.rds"))
      "completed"
    }, warning = function(w) {
      # Keep warnings for diagnosis without flooding the console during the long batch.
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) {
      # A failed area must not prevent collection of outcomes for the remaining areas.
      manifest$error[i] <<- conditionMessage(e)
      "failed"
    })
    # Save progress after every area; earlier outcomes survive a later interruption.
    manifest$status[i] <- result
    manifest$elapsed_seconds[i] <- proc.time()[["elapsed"]] - started
    writeLines(unique(warnings), file.path(output, "warnings.txt"))
    write.csv(manifest, file.path(runRoot, "results.csv"), row.names = FALSE)
    if (exists("sim", inherits = FALSE)) rm(sim)
    gc()
  }
  result <- list(output_directory = runRoot, results = manifest)
  # Converting recorded failures into an error makes both entry points enforce a pass/fail result.
  validateTenLargeAreas(result)
  result
}

# A local fallback avoids requiring an additional package solely for this operator.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Validate the manifest and persisted outputs without rerunning simulations.
# This contract is for the ten-area, 0--100-year batch; it is not a general simulation validator.
# Raises an error identifying the failed area or missing artifact; returns invisible TRUE on success.
validateTenLargeAreas <- function(result) {
  manifest <- result$results
  if (nrow(manifest) != 10L || !setequal(manifest$area_id, seq_len(10L)))
    stop("Expected exactly ten distinct area results.")
  # Report recorded simulation errors before attempting to open artifacts from failed areas.
  failed <- is.na(manifest$status) | manifest$status != "completed"
  if (any(failed)) {
    stop("Large-area failures (", result$output_directory, "): ",
      paste(paste0(manifest$area_id[failed], ": ", manifest$error[failed]), collapse = "; "))
  }
  for (i in seq_len(10L)) {
    output <- file.path(result$output_directory, sprintf("area_%02d", i))
    required <- file.path(output, c("class_area_history.csv", "completed_events.csv", "validation.rds"))
    if (!all(file.exists(required)) || any(file.info(required)$size == 0))
      stop("Missing or empty output artifacts for area ", i)
    # Cross-check history years and saved metadata against completed classification events.
    # Nonempty data and endpoint checks prevent vacuous success from all(logical(0)).
    history <- read.csv(required[1L])
    events <- read.csv(required[2L])
    final <- events[events$moduleName == "WB_HartJohnstoneForestClasses" &
      events$eventType == "plotAndSaveFinalSummary", ]
    expected <- events$eventTime[events$moduleName == "WB_HartJohnstoneForestClasses" &
      events$eventType == "reComputeHJForestClassesMap"]
    validation <- readRDS(required[3L])
    if (nrow(final) != 1L || final$eventTime != 100 || !length(expected) ||
        !all(c(0, 100) %in% expected) || nrow(history) == 0L ||
        !setequal(history$year, expected) ||
        !setequal(validation$classification_years, expected) || any(!is.finite(history$area_ha)) ||
        any(history$area_ha < 0)) stop("Invalid classification history for area ", i)
    # Check artifact presence and size here; this does not decode or visually validate the PNG.
    png <- file.path(output, "WB_HartJohnstoneForestClasses_finalSummary_100.png")
    if (!file.exists(png) || file.info(png)$size == 0)
      stop("Missing final summary PNG for area ", i)
  }
  invisible(TRUE)
}
