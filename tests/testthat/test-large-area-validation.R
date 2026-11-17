# Exercise validation with small disk fixtures: no downloads, authentication, or simulations.
# Loading definitions into a private environment also checks that sourcing them is safe.
largeAreaHarness <- new.env(parent = globalenv())
sys.source(file.path(moduleDir, "tests", "large-area-functions.R"), envir = largeAreaHarness)

test_that("large-area validation rejects incomplete or failed batches", {
  result <- list(output_directory = scratch,
    results = data.frame(area_id = 1:10, status = "completed", error = ""))
  # Recorded failures should identify the area and preserve its diagnostic message.
  result$results$status[4] <- "failed"
  result$results$error[4] <- "crop failed"
  expect_error(largeAreaHarness$validateTenLargeAreas(result), "4: crop failed")
  # A batch cannot pass merely because its failed area is omitted from the manifest.
  result$results <- result$results[-4, ]
  expect_error(largeAreaHarness$validateTenLargeAreas(result), "ten distinct")
})

test_that("large-area validation checks histories and output artifacts", {
  root <- file.path(scratch, "large-area-validation")
  result <- list(output_directory = root,
    results = data.frame(area_id = 1:10, status = "completed", error = ""))
  # Minimal artifacts satisfy the validator's contract without creating real simulation rasters.
  for (i in 1:10) {
    output <- file.path(root, sprintf("area_%02d", i))
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    write.csv(data.frame(year = c(0, 100), area_ha = c(10, 12)),
      file.path(output, "class_area_history.csv"), row.names = FALSE)
    write.csv(data.frame(moduleName = "WB_HartJohnstoneForestClasses",
      eventType = c("reComputeHJForestClassesMap", "reComputeHJForestClassesMap", "plotAndSaveFinalSummary"),
      eventTime = c(0, 100, 100)), file.path(output, "completed_events.csv"), row.names = FALSE)
    saveRDS(list(classification_years = c(0, 100)), file.path(output, "validation.rds"))
    # The validator checks PNG existence/size, not image contents; one byte is sufficient here.
    writeBin(as.raw(1), file.path(output, "WB_HartJohnstoneForestClasses_finalSummary_100.png"))
  }
  expect_true(largeAreaHarness$validateTenLargeAreas(result))
  # Alter one artifact at a time to distinguish empty histories from missing classification years.
  history <- file.path(root, "area_01", "class_area_history.csv")
  write.csv(data.frame(year = numeric(), area_ha = numeric()), history, row.names = FALSE)
  expect_error(largeAreaHarness$validateTenLargeAreas(result), "Invalid classification history")
  write.csv(data.frame(year = 0, area_ha = 10), history, row.names = FALSE)
  expect_error(largeAreaHarness$validateTenLargeAreas(result), "Invalid classification history")
  # Restore valid history before testing a separate empty-summary failure.
  write.csv(data.frame(year = c(0, 100), area_ha = c(10, 12)), history, row.names = FALSE)
  png <- file.path(root, "area_01", "WB_HartJohnstoneForestClasses_finalSummary_100.png")
  writeBin(raw(), png)
  expect_error(largeAreaHarness$validateTenLargeAreas(result), "Missing final summary PNG")
})
