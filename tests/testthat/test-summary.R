# Build a standalone simulation with no map plots or TIFF saves.
makeSummarySim <- function(name, parameters = list()) {
  SpaDES.core::simInit(modules = moduleName,
    times = list(start = 3, end = 7, timeunit = "year"),
    params = list(WB_HartJohnstoneForestClasses = c(list(useDrainage = FALSE), parameters)),
    objects = spruceInputs(), paths = simPaths(name, clean = TRUE))
}

# Area totals include all classes but keep missing cells separate from classified area.
test_that("class areas include absent classes and exclude NoData", {
  map <- rast(nrows = 1, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 1,
              crs = "EPSG:4326", vals = c(1, 1, 6, NA))
  levels(map) <- data.frame(ID = 1:6,
    class = c("deci", "mixed", "conimix", "jackpine", "larch", "spruce"))
  coltab(map) <- cbind(ID = 1:6,
    col = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f"))
  areas <- forestClassAreas(map, 2.5)
  hectares <- as.vector(values(cellSize(map, unit = "ha")))
  expect_equal(areas$year, rep(2.5, 6))
  expect_equal(areas$area_ha, c(sum(hectares[1:2]), 0, 0, 0, 0, hectares[3]))
  expect_equal(areas$nodata_ha, rep(hectares[4], 6))
  expect_equal(toupper(areas$colour[6]), "#FFD92F")
  expect_s3_class(plotForestClassAreas(areas), "ggplot")
  expect_match(plotForestClassAreas(areas)$labels$title, "WB_HartJohnstoneForestClasses", fixed = TRUE)
})

# Coverage uses the full raster extent and does not sum repeated NoData across classes.
test_that("coverage caption summarizes areas and percentages across times", {
  history <- data.table(year = rep(c(0, 10), each = 2), classId = rep(1:2, 2),
    class = rep(c("deci", "mixed"), 2), area_ha = c(30, 20, 50, 25),
    colour = rep(c("#66c2a5", "#fc8d62"), 2), nodata_ha = c(50, 50, 25, 25))
  original <- copy(history)
  graph <- plotForestClassAreas(history)
  expect_identical(graph$labels$subtitle, "Classified area only; NoData excluded from bars")
  expect_match(graph$labels$caption, "classified 50.0 to 75.0 ha; NoData 25.0 to 50.0 ha", fixed = TRUE)
  expect_match(graph$labels$caption, "50.0 to 75.0% of the full raster extent", fixed = TRUE)
  expect_identical(history, original)
  graph <- plotForestClassAreas(history[year == 0])
  expect_match(graph$labels$caption, "classified 50.0 ha; NoData 50.0 ha", fixed = TRUE)
  history[, `:=`(area_ha = 0, nodata_ha = 100)]
  expect_match(plotForestClassAreas(history)$labels$caption, "0.0% of the full raster extent", fixed = TRUE)
})

# The final event is independent of map plotting and raster saving, and sees the last update.
test_that("final summary is enabled by default and includes the end-time classification", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeSummarySim("final-summary", list(WB_HJForestClassesTimeStep = 2)))
    events <- moduleEvents(sim, "plotAndSaveFinalSummary")
    expect_equal(events$eventTime, 7)
    expect_equal(events$eventPriority, 10)
    history <- sim$WB_HartJohnstoneForestClassesAreaHistory
    expect_equal(sort(unique(history$year)), c(3, 5, 7))
    expect_equal(nrow(history), 18L)
    expect_equal(history$area_ha[history$class %in% c("mixed", "conimix", "larch")], rep(0, 9))
    expect_equal(nrow(moduleEvents(sim, "plot")), 0L)
    expect_equal(nrow(moduleEvents(sim, "save")), 0L)
    png <- file.path(simPaths("final-summary")$outputPath,
                     "WB_HartJohnstoneForestClasses_finalSummary_007.png")
    expect_true(file.exists(png))
    expect_gt(file.info(png)$size, 0)
    # The PNG signature confirms the saved output is an image rather than a placeholder.
    expect_identical(readBin(png, "raw", n = 8L),
                     as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
  })
})

test_that("final summary can be disabled", {
  skip_if_not_installed("SpaDES.core")
  withSpades({
    sim <- SpaDES.core::spades(makeSummarySim("no-final-summary", list(plotAndSaveFinalSummary = FALSE)))
    expect_equal(nrow(moduleEvents(sim, "plotAndSaveFinalSummary")), 0L)
    expect_null(sim$WB_HartJohnstoneForestClassesAreaHistory)
    expect_length(list.files(simPaths("no-final-summary")$outputPath, pattern = "\\.png$"), 0L)
  })
})
