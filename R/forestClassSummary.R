# Cell areas are calculated in hectares, including geographic or non-equal-area grids.
# Every legend class gets a row, even when absent; NoData is tracked outside class totals.
forestClassAreas <- function(classMap, year) {
  if (!inherits(classMap, "SpatRaster")) classMap <- terra::rast(classMap)
  legend <- terra::cats(classMap)[[1L]]
  codes <- as.vector(terra::values(classMap))
  hectares <- as.vector(terra::values(terra::cellSize(classMap, unit = "ha", mask = FALSE)))
  palette <- terra::coltab(classMap)[[1L]]
  palette <- palette[match(legend[[1L]], palette[[1L]]), ]
  data.table::data.table(
    year = year, classId = legend[[1L]], class = legend[[2L]],
    area_ha = vapply(legend[[1L]], function(id) {
      sum(hectares[!is.na(codes) & codes == id])
    }, numeric(1)),
    colour = grDevices::rgb(palette$red, palette$green, palette$blue, maxColorValue = 255),
    nodata_ha = sum(hectares[is.na(codes)]))
}

# Each stacked bar represents the classified area at one classification time.
# Class colours come from the raster legend; missing area is excluded from the stacks.
plotForestClassAreas <- function(history) {
  history <- data.table::copy(history)
  legend <- unique(history[, .(classId, class, colour)])
  data.table::setorder(legend, classId, class)
  history[, class := factor(class, levels = unique(legend$class))]
  years <- sort(unique(history$year))
  width <- if (length(years) > 1L) min(diff(years)) * 0.8 else 0.8
  # NoData is repeated for each class, so count it only once per classification time.
  coverage <- history[, .(classified_ha = sum(area_ha), nodata_ha = nodata_ha[1L]), by = year]
  # Classified plus NoData area covers the full raster extent, including masked cells.
  # This denominator is not restricted to the study-area boundary.
  coverage[, classified_pct := 100 * classified_ha / (classified_ha + nodata_ha)]
  # Ranges keep the caption compact even when the simulation has many classification times.
  # Show one decimal and thousands separators; a constant range is printed only once.
  formatRange <- function(x) {
    bounds <- formatC(range(x), format = "f", digits = 1, big.mark = ",")
    if (bounds[1L] == bounds[2L]) bounds[1L] else paste(bounds, collapse = " to ")
  }
  # Each range summarizes all recorded times independently; its endpoints need not occur
  # in the same years as the endpoints of the other ranges. Two lines fit below the graph.
  coverageCaption <- paste0(
    "Coverage across classification times: classified ", formatRange(coverage$classified_ha),
    " ha; NoData ", formatRange(coverage$nodata_ha), " ha.\n",
    "Classified coverage: ", formatRange(coverage$classified_pct),
    "% of the full raster extent; NoData includes any masked cells.")
  displayNames <- c(deci = "Deciduous", mixed = "Mixed", conimix = "Conifer mix",
                    jackpine = "Jack pine", larch = "Larch", spruce = "Spruce",
                    wd_spruce = "Well-drained spruce", pd_spruce = "Poorly-drained spruce")
  ggplot2::ggplot(history, ggplot2::aes(x = year, y = area_ha, fill = class)) +
    ggplot2::geom_col(width = width) +
    ggplot2::scale_fill_manual(values = stats::setNames(legend$colour, legend$class),
      breaks = legend$class, labels = unname(displayNames[legend$class]), drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = years) +
    # These labels belong to the plot itself, so screen and saved PNG share the same notes.
    ggplot2::labs(title = "WB_HartJohnstoneForestClasses: Forest-class area over time", x = "Simulation year",
      subtitle = "Classified area only; NoData excluded from bars",
      caption = coverageCaption, y = "Area (ha)", fill = "Forest class") +
    # Left alignment keeps the two-line coverage summary readable beneath the axes.
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
}
