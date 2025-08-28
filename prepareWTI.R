library(reproducible)
library(terra)
pgmFolder <- "G:/Home/FromMaria/CanESM2_run1"
pixelGroupMap <- rast(readRDS(file.path(pgmFolder, "pixelGroupMap_year2011.rds")))

TWIRasterFolder <- "G:/Home/FromAndres/TWI/"
inRast <- rast(file.path(TWIRasterFolder, "TWI_boreal_forest_30m.tif"))

outFilename <- "TWI_boreal_forest_250m.tif"

newRast <- postProcessTo(rast,
                         rasterToMatch = pixelGroupMap,
                         maskWithRTM  = TRUE,
                         writeTo = file.path(TWIRasterFolder, outFilename), 
                         method = "bilinear",
                         overwrite = TRUE,
                         verbose = 10
)

