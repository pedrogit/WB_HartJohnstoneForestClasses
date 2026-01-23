# WB_HartJohnstoneForestClasses

### Overview

WB_HartJohnstoneForestClasses is a [SpaDES](https://spades.predictiveecology.org/) 
module complementing the [LandR](https://landr-manual.predictiveecology.org/) 
ecosystem of modules for forest biomass and succession simulation. It is part of 
a series of modules modelling boreal forests in Western Canada adding some layers of 
information to the LandR ecosystem. Those modules are:

- [WB_HartJohnstoneForestClasses](https://github.com/pedrogit/WB_HartJohnstoneForestClasses) - This module. Generates a map classifying LandR forested pixels to 6 (or 7) classes.
- [WB_VegBasedDrainage](https://github.com/pedrogit/WB_VegBasedDrainage) - Generates a map with two drainage classes.
- [WB_NonForestedVegClasses](https://github.com/pedrogit/WB_NonForestedVegClasses) - Generates a map of land cover classes for areas not covered by LandR (non-forested).
- [WB_LichenBiomass](https://github.com/pedrogit/WB_LichenBiomass) - Generates a map of lichen biomass for forested and non-forested areas.

As their names suggest, those modules were developed using field data collected 
in the western boreal forest of Canada. They were developed to support lichen 
biomass modelling for woodland caribou conservation.

At each simulation step, WB_HartJohnstoneForestClasses classifies cohort data 
produced by Biomass_core into 6 (or 7) classes of pixels based on the 
classification rules found in [Hart, Henkelman et al. (2019)](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14550). 

| Class Code | Description |
|-----------|-------------|
| NA | Non-forested |
| 1 | Deciduous |
| 2 | Conifer–deciduous mix |
| 3 | Conifer mix |
| 4 | Jack pine |
| 5 | Larch |
| 6 | Spruce |

If the WB_VegBasedDrainage module is part of the model during the simulation, the 
spruce class will be refined according to the quality of the drainage modeled by 
the WB_VegBasedDrainage module:

| Class Code | Description |
|-----------|-------------|
| ... | Other non-spruce classes |
| 6 | Well-drained spruce |
| 7 | Poorly-drained spruce |

Class 7 is only produced when useDrainage = TRUE and WB_VegBasedDrainage is 
present.

WB_VegBasedDrainage (note the "Veg" part in its name) is also based on 
WB_HartJohnstoneForestClasses making an optional cyclic dependency between the two 
modules. Normally a first run of WB_HartJohnstoneForestClasses during module 
initialization will classify the forest to classes 1-6, without taking drainage 
into account. A WB_VegBasedDrainage map will then be computed and used, at the 
next simulation step, by the WB_HartJohnstoneForestClasses module to refine the
classification of spruce from classes 6 to classes 6 and 7.

1. Initialization:
   - WB_HartJohnstoneForestClasses runs without drainage
   - Forest is classified into classes 1–6

2. Drainage estimation:
   - WB_VegBasedDrainage uses forest classes to compute drainage

3. Simulation steps:
   - WB_HartJohnstoneForestClasses re-runs
   - Spruce pixels are refined into well- or poorly-drained classes (6 or 7)

WB_HartJohnstoneForestClasses is dynamic in that pixelGroupMap and cohort data are 
also dynamic: relative biomass changes with time and influence the classification.

The classification rules are described in details in the classifyStand() 
function. They are based on the relative biomass quantities computed from the 
cohortData values for each pixelGroupMap.

Biomass_core pixelGroupMap pixels set to NA or 0 (disturbed) are also set to NA 
by WB_HartJohnstoneForestClass.

### Authors and Citation

* Pierre Racine <pierre.racine@sbf.ulaval.ca> [aut, cre]
* Steven G. Cumming <stevec.boreal@gmail.com> [aut]

Racine, P., Cumming, S.G. (2026) *WB_HartJohnstoneForestClasses: A SpaDES module for boreal forest classification.* SpaDES Module.

### Module Parameters

| Parameter | Class | Default | Description |
| --- | --- | --- | --- |
| WB_HJForestClassesTimeStep | integer | 1 | Simulation time at which the reclassify event should occur. |
| jackPineSp | character | \"Pinu\" | Vector of jackpine case insensitive species names e.g. c(\"Pinu_ban\", \"Pinu_con\"). Can also be the whole genus: \"Pinu\" |
| larchSp | character | \"Lari\" | Vector of larch case insensitive species names e.g. c(\"Lari_lar\", \"Lari_occ\"). Can also be the whole genus: \"Lari\" |
| spruceSp | character | \"Pice\" | Vector of spruce case insensitive species names e.g. c(\"Pice_gla\", \"Pice_mar\"). Can also be the whole genus: \"Pice\" |
| useDrainage | boolean | TRUE | Indicating if the drainage produced by the WB_VegBasedDrainage module should be taken into account to refine the spruce class into poorly-drained spruce (pd_spruce) and well-drained spruce (wd_spruce). If the WB_VegBasedDrainage module is not present, this parameter is ignored. |

### Expected Module Inputs

| Input Object | Class | Description |
| --- | --- | --- |
| cohortData | data.table | Community table created from available biomass (g/m2), age and species cover data, as well as ecozonation information or equivalent. Columns: B, pixelGroup, speciesCode. |
| pixelGroupMap | SpatRaster | Community map having mapcodes matching the cohortData community table or equivalent. |
| WB_VegBasedDrainageMap | SpatRaster | Drainage map produced by the WB_VegBasedDrainageMap module or equivalent. |

### Module Outputs

| Output Object | Class | Description |
| --- | --- | --- |
| WB_HartJohnstoneForestClassesMap | SpatRaster | Raster map classified into pre-defined 6 or 7 vegetation classes. |

### Code

The code is available here: https://github.com/pedrogit/WB_HartJohnstoneForestClasses

### Minimal Self Contained Workflow Example

Soon...



