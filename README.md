# WB_HartJohnstoneForestClass

WB_HartJohnstoneForestClass is a SpaDES module associated with the LandR set of 
modules for forest biomass and succession simulation. It is part of a series of 
module modelling boreal forest in Western Canada adding some layers of 
information to the LandR ecosystem. Those modules are:

- [WB_HartJohnstoneForestClasses](https://github.com/pedrogit/WB_HartJohnstoneForestClasses): this module classifying LandR forested pixels to 6 (or 7) classes.
- [WB_VegBaseDrainage](https://github.com/pedrogit/WB_VegBasedDrainage): produce a map with two drainage classes.
- [WB_NonForestedVegClasses](https://github.com/pedrogit/WB_NonForestedVegClasses): determine the land cover areas not covered by LandR.
- [WB_LichenBiomass](): generate a map of lichen biomass.

As their names suggest, those modules were developed using field data collected 
in the Western boreal forest of Canada. Their main aim is to model lichen biomass 
for caribou conservation.

At each simulation step, WB_HartJohnstoneForestClass classifies cohort data 
produced by Biomass_core into 6 (or 7) classes of pixels based on the 
classification rules found in [Hart, Henkelman et al. (2019)](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14550). 

 - non-forested (NA)
 - deciduous (1)
 - mixed types (conifer and deciduous) (2)
 - mix of conifer (3)
 - jackpine (4)
 - larch (5)
 - spruce (6)

If the WB_VegBasedDrainage module is part of the model during the simulation, the 
spruce class will be refined according to the quality of the drainage modeled by 
the WB_VegBasedDrainage module:

 - well-drained spruce (6)
 - poorly-drained spruce (7).

WB_VegBasedDrainage (note the "Veg" part in it's name) is also based on 
WB_HartJohnstoneForestClass making an optional cyclic dependency between the two 
modules. Normally a first run of WB_HartJohnstoneForestClass during module 
initialization will classify the forest to classes 1-6, without taking drainage 
into account. A WB_VegBasedDrainage maps will then be computed and used, at the 
next simulation step, by the WB_HartJohnstoneForestClass module to refine the
classification of spruce from classes 1-6 to classes 1-7.

WB_HartJohnstoneForestClass is dynamic in that pixelGroupMap and cohort data are 
also dynamic: relative biomass changes with time and influence the classification.

The classification rules are explicited in details in the classifyStand() 
function. They are based on the relative biomass quantities computed from the 
cohortData values for each pixelGroupMap.

Biomass_core pixelGroupMap pixels set to NA or 0 (disturbed) are also set to NA 
by WB_HartJohnstoneForestClass.

### Authors

Pierre Racine <pierre.racine@sbf.ulaval.ca> [aut, cre]

### Module Parameters

| Parameter | Class | Default | Description |
| --- | --- | --- | --- |
| WB_HJForestClassesTimeStep | integer | 1 | Simulation time at which the reclassify event should occur. |
| jackPineSp | character | \"Pinu\" | Vector of jackpine species names e.g. c(\"Pinu_ban\", \"Pinu_con\"). Can also be the whole genus: \"Pinu\" |
| larchSp | character | \"Lari\" | Vector of larch species names e.g. c(\"Lari_lar\", \"Lari_occ\"). Can also be the whole genus: \"Lari\" |
| spruceSp | larchSp | character | \"Pice\" | Vector of larch species names e.g. c(\"Lari_lar\", \"Lari_occ\"). Can also be the whole genus: \"Pice\" |
Vector of spruce species names e.g. c(\"Pice_gla\", \"Pice_mar\"). Can also be the whole genus: \"Pice\" |
| useDrainage | boolean | TRUE | Indicating if the drainage produced by the WB_VegBasedDrainage module should be taken into account to refine the spruce class into poorly-drained spruce (pd_spruce) and well-drained sprice (wd_spruce). If the WB_VegBasedDrainage module is not present, this parameter is ignored. |

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

### Minimal Self Contained Example



