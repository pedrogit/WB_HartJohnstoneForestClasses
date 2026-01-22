# WB_HartJohnstoneForestClass

WB_HartJohnstoneForestClass is a SpaDES module associated with the LandR set of 
modules for forest biomass and succession simulation. At each simulation step, 
WB_HartJohnstoneForestClass reclassify the pixels produced by Biomass_core to 6 
(or 7) classes based on the classification rules found in [Hart, Henkelman et al. (2019)](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14550). 

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

WB_VegBasedDrainage is also based on WB_HartJohnstoneForestClass making an 
optional cyclic dependency between the tow modules. Normally a first run of 
WB_HartJohnstoneForestClass during module initialization will classify the 
forest to classes 1-6, without taking drainage into account. A 
WB_VegBasedDrainage maps will then be compute and used at the next simulation 
step to produce a refined classification to classes 1-7.

WB_HartJohnstoneForestClass is dynamic in that pixelGroupMap and cohort data are 
also dynamic: relative biomass changes with time and influence the classification.

The classification rules are explicited in details in the classifyStand() 
function. They are based on the relative biomass quantities computed from the 
cohortData values for each pixelGroupMap.

Biomass_core pixelGroupMap pixels set to NA or 0 (disturbed) are also set to NA 
by WB_HartJohnstoneForestClass.

#### Authors:

Pierre Racine <pierre.racine@sbf.ulaval.ca> [aut, cre]

### Module Parameters

| Parameter | Description |
| --- | --- |
| WB_HartJohnstoneForestClassesTimeStep | simulation time at which the reclassify event should occur. |
| jackPineSp | Vector of jackpine species names e.g. c(\"Pinu_ban\", \"Pinu_con\"). Can also be the whole genus: \"Pinu\" |
| larchSp | Vector of larch species names e.g. c(\"Lari_lar\", \"Lari_occ\"). Can also be the whole genus: \"Lari\" |
| spruceSp | Vector of spruce species names e.g. c(\"Pice_gla\", \"Pice_mar\"). Can also be the whole genus: \"Pice\" |
| useDrainage | boolean indicating if the drainage produced by the WB_VegBasedDrainage module should be taken into account. If the WB_VegBasedDrainage module is not present, this parameter is ignored. |

### Expected Module Inputs

### Module Outputs

### Code

### Minimal Self Contained Example



