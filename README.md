# WB_HartJohnstoneForestClasses

## Introduction

WB_HartJohnstoneForestClasses is a [SpaDES](https://spades.predictiveecology.org/) 
module complementing the [LandR](https://landr-manual.predictiveecology.org/) 
ecosystem of modules for forest biomass and succession simulation. It is part of an ensemble of modules that provide to LandR the statistical prediction of terrestrial lichen biomass from stand type, time-since fire, and terrestrial ecoprovince.

These modules are an implementatiom of [Greuel and Degré-Timmons et al (2021)](https://esajournals-onlinelibrary-wiley-com.acces.bibl.ulaval.ca/doi/full/10.1002/ecs2.3481), developed to support lichen biomass modelling for woodland caribou conservation in the Northwest Territories. The geographical area wherein the model may reasonably be applied should be assessed from Figure 2 of the cited paper. 

The components of the module ensemble are:

- [WB_HartJohnstoneForestClasses](https://github.com/pedrogit/WB_HartJohnstoneForestClasses) - This module. Generates a map classifying LandR forested pixels to 6 (or 7) classes.
- [WB_VegBasedDrainage](https://github.com/pedrogit/WB_VegBasedDrainage) - Generates a map of two drainage classes.
- [WB_NonForestedVegClasses](https://github.com/pedrogit/WB_NonForestedVegClasses) - Generates a map of land cover classes for areas LandR considers to be non-forested.
- [WB_LichenBiomass](https://github.com/pedrogit/WB_LichenBiomass) - Generates a wall-to-wall map of predicted lichen biomass density for forested and non-forested pixels.

These modules are derived from extensive empirical research in the northwest boreal of North America, as described in [Greuel and Degré-Timmons et al (2021)](https://esajournals-onlinelibrary-wiley-com.acces.bibl.ulaval.ca/doi/full/10.1002/ecs2.3481), Casheiro-Guilhem et. al (in prep.) and foundational papers by [Hart and Johnstone et al. (2018)](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14550).

## WB_HartJohnstoneForestClasses Module Overview
At each simulation step, WB_HartJohnstoneForestClasses classifies LandR cohort data produced by Biomass_core into 6 (or 7) forest classes. The classification is based on total species' biomasses, adapting the stand-type definitions in Appendix S3 of [Greuel and Degré-Timmons et al. (2021)](https://doi.org/10.1002/ecs2.3481), which follow [Hart et al. (2019)](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14550). Hart et al. judge dominance from relative live basal area (or stem density for stands under 20 years since fire). This module uses biomass instead.

| Class Code | Description |
|-----------|-------------|
| 1 | Deciduous |
| 2 | Conifer–deciduous mix |
| 3 | Conifer mix |
| 4 | Jack pine |
| 5 | Larch |
| 6 | Spruce |
| NA | LandR non-forested pixels |

The rules are tested in this order on the total biomass of each pixel group (every cohort and age summed). All thresholds are inclusive (`>=`), and there is no floating-point tolerance:

| Priority | Class | Rule | Appendix S3 rule |
|---:|---|---|---|
| 1 | 4 Jack pine | jack pine share >= 2/3 | >= 66% jack pine |
| 2 | 5 Larch | larch share >= 0.75 | > 75% larch |
| 3 | 6 Spruce | spruce share >= 2/3 | >= 66% spruce |
| 4 | 3 Conifer mix | all conifer groups >= 2/3 | < 66% spruce or jack pine but >= 2/3 conifer |
| 5 | 1 Deciduous | non-conifer share (1 - conifer share) >= 2/3 | >= 66% broadleaf deciduous species |
| 6 | 2 Conifer–deciduous mix | everything else, including groups with zero biomass | < 66% broadleaf deciduous and < 66% conifer |

Differences from Appendix S3 that remain: thresholds use 2/3 where the appendix writes 66%; larch is inclusive at 0.75 where the appendix says "> 75%"; the deciduous class is the share of all biomass outside the four conifer groups, which includes unrecognised or missing species codes, instead of the share of broadleaf deciduous species; the mixed class is the default for everything else, so it also receives stands that the appendix leaves undefined; and biomass replaces basal area and stem density.

If the WB_VegBasedDrainage module is part of the SpaDES simulation model, the WB_HartJohnstoneForestClasses spruce class will be refined to well-drained and poorly-drained spruce following to the two drainage classes produced by WB_VegBasedDrainage :

| Class Code | Description |
|-----------|-------------|
| 1-5 | Other non-spruce classes |
| 6 | Well-drained spruce (drainage map value 2) |
| 7 | Poorly-drained spruce (drainage map value 1) |

Class 7 is only produced when module parameter **useDrainage** is equal to TRUE and the WB_VegBasedDrainage module is present. Otherwise Class 6 combines spruce pixels of all drainage classes.

WB_VegBasedDrainage (note the "Veg" part in the name) is also based on WB_HartJohnstoneForestClasses, because the determination of drainag class is itself forest-class dependent. This relationship leads to an optional cyclic dependency between the two modules. Normally a first run of WB_HartJohnstoneForestClasses during module initialization will classify the forest to classes 1-6, without taking drainage into account. A WB_VegBasedDrainage map will then be computed and used, at the next simulation step, by the WB_HartJohnstoneForestClasses module, to refine the classification of spruce from classes 6 to classes 6 and 7.

1. Initialization:
   - WB_HartJohnstoneForestClasses runs without drainage
   - Forest is classified into classes 1–6

2. Drainage estimation:
   - WB_VegBasedDrainage uses forest classes to compute drainage

3. Simulation steps:
   - WB_HartJohnstoneForestClasses re-runs
   - Spruce pixels are refined into well- or poorly-drained classes (6 or 7)

WB_HartJohnstoneForestClasses is dynamic in that pixelGroupMap and cohort data are 
also dynamic: relative biomass changes with time and therefore influence the forest classification. 
This in turn may affect the drainage classification, if module WB_VegBasedDrainage in included in the simulation. 

The classification rules are described in details in the classifyStand() 
function. They are based on the relative biomass quantities computed from the 
cohortData values for each pixelGroupMap.

Biomass_core pixelGroupMap pixels set to NA or 0 (disturbed) are also set to NA 
by WB_HartJohnstoneForestClass.

### Authors and Citation

* Pierre Racine <pierre.racine@sbf.ulaval.ca> [aut, cre]
* Steven G. Cumming <stevec.boreal@gmail.com> [aut]

Racine, P., Cumming, S.G. (2026) *WB_HartJohnstoneForestClasses: A SpaDES module for the classification of western boreal forest in Canada.* SpaDES Module.

### Module Parameters

| Parameter | Class | Default | Description |
| --- | --- | --- | --- |
| WB_HJForestClassesTimeStep | integer | 10 | Return interval for classification even, as per LandR.|
| jackPineSp | character | \"Pinu\" | Jack pine species: Vector of case insensitive species codes e.g. c(\"Pinu_ban\", \"Pinu_con\"). Can also be the whole genus: \"Pinu\" |
| larchSp | character | \"Lari\" | Larch species: Vector of case insensitive species codes e.g. c(\"Lari_lar\", \"Lari_occ\"). Can also be the whole genus: \"Lari\" |
| spruceSp | character | \"Pice\" | Spruce species: Vector of case insensitive species codes e.g. c(\"Pice_gla\", \"Pice_mar\"). Can also be the whole genus: \"Pice\" |
| otherConiferSp | character | `c("Abie", "Calc", "Cham", "Juni", "Pinu", "Pseu", "Sequ", "Taxu", "Thuj", "Tsug")` | Case-insensitive literal species-code prefixes for additional conifers. Applied after jack pine, larch and spruce, without double counting. An empty character vector disables this group. |
| writeClassificationCsv | boolean | FALSE | Whether to write the classification summary CSV (`WB_HartJohnstoneForestClasses_<time>.csv`, one row per pixel group with biomass shares and class) in the output path at each classification. |
| plotAndSaveFinalSummary | logical | TRUE | Display and save a PNG of stacked forest-class areas (ha) at simulation end. Independent of recurring map plots and TIFF saving. |
| useDrainage | boolean | TRUE | Indicating if the drainage produced by the WB_VegBasedDrainage module should be taken into account to refine the spruce class into poorly-drained spruce (pd_spruce) and well-drained spruce (wd_spruce). If the WB_VegBasedDrainage module is not present, this parameter is ignored. |

All four species parameters use case-insensitive literal prefix matching, not
regular expressions. Missing, blank or whitespace-padded prefixes are rejected.

An empty character vector specifies no species prefixes. For `otherConiferSp`,
it disables additional conifers while leaving the jack-pine, larch and spruce
settings unchanged.

### Final area summary

With `plotAndSaveFinalSummary = TRUE`, each classification records areas calculated from raster
cell areas in hectares. The table `sim$WB_HartJohnstoneForestClassesAreaHistory` contains
`year`, `classId`, `class`, `area_ha`, `colour` and `nodata_ha`. Absent classes have zero
area, and NoData area is tracked separately rather than included in the bars.
The subtitle states that NoData is excluded. A caption beneath the graph summarizes
classified and NoData area ranges across classification times, plus classified coverage
as a percentage of the full raster extent. NoData includes masked cells; this percentage
is not necessarily the percentage of the study area with valid data.

The `plotAndSaveFinalSummary` event runs once at simulation end, at priority 10, after the final
classification and TIFF save. It displays the stacked-bar graph on the active graphics
device using the raster's class colours, with the module name in its title. The same graph
is saved to `WB_HartJohnstoneForestClasses_finalSummary_<time>.png` in `outputPath(sim)`
at 11 by 6 inches and 300 dpi; an existing file is overwritten. It does not require saved
TIFFs or `.plotInitialTime`. Set `plotAndSaveFinalSummary = FALSE` to disable area recording,
display and PNG saving. Regular map plotting remains opt-in.

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



## Change History

### 2026-11-06 — Biomass normalization

`classifyStand()` requires a `B` column. On its local working table, finite negative
biomass, logical values, missing values (including NaN) and unparseable strings become
zero. Numeric strings and factor labels are converted to numbers. Infinite values are
left unchanged; no overflow check is applied. Finite nonnegative numeric inputs retain
their existing results, and zero-total groups remain mixed (class 2). The caller's
`cohortData` is not modified.

### 2026-11-06 — cohortData is no longer modified

`classifyStand()` added a `speciesNb` column to the `cohortData` table it received, because
`cd <- cohortData` does not copy a data.table. In a simulation this table is
`sim$cohortData`, which belongs to Biomass_core. The function now works on a copy of the
three columns it uses (`pixelGroup`, `speciesCode`, `B`), so the caller's table is left
unchanged, a stale `speciesNb` column in the input is ignored, and a plain data.frame is
accepted. Classification results are unchanged (identical maps on the year 0, 50 and 100
snapshots).

### 2026-11-06 — Fractional simulation times in file names

The saved TIFF and the optional CSV were named with `sprintf("%03d", time(sim))`, which
fails when the simulation time is fractional (for example 2.5). File names are now built by
`formatSimulationTime()`. Whole-number times keep their existing names
(`WB_HartJohnstoneForestClasses_003.tif`); fractional times keep their fraction
(`WB_HartJohnstoneForestClasses_002.5.tif`).

### 2026-11-06 — Drainage labels corrected

WB_VegBasedDrainage codes 1 as poorly drained and 2 as well drained. The module
previously moved spruce to class 7 where the drainage map was 2, so well-drained spruce
carried the poorly-drained label and the reverse. Spruce on poorly drained ground (1) now
becomes class 7 (`pd_spruce`) and spruce on well drained ground (2) stays class 6
(`wd_spruce`). Results with a drainage map change for every spruce cell: classes 6 and 7
are swapped. Results without a drainage map are unchanged.

### 2026-11-04 — Genus prefixes

Removed trailing underscores from the `otherConiferSp` defaults to match the
genus-prefix style of larch and spruce. Both forms match standard LandR codes;
the shorter prefixes also accept codes without an underscore after the genus.
The species-specific jack-pine default remains `"Pinu_ban"`.

### 2026-11-04 — Consistent species matching

All four species parameters now share literal prefix matching and validation.
Defaults, thresholds and group precedence are unchanged. Custom regex patterns
are no longer interpreted: replace patterns such as `"^Pice"` or `"Pice.*"`
with the prefix `"Pice"`. Empty vectors now consistently match nothing.

### 2026-11-04 — Additional conifers

Added `otherConiferSp`, a character vector of case-insensitive literal
species-code prefixes. Default:

```r
c("Abie", "Calc", "Cham", "Juni", "Pinu", "Pseu", "Sequ", "Taxu", "Thuj", "Tsug")
```

Jack pine, larch and spruce are assigned first; remaining matches become other
conifers. All four groups contribute to conifer mix without double counting.
Existing dominance thresholds are unchanged. Deciduous remains the residual
outside these groups. Use an empty character vector for the previous behavior. CSV summaries
now include `otherConiferPct` and `coniferPct`.

### 2026-11-03 — Scheduling

Classification now runs after core growth and summary, at simulation start and
each succession boundary (priority 8.5). `WB_HJForestClassesTimeStep` now defaults
to `NA`, following the core interval, or 10 years standalone. Explicit intervals
must match the core. Optional plots and saves follow classification; their
missing intervals use the classification interval. These changes supersede the
older timing descriptions above.

### 2026-11-04 — Optional CSV summary

Added `writeClassificationCsv` (default `FALSE`). The classification summary
CSV is now written only when it is `TRUE`; previously it was always written.
Classification results are unchanged.

### 2026-11-06 — Inclusive deciduous boundary

The deciduous rule is now `1 - conifer share >= 2/3` instead of `> 2/3`, so that it is
inclusive like every other dominance rule and like Appendix S3 of Greuel et al. (2021)
(">= 66% broadleaf deciduous species"). A pixel group whose non-conifer share is exactly
2/3 is now deciduous instead of mixed. No pixel group of the year 0, 50 or 100 snapshots
(82,928 cells each) sits exactly on the boundary, so their maps are unchanged. The
classification rules table above now lists the Appendix S3 rule next to each rule.
