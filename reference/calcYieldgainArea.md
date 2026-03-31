# calcYieldgainArea

reports potentially irrigated area depending on gainthreshold and land
constraint only

## Usage

``` r
calcYieldgainArea(
  rangeGT,
  lpjml,
  selectyears,
  iniyear,
  climatetype,
  yieldcalib,
  unit,
  irrigationsystem,
  landScen,
  cropmix,
  multicropping
)
```

## Arguments

- rangeGT:

  Range of gainthreshold for calculation of potentially irrigated areas
  (in USD per hectare)

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- selectyears:

  Years for which irrigatable area is calculated

- iniyear:

  Initialization year for cropland area

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- yieldcalib:

  If TRUE: LPJmL yields calibrated to FAO country yield in iniyear Also
  needs specification of refYields, separated by ":". Options: FALSE
  (for single cropping analyses) or "TRUE:actual:irrig_crop" (for
  multiple cropping analyses) If FALSE: uncalibrated LPJmL yields are
  used

- unit:

  Unit of yield improvement potential used as threshold, consisting of
  unit and price aggregation level separated by ":". Unit: tDM (tons per
  dry matter), USD_ha (USD per hectare) for area return, or USD_m3 (USD
  per cubic meter) for volumetric return. Price aggregation: "GLO" for
  global average prices, or "ISO" for country-level prices

- irrigationsystem:

  Irrigation system used: system share as in initialization year, or
  drip, surface, sprinkler for full irrigation by selected system

- landScen:

  Land availability scenario consisting of two parts separated by
  ":": 1. available land scenario (currCropland, currIrrig,
  potCropland) 2. protection scenario (WDPA, or one of the scenarios
  available in calcConservationPriorities, e.g., 30by20, BH, BH_IFL,
  PBL_HalfEarth, or NA for no protection). For case of no land
  protection select "NA" in second part of argument or do not specify
  second part of the argument

- cropmix:

  Selected cropmix (options: "hist_irrig" for historical cropmix on
  currently irrigated area, "hist_total" for historical cropmix on total
  cropland, or selection of proxycrops)

- multicropping:

  Multicropping activated (TRUE) or not (FALSE)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcYieldgainArea(rangeGT = seq(0, 10000, by = 100), scenario = "ssp2")
} # }
```
