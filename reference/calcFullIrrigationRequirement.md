# calcFullIrrigationRequirement

This function calculates the water requirements for full irrigation per
cell per crop given potentially available land

## Usage

``` r
calcFullIrrigationRequirement(
  lpjml,
  climatetype,
  selectyears,
  iniyear,
  comagyear,
  irrigationsystem,
  landScen,
  cropmix,
  multicropping
)
```

## Arguments

- lpjml:

  LPJmL version used

- climatetype:

  Climate model or historical baseline "GSWP3-W5E5:historical"

- selectyears:

  Years to be returned

- iniyear:

  Croparea initialization year

- comagyear:

  if !NULL: already irrigated area is subtracted; if NULL: total
  potential land area is used; year specified here is the year of the
  initialization used for cropland area initialization in
  calcIrrigatedArea

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

  Multicropping activated (TRUE) or not (FALSE) and Multiple Cropping
  Suitability mask selected (mask can be: "none": no mask applied (only
  for development purposes) "actual:total": currently multicropped areas
  calculated from total harvested areas and total physical areas per
  cell from readLandInG "actual:crop" (crop-specific),
  "actual:irrigation" (irrigation-specific), "actual:irrig_crop" (crop-
  and irrigation-specific) "total" "potential:endogenous": potentially
  multicropped areas given temperature and productivity limits
  "potential:exogenous": potentially multicropped areas given GAEZ
  suitability classification) (e.g. TRUE:actual:total; TRUE:none; FALSE)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FullIrrigationRequirement", aggregate = FALSE)
} # }
```
