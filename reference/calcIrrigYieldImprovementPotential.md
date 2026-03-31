# calcIrrigYieldImprovementPotential

This function calculates the yield improvement potential through
irrigation for all grid cells given a certain crop mix

## Usage

``` r
calcIrrigYieldImprovementPotential(
  lpjml,
  climatetype,
  unit,
  iniyear,
  selectyears,
  comagyear,
  cropmix,
  landScen,
  irrigationsystem,
  yieldcalib,
  multicropping
)
```

## Arguments

- lpjml:

  LPJmL version used for yields

- climatetype:

  Climate scenarios or historical baseline "GSWP3-W5E5:historical"

- unit:

  Unit of yield improvement potential to be returned and level of price
  aggregation used, separated by ":". Unit: USD_ha (USD per hectare) for
  relative area return, or USD_m3 (USD per cubic meter) for relative
  volumetric return; USD for absolute return (total profit); for
  relative return according to area and volume. Price aggregation: "GLO"
  for global average prices, or "ISO" for country-level prices

- iniyear:

  initialization year for food price and cropmix area

- selectyears:

  Years to be returned by the function

- comagyear:

  if !NULL: already irrigated area is subtracted; if NULL: total
  potential land area is used; year specified here is the year of the
  initialization used for cropland area initialization in
  calcIrrigatedArea

- cropmix:

  Selected cropmix for which yield improvement potential is calculated
  (options: "hist_irrig" for historical cropmix on currently irrigated
  area, "hist_total" for historical cropmix on total cropland, or
  selection of proxycrops) NULL returns all crops individually

- landScen:

  Land availability scenario consisting of two parts separated by
  ":": 1. available land scenario (currCropland, currIrrig,
  potCropland) 2. protection scenario (WDPA, or one of the scenarios
  available in calcConservationPriorities, e.g., 30by20, BH, BH_IFL,
  PBL_HalfEarth, or NA for no protection). For case of no land
  protection select "NA" in second part of argument or do not specify
  second part of the argument

- irrigationsystem:

  Irrigation system used: system share as in initialization year, or
  drip, surface, sprinkler for full irrigation by selected system

- yieldcalib:

  If TRUE: LPJmL yields calibrated to FAO country yield in iniyear Also
  needs specification of refYields, separated by ":". Options: FALSE
  (for single cropping analyses) or "TRUE:actual:irrig_crop" (for
  multiple cropping analyses) If FALSE: uncalibrated LPJmL yields are
  used

- multicropping:

  Multicropping activated (TRUE) or not (FALSE)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("IrrigYieldImprovementPotential", aggregate = FALSE)
} # }
```
