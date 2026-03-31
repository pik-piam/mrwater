# calcIrrigatableAreaUnlimited

calculates area that can potentially be irrigated given chosen land
scenario and gainthreshold

## Usage

``` r
calcIrrigatableAreaUnlimited(
  selectyears,
  iniyear,
  landScen,
  lpjml,
  climatetype,
  cropmix,
  yieldcalib,
  irrigationsystem,
  unit,
  gainthreshold,
  multicropping
)
```

## Arguments

- selectyears:

  years for which irrigatable area is calculated

- iniyear:

  initialization year

- landScen:

  Land availability scenario consisting of two parts separated by
  ":": 1. available land scenario (currCropland, currIrrig,
  potCropland) 2. protection scenario (WDPA, or one of the scenarios
  available in calcConservationPriorities, e.g., 30by20, BH, BH_IFL,
  PBL_HalfEarth, or NA for no protection). For case of no land
  protection select "NA" in second part of argument or do not specify
  second part of the argument

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- cropmix:

  Selected cropmix (options: "hist_irrig" for historical cropmix on
  currently irrigated area, "hist_total" for historical cropmix on total
  cropland, or selection of proxycrops)

- yieldcalib:

  If TRUE: LPJmL yields calibrated to FAO country yield in iniyear Also
  needs specification of refYields, separated by ":". Options: FALSE
  (for single cropping analyses) or "TRUE:actual:irrig_crop" (for
  multiple cropping analyses) If FALSE: uncalibrated LPJmL yields are
  used

- irrigationsystem:

  Irrigation system used: system share as in initialization year, or
  drip, surface, sprinkler for full irrigation by selected system

- unit:

  Unit of yield improvement potential to be returned and level of price
  aggregation used, separated by ":". Unit: USD_ha (USD per hectare) for
  relative area return, or USD_m3 (USD per cubic meter) for relative
  volumetric return; USD for absolute return (total profit); Price
  aggregation: "GLO" for global average prices, or "ISO" for
  country-level prices

- gainthreshold:

  Threshold of yield improvement potential (in USD per hectare)

- multicropping:

  Multicropping activated (TRUE) or not (FALSE)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("IrrigatableAreaUnlimited", aggregate = FALSE)
} # }
```
