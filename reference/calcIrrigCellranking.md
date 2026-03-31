# calcIrrigCellranking

This function calculates a cellranking for the river basin discharge
allocation based on yield improvement potential through irrigation

## Usage

``` r
calcIrrigCellranking(
  lpjml,
  climatetype,
  cellrankyear,
  iniyear,
  comagyear,
  irrigationsystem,
  landScen,
  method,
  cropmix,
  yieldcalib,
  multicropping
)
```

## Arguments

- lpjml:

  LPJmL version used for yields

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical" for yields

- cellrankyear:

  Year(s) for which cell rank is calculated

- iniyear:

  Initialization year for price

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

- method:

  Rank and optimization method consisting of Unit according to which
  rank is calculated: tDM (tons per dry matter), USD_ha (USD per
  hectare) for relative area return, or USD_m3 (USD per cubic meter) for
  relative volumetric return; USD for absolute return (total profit);
  Price aggregation: "GLO" for global average prices, or "ISO" for
  country-level prices; and boolean indicating fullpotential (TRUE) or
  reduced potential (FALSE)

- cropmix:

  Selected cropmix for which yield improvement potential is calculated
  (options: "hist_irrig" for historical cropmix on currently irrigated
  area, "hist_total" for historical cropmix on total cropland, or
  selection of proxycrops) NULL returns all crops individually

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
calcOutput("IrrigCellranking", aggregate = FALSE)
} # }
```
