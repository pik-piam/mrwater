# calcCropAreaPotIrrig

This function calculates croparea that is potentially available for
irrigated agriculture per crop given a chosen cropmix

## Usage

``` r
calcCropAreaPotIrrig(selectyears, comagyear, iniyear, cropmix, landScen)
```

## Arguments

- selectyears:

  Years to be returned

- comagyear:

  If NULL: total potential croparea is used; if !NULL: already irrigated
  area is subtracted; year specified here is the year of the
  initialization used for cropland area initialization in
  calcIrrigatedArea (e.g. NULL, 1995, 2010)

- iniyear:

  Initialization year for current cropland area

- cropmix:

  Cropmix for which croparea share is calculated (options: "hist_irrig"
  for historical cropmix on currently irrigated area, "hist_total" for
  historical cropmix on total cropland, or selection of proxycrops as
  vector)

- landScen:

  Land availability scenario consisting of two parts separated by
  ":": 1. available land scenario (currCropland, currIrrig,
  potCropland) 2. protection scenario (WDPA, or one of the scenarios
  available in calcConservationPriorities, e.g., 30by20, BH, BH_IFL,
  PBL_HalfEarth, or NA for no protection). For case of no land
  protection select "NA" in second part of argument or do not specify
  second part of the argument

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("CropAreaPotIrrig", aggregate = FALSE)
} # }
```
