# calcIrrigCropYieldGain

This function calculates the yield gains per crop through irrigation
(including negatives)

## Usage

``` r
calcIrrigCropYieldGain(
  lpjml,
  climatetype,
  priceAgg,
  iniyear,
  selectyears,
  yieldcalib,
  multicropping
)
```

## Arguments

- lpjml:

  LPJmL version used for yields

- climatetype:

  Climate scenarios or historical baseline "GSWP3-W5E5:historical"

- priceAgg:

  Price aggregation: "GLO" for global average prices, or "ISO" for
  country-level prices, or "CONST" for same price for all crops

- iniyear:

  initialization year for food price and cropmix area

- selectyears:

  Years to be returned by the function

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
calcOutput("IrrigCropYieldGain", aggregate = FALSE)
} # }
```
