# calcCropAreaShare

This function calculates the crop area share for a chosen cropmix

## Usage

``` r
calcCropAreaShare(iniyear, cropmix)
```

## Arguments

- iniyear:

  Croparea initialization year

- cropmix:

  Cropmix for which croparea share is calculated (options: "hist_irrig"
  for historical cropmix on currently irrigated area, "hist_rainf" for
  historical cropmix on currently irrigated area, "hist_total" for
  historical cropmix on total cropland, or selection of proxycrops)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier, Benjamin L. Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("CropAreaShare", aggregate = FALSE)
} # }
```
