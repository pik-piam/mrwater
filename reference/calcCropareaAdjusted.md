# calcCropareaAdjusted

This returns croparea as reported by FAO and LUH for the initialization
year and

## Usage

``` r
calcCropareaAdjusted(iniyear, dataset = "LandInG", sectoral = "kcr")
```

## Arguments

- iniyear:

  initialization year

- dataset:

  LUH or LandInG Note: once migration to Toolbox data is complete, this
  function can be replaced with calcCropareaLandInG

- sectoral:

  crops to be reported: "kcr" for MAgPIE items, and "lpj" for LPJmL
  items

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("CropareaAdjusted", aggregate = FALSE)
} # }
```
