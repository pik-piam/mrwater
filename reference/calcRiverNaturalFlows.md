# calcRiverNaturalFlows

This function calculates natural discharge for the river routing derived
from inputs from LPJmL

## Usage

``` r
calcRiverNaturalFlows(selectyears, lpjml, climatetype)
```

## Arguments

- selectyears:

  Years to be returned (Note: does not affect years of harmonization or
  smoothing)

- lpjml:

  LPJmL version used

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

## Value

magpie object in cellular resolution

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("RiverNaturalFlows", aggregate = FALSE)
} # }
```
