# calcYearlyRunoff

This function calculates yearly runoff from runoff on land and water
provided by LPJmL

## Usage

``` r
calcYearlyRunoff(selectyears, lpjml, climatetype)
```

## Arguments

- selectyears:

  Years to be returned (Note: does not affect years of harmonization or
  smoothing)

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

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
calcOutput("YearlyRunoff", aggregate = FALSE)
} # }
```
