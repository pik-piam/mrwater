# calcDischargeInaccessible

This function calculates the discharge that is inaccessible to humans
based on the variability of monthly flows and natural discharge.

## Usage

``` r
calcDischargeInaccessible(lpjml, selectyears, climatetype, accessibilityrule)
```

## Arguments

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- selectyears:

  Years to be returned (Note: does not affect years of harmonization or
  smoothing)

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- accessibilityrule:

  Method used: Quantile method (Q) or Coefficient of Variation (CV)
  combined with scalar value defining the strictness of accessibility
  restriction: discharge that is exceeded x percent of the time on
  average throughout a year (Qx, e.g. Q75: 0.25, Q50: 0.5) or base value
  for exponential curve separated by : (CV:2)

## Value

magpie object in cellular resolution representing discharge that is
inaccessible to humans

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("DischargeInaccessible", aggregate = FALSE)
} # }
```
