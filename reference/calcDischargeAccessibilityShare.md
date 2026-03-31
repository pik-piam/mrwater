# calcDischargeAccessibilityShare

This function calculates the share of discharge that is useable given
the variability of monthly flows. If discharge is highly variable, it is
harder to bring into productive use and therefore water availability
(for human use) is reduced.

## Usage

``` r
calcDischargeAccessibilityShare(
  lpjml,
  selectyears,
  climatetype,
  accessibilityrule
)
```

## Arguments

- lpjml:

  LPJmL version used

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
  average throughout a year is inaccessible (e.g. Q:1 all discharge
  accessible, Q:0.75 0.75-quantile is accessible everything that is more
  variable inaccessible) or base value for exponential curve, separated
  by : (CV:2)

## Value

magpie object in cellular resolution representing share of discharge
that is accessible to humans

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("DischargeAccessibilityShare", aggregate = FALSE)
} # }
```
