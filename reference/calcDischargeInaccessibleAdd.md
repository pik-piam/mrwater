# calcDischargeInaccessibleAdd

This function calculates water that is inaccessible to humans but not
part of EFRs. Reason: Inacessible discharge is highly variable discharge
that can also serve as high flow requirements (HFR) for EFRs. This has
to be accounted in the Discharge Allocation Algorithm for the
determination of potential irrigation.

## Usage

``` r
calcDischargeInaccessibleAdd(
  selectyears = selectyears,
  lpjml = lpjml,
  climatetype = climatetype,
  accessibilityrule = accessibilityrule,
  efrMethod = efrMethod
)
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

- accessibilityrule:

  Method used: Quantile method (Q) or Coefficient of Variation (CV)
  combined with scalar value defining the strictness of accessibility
  restriction: discharge that is exceeded x percent of the time on
  average throughout a year (Qx, e.g. Q75: 0.25, Q50: 0.5) or base value
  for exponential curve separated by : (CV:2)

- efrMethod:

  EFR method used including selected strictness of EFRs (Smakhtin:good,
  VMF:fair)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("DischargeInaccessibleAdd", aggregate = FALSE)
} # }
```
