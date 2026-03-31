# calcEnvmtlFlowRequirementsInaccess

This function calculates environmental flow requirements (EFR) that are
inaccessible to humans based on EFRs and inaccessible discharge
calculated from LPJmL monthly discharge

## Usage

``` r
calcEnvmtlFlowRequirementsInaccess(
  lpjml,
  selectyears,
  climatetype,
  efrMethod,
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

- efrMethod:

  EFR method used including selected strictness of EFRs (Smakhtin:good,
  VMF:fair)

- accessibilityrule:

  Method used: Quantile method (Q) or Coefficient of Variation (CV)
  combined with scalar value defining the strictness of accessibility
  restriction: discharge that is exceeded x percent of the time on
  average throughout a year (Qx, e.g. Q75: 0.25, Q50: 0.5) or base value
  for exponential curve separated by : (CV:2)

## Value

magpie object with EFRs, LFRs and HFRs in cellular resolution

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("calcEnvmtlFlowRequirementsInaccess", aggregate = FALSE)
} # }
```
