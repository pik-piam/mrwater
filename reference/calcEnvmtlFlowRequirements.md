# calcEnvmtlFlowRequirements

This function calculates environmental flow requirements (EFR) based on
EFR share calculated from LPJmL monthly discharge

## Usage

``` r
calcEnvmtlFlowRequirements(lpjml, selectyears, climatetype, efrMethod)
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

## Value

magpie object with EFRs, LFRs and HFRs in cellular resolution

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("EnvmtlFlowRequirements", aggregate = FALSE)
} # }
```
