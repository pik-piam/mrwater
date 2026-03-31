# calcEnvmtlFlowRequirementsShare

This function calculates environmental flow requirements (EFR) (as share
of discharge) based on LPJmL monthly discharge

## Usage

``` r
calcEnvmtlFlowRequirementsShare(lpjml, climatetype, efrMethod)
```

## Arguments

- lpjml:

  LPJmL version used for monthly discharge

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- efrMethod:

  EFR method used including selected strictness of EFRs (e.g.
  Smakhtin:good, VMF:fair, PB:Rockstroem)

## Value

magpie object in cellular resolution representing share of discharge
that is reserved for environmental flows

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("EnvmtlFlowRequirementsShare", aggregate = FALSE)
} # }
```
