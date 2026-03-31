# calcActualIrrigWatRequirements

This function calculates actual irrigation water requirements per cell
given the chosen irrigation system

## Usage

``` r
calcActualIrrigWatRequirements(
  selectyears,
  iniyear,
  lpjml,
  climatetype,
  irrigationsystem,
  multicropping
)
```

## Arguments

- selectyears:

  Years to be returned

- iniyear:

  Initialization year (for weight by cropland)

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- irrigationsystem:

  Irrigation system used: system share as in initialization year, or
  drip, surface, sprinkler for full irrigation by selected system

- multicropping:

  Multicropping activated (TRUE) or not (FALSE) and Multiple Cropping
  Suitability mask selected (mask can be: "none": no mask applied (only
  for development purposes) "actual:total": currently multicropped areas
  calculated from total harvested areas and total physical areas per
  cell from readLandInG "actual:crop" (crop-specific),
  "actual:irrigation" (irrigation-specific), "actual:irrig_crop" (crop-
  and irrigation-specific) "total" "potential:endogenous": potentially
  multicropped areas given temperature and productivity limits
  "potential:exogenous": potentially multicropped areas given GAEZ
  suitability classification) (e.g. TRUE:actual:total; TRUE:none; FALSE)

## Value

magpie object in cellular resolution

## See also

[`calcIrrigationSystem`](calcIrrigationSystem.md),
[`calcIrrigWatRequirements`](calcIrrigWatRequirements.md)

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ActualIrrigWatRequirements", aggregate = FALSE)
} # }
```
