# calcWaterUseActuallyCommittedAg

This function calculates committed agricultural water uses that are used
in the river routing algorithm for distributing available water across
the basin

## Usage

``` r
calcWaterUseActuallyCommittedAg(
  iteration = "committed_agriculture",
  lpjml,
  climatetype,
  selectyears,
  iniyear,
  multicropping,
  efrMethod,
  fossilGW,
  transDist
)
```

## Arguments

- iteration:

  Default: "committed_agriculture", Special case:
  "committed_agriculture_fullPotential". Special case should only be
  used for calculation of full multicropping potential committed
  agricultural area for case of Current Irrigation.

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- selectyears:

  Years to be returned

- iniyear:

  Year of initialization for cropland area

- multicropping:

  Multicropping activated (TRUE) or not (FALSE) and Multiple Cropping
  Suitability mask selected (mask can be: "none": no mask applied (only
  for development purposes) "actual:total": currently multicropped areas
  calculated from total harvested areas and total physical areas per
  cell from LandInG "actual:crop" (crop-specific), "actual:irrigation"
  (irrigation-specific), "actual:irrig_crop" (crop- and
  irrigation-specific) "total" "potential:endogenous": potentially
  multicropped areas given temperature and productivity limits
  "potential:exogenous": potentially multicropped areas given GAEZ
  suitability classification) (e.g. TRUE:actual:total; TRUE:none; FALSE)

- efrMethod:

  EFR method used including selected strictness of EFRs (e.g.
  Smakhtin:good, VMF:fair)

- fossilGW:

  If TRUE: non-renewable groundwater can be used. If FALSE:
  non-renewable groundwater cannot be used.

- transDist:

  Water transport distance allowed to fulfill locally unfulfilled water
  demand by surrounding cell water availability

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("WaterUseActuallyCommittedAg", aggregate = FALSE)
} # }
```
