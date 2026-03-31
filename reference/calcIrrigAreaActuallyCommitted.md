# calcIrrigAreaActuallyCommitted

calculates area reserved for irrigation based on area irrigated in
initialization and available water resources

## Usage

``` r
calcIrrigAreaActuallyCommitted(
  lpjml,
  climatetype,
  selectyears,
  iniyear,
  efrMethod,
  fossilGW,
  multicropping,
  transDist
)
```

## Arguments

- lpjml:

  LPJmL version used

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- selectyears:

  Years to be returned (Note: does not affect years of harmonization or
  smoothing)

- iniyear:

  Initialization year of irrigation system

- efrMethod:

  EFR method used including selected strictness of EFRs (Smakhtin:good,
  VMF:fair)

- fossilGW:

  If TRUE: non-renewable groundwater can be used. If FALSE:
  non-renewable groundwater cannot be used.

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
calcOutput("IrrigAreaActuallyCommitted", aggregate = FALSE)
} # }
```
