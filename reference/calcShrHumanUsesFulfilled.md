# calcShrHumanUsesFulfilled

calculates of share of current non-agricultural and irrigation that can
be fulfilled given renewable water availability of the algorithm

## Usage

``` r
calcShrHumanUsesFulfilled(
  lpjml,
  climatetype,
  transDist,
  multicropping,
  selectyears,
  iniyear,
  efrMethod
)
```

## Arguments

- lpjml:

  LPJmL version used

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- transDist:

  Water transport distance allowed to fulfill locally unfulfilled water
  demand by surrounding cell water availability

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

- selectyears:

  Years to be returned

- iniyear:

  Initialization year

- efrMethod:

  EFR method used including selected strictness of EFRs (e.g.
  Smakhtin:good, VMF:fair)

## Value

cellular magpie object

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ShrHumanUsesFulfilled", aggregate = FALSE)
} # }
```
