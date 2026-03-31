# calcBlueWaterConsumption

This function calculates consumptive blue water use for the whole year
based on LPJmL blue water consumption of crops and the difference
between rainfed and irrigated evapotranspiration of grass

## Usage

``` r
calcBlueWaterConsumption(
  selectyears,
  lpjml,
  climatetype,
  fallowFactor = 0.75,
  areaMask,
  output
)
```

## Arguments

- selectyears:

  Years to be returned

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- climatetype:

  Climate model (e.g., "MRI-ESM2-0:ssp370") or historical baseline
  (e.g., "GSWP3-W5E5:historical")

- fallowFactor:

  Factor determining water requirement reduction in off season due to
  fallow period between harvest of first (main) season and sowing of
  second (off) season

- areaMask:

  Multicropping area mask to be used "none": no mask applied (only for
  development purposes) "actual:total": currently multicropped areas
  calculated from total harvested areas and total physical areas per
  cell from readLandInG "actual:crop" (crop-specific),
  "actual:irrigation" (irrigation-specific), "actual:irrig_crop" (crop-
  and irrigation-specific) "total" "potential:endogenous": potentially
  multicropped areas given temperature and productivity limits
  "potential:exogenous": potentially multicropped areas given GAEZ
  suitability classification

- output:

  output to be returned by the function: combination of crop type
  ("crops" or "grass") and season ("main" (LPJmL growing period), "year"
  (entire year)), separated by ":" ("crops:main", "crops:year",
  "grass:main", "grass:year")

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("BlueWaterConsumption", aggregate = FALSE)
} # }
```
