# calcGrassET

Calculates evapotranspiration (ET) of grassland under irrigated and
rainfed conditions based on LPJmL inputs.

## Usage

``` r
calcGrassET(selectyears, lpjml, climatetype, season)
```

## Arguments

- selectyears:

  Years to be returned

- lpjml:

  LPJmL version required for respective inputs: natveg or crop

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- season:

  "wholeYear": grass et in the entire year (main + off season)
  "mainSeason": grass etP in the crop-specific growing period of LPJmL
  (main season)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("GrassET", aggregate = FALSE)
} # }
```
