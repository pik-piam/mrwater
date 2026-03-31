# fullTRANSPORT

Function that produces output for analysis of water transport for
provision to cells in surrounding

## Usage

``` r
fullTRANSPORT(multicropping, rankmethod = "USD_ha:GLO:TRUE")
```

## Arguments

- multicropping:

  Multicropping activated (TRUE) or not (FALSE) and Multiple Cropping
  Suitability mask selected ("endogenous": suitability for multiple
  cropping determined by rules based on grass and crop productivity
  "exogenous": suitability for multiple cropping given by GAEZ data
  set), separated by ":" (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)

- rankmethod:

  Rank and optimization method consisting of Unit according to which
  rank is calculated: USD_ha (USD per hectare) for relative area return,
  or USD_m3 (USD per cubic meter) for relative volumetric return; USD
  for absolute return (total profit); Price aggregation: "GLO" for
  global average prices, or "ISO" for country-level prices and boolean
  indicating fullpotential (TRUE, i.e. cell receives full irrigation
  requirements in total area) or reduced potential (FALSE, reduced
  potential of cell receives at later stage in allocation algorithm);
  separated by ":"

## Author

Felicitas Beier
