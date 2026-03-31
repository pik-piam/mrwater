# fullSIMPLE

Function that produces gridded outputs for usage in the SIMPLE model
(Baldos & Hertel 2012) and the SIMPLE-G model (Baldos et al. 2020)

## Usage

``` r
fullSIMPLE(
  transDist = 100,
  fossilGW = TRUE,
  allocationrule = "optimization",
  rankmethod = "USD_m3:GLO:TRUE"
)
```

## Arguments

- transDist:

  Water transport distance allowed to fulfill locally unfulfilled water
  demand by surrounding cell water availability

- fossilGW:

  If TRUE: non-renewable groundwater can be used. If FALSE:
  non-renewable groundwater cannot be used.

- allocationrule:

  Rule to be applied for river basin discharge allocation across cells
  of river basin ("optimization", "upstreamfirst")

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
