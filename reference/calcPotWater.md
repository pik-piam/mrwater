# calcPotWater

This function returns the potential water quantity (separted into
withdrawal and consumption) available for different uses

## Usage

``` r
calcPotWater(
  lpjml,
  selectyears,
  climatetype,
  efrMethod,
  accessibilityrule,
  rankmethod,
  yieldcalib,
  allocationrule,
  gainthreshold,
  irrigationsystem,
  iniyear,
  landScen,
  cropmix,
  comAg,
  fossilGW,
  multicropping,
  transDist
)
```

## Arguments

- lpjml:

  LPJmL version used

- selectyears:

  Years for which irrigatable area is calculated

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- efrMethod:

  EFR method used including selected strictness of EFRs (e.g.
  Smakhtin:good, VMF:fair)

- accessibilityrule:

  Strictness of accessibility restriction: discharge that is exceeded x
  percent of the time on average throughout a year (Qx). (e.g. Q75:
  0.25, Q50: 0.5)

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

- yieldcalib:

  If TRUE: LPJmL yields calibrated to FAO country yield in iniyear Also
  needs specification of refYields, separated by ":". Options: FALSE
  (for single cropping analyses) or "TRUE:actual:irrig_crop" (for
  multiple cropping analyses) If FALSE: uncalibrated LPJmL yields are
  used

- allocationrule:

  Rule to be applied for river basin discharge allocation across cells
  of river basin ("optimization", "upstreamfirst", "equality")

- gainthreshold:

  Threshold of yield improvement potential required (in USD per hectare)

- irrigationsystem:

  Irrigation system used ("surface", "sprinkler", "drip",
  "initialization")

- iniyear:

  Initialization year of irrigation system

- landScen:

  Land availability scenario consisting of two parts separated by
  ":": 1. available land scenario (currCropland, currIrrig,
  potCropland) 2. protection scenario (WDPA, or one of the scenarios
  available in calcConservationPriorities, e.g., 30by20, BH, BH_IFL,
  PBL_HalfEarth, or NA for no protection). For case of no land
  protection select "NA" in second part of argument or do not specify
  second part of the argument

- cropmix:

  Selected cropmix (options: "hist_irrig" for historical cropmix on
  currently irrigated area, "hist_total" for historical cropmix on total
  cropland, or selection of proxycrops)

- comAg:

  If TRUE: currently already irrigated areas in initialization year are
  reserved for irrigation, if FALSE: no irrigation areas reserved
  (irrigation potential)

- fossilGW:

  If TRUE: non-renewable groundwater can be used. If FALSE:
  non-renewable groundwater cannot be used.

- multicropping:

  Multicropping activated (TRUE) or not (FALSE) and Multiple Cropping
  Suitability mask selected ("endogenous": suitability for multiple
  cropping determined by rules based on grass and crop productivity
  "exogenous": suitability for multiple cropping given by GAEZ data
  set), separated by ":" (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)

- transDist:

  Water transport distance allowed to fulfill locally unfulfilled water
  demand by surrounding cell water availability

## Value

magpie object in cellular resolution

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("PotWater", aggregate = FALSE)
} # }
```
