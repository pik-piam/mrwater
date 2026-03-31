# calcRiverRoutingInputs

This function collects inputs necessary for the different river routings
depending on the chosen settings

## Usage

``` r
calcRiverRoutingInputs(
  lpjml,
  climatetype,
  selectyears,
  iniyear,
  iteration,
  transDist,
  efrMethod,
  accessibilityrule,
  multicropping,
  comAg,
  rankmethod,
  gainthreshold,
  cropmix,
  yieldcalib,
  irrigationsystem,
  landScen
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

- iteration:

  Water use to be allocated in this river routing iteration
  (non_agriculture, committed_agriculture, potential_irrigation,
  committed_agriculture_fullMulticropping).

- transDist:

  Water transport distance allowed to fulfill locally unfulfilled water
  demand by surrounding cell water availability

- efrMethod:

  EFR method used including selected strictness of EFRs (e.g.
  Smakhtin:good, VMF:fair)

- accessibilityrule:

  Water accessibility rule. Available methods: Quantile method (Q) or
  Coefficient of Variation (CV); combined with scalar value defining the
  strictness of accessibility restriction: discharge that is exceeded x
  percent of the time on average throughout a year (Qx, e.g. Q75: 0.25,
  Q50: 0.5) or base value for exponential curve separated by : (CV:2)

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

- comAg:

  if TRUE: the currently already irrigated areas in initialization year
  are reserved for irrigation, if FALSE: no irrigation areas reserved
  (irrigation potential). Only relevant for iteration = potential

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

- gainthreshold:

  Threshold of yield improvement potential (in USD per hectare)

- cropmix:

  Selected cropmix (options: "hist_irrig" for historical cropmix on
  currently irrigated area, "hist_total" for historical cropmix on total
  cropland, or selection of proxycrops)

- yieldcalib:

  If TRUE: LPJmL yields calibrated to FAO country yield in iniyear Also
  needs specification of refYields, separated by ":". Options: FALSE
  (for single cropping analyses) or "TRUE:actual:irrig_crop" (for
  multiple cropping analyses) If FALSE: uncalibrated LPJmL yields are
  used

- irrigationsystem:

  Irrigation system to be used for river basin discharge allocation
  algorithm ("surface", "sprinkler", "drip", "initialization")

- landScen:

  Land availability scenario consisting of two parts separated by
  ":": 1. available land scenario (currCropland, currIrrig,
  potCropland) 2. protection scenario (WDPA, or one of the scenarios
  available in calcConservationPriorities, e.g., 30by20, BH, BH_IFL,
  PBL_HalfEarth, or NA for no protection). For case of no land
  protection select "NA" in second part of argument or do not specify
  second part of the argument

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("RiverRoutingInputs", aggregate = FALSE)
} # }
```
