# calcYieldgainPotential

reports yield gain potential for irrigatable area under different
scenarios

## Usage

``` r
calcYieldgainPotential(
  scenario,
  selectyears,
  iniyear,
  lpjml,
  climatetype,
  efrMethod,
  yieldcalib,
  irrigationsystem,
  accessibilityrule,
  rankmethod,
  gainthreshold,
  allocationrule,
  transDist,
  fossilGW,
  landScen,
  cropmix,
  multicropping,
  unlimited
)
```

## Arguments

- scenario:

  Non-agricultural water use and EFP scenario, separated by "." (e.g.
  "on.ssp2")

- selectyears:

  Years for which yield gain potential is calculated

- iniyear:

  Initialization year

- lpjml:

  LPJmL version used

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- efrMethod:

  EFR method used including selected strictness of EFRs (e.g.
  Smakhtin:good, VMF:fair)

- yieldcalib:

  If TRUE: LPJmL yields calibrated to FAO country yield in iniyear Also
  needs specification of refYields, separated by ":". Options: FALSE
  (for single cropping analyses) or "TRUE:actual:irrig_crop" (for
  multiple cropping analyses) If FALSE: uncalibrated LPJmL yields are
  used

- irrigationsystem:

  Irrigation system used ("surface", "sprinkler", "drip",
  "initialization")

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

- gainthreshold:

  Threshold of yield improvement potential required (in USD per hectare)

- allocationrule:

  Rule to be applied for river basin discharge allocation across cells
  of river basin ("optimization", "upstreamfirst", "equality")

- transDist:

  Water transport distance allowed to fulfill locally unfulfilled water
  demand by surrounding cell water availability

- fossilGW:

  If TRUE: non-renewable groundwater can be used. If FALSE:
  non-renewable groundwater cannot be used.

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

- unlimited:

  TRUE: no water limitation to potentially irrigated area FALSE:
  irrigatable area limited by water availability

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("YieldgainPotential", aggregate = FALSE)
} # }
```
