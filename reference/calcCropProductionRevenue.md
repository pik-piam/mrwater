# calcCropProductionRevenue

calculates irrigated and rainfed production quantities or revenue on
given crop areas under selected management scenario. It can return
quantities in terms of the sum of crop biomass produced in tDM or
production revenue in terms of monetary revenue achieved in USD

## Usage

``` r
calcCropProductionRevenue(
  outputtype,
  scenario,
  management,
  area,
  lpjml,
  climatetype,
  selectyears,
  iniyear,
  efrMethod,
  accessibilityrule,
  rankmethod,
  yieldcalib,
  allocationrule,
  gainthreshold,
  irrigationsystem,
  cropmix,
  comAg,
  transDist,
  fossilGW
)
```

## Arguments

- outputtype:

  "biomass": returns the production quantity in terms of crop biomass
  (in tDM) "revenue": returns the revenue in terms of price x quantity
  of crop production (in USD)

- scenario:

  EFP and non-agricultural water use scenario separated with a "." (e.g.
  "on.ssp2")

- management:

  management in terms of irrigation and multiple cropping practices
  consisting of multiple cropping scenario ("single", actMC", "potMC")
  and ("potential", "counterfactual") separated by ":" (e.g.
  "single:potential", "actMC:counterfactual") Multiple cropping
  practices: "single": single cropping assumed everywhere "actMC":
  multiple cropping as reported by LandInG data "potMC": multiple
  cropping practiced everywhere where it is possible Irrigation
  assumption: "potential": irrigation practiced where possible according
  to irrigation potentials or practiced according to area information
  (see param \`area\`) "counterfactual": production and revenue for
  counterfactual case that irrigated areas were managed under rainfed
  conditions

- area:

  area scenario "actual": current cropland and currently irrigated areas
  or potentially irrigated areas following land availability defined in
  landScen scenario: consisting of two parts separated by ":": 1.
  available land scenario (currCropland, currIrrig, potCropland) 2.
  protection scenario (WDPA, or one of the scenarios available in
  calcConservationPriorities, e.g., 30by20, BH, BH_IFL, PBL_HalfEarth,
  or NA for no protection). For case of no land protection select "NA"
  in second part of argument or do not specify second part of the
  argument

- lpjml:

  LPJmL version used

- climatetype:

  Switch between different climate scenarios or historical baseline
  "GSWP3-W5E5:historical"

- selectyears:

  Years for which irrigatable area is calculated

- iniyear:

  Initialization year for initial croparea

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

- cropmix:

  Selected cropmix (options: "hist_irrig" for historical cropmix on
  currently irrigated area, "hist_total" for historical cropmix on total
  cropland, "hist_rainf" for historical rainfed cropmix, or selection of
  proxycrops)

- comAg:

  If TRUE: currently already irrigated areas in initialization year are
  reserved for irrigation, if FALSE: no irrigation areas reserved
  (irrigation potential)

- transDist:

  Water transport distance allowed to fulfill locally unfulfilled water
  demand by surrounding cell water availability

- fossilGW:

  If TRUE: non-renewable groundwater can be used. If FALSE:
  non-renewable groundwater cannot be used.

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("CropProductionRevenue", aggregate = FALSE)
} # }
```
