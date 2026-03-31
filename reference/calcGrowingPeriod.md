# calcGrowingPeriod

This function determines a mean sowing date and a mean growing period
for each cell in order to determine when irrigation can take place.

## Usage

``` r
calcGrowingPeriod(
  lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop =
    "ggcmi_phase3_nchecks_9ca735cb"),
  climatetype = "GSWP3-W5E5:historical",
  stage = "harmonized2020",
  yield_ratio = 0.1,
  cells = "lpjcell"
)
```

## Arguments

- lpjml:

  Defines LPJmL version for crop/grass and natveg specific inputs

- climatetype:

  Switch between different climate scenarios

- stage:

  Degree of processing: raw, smoothed, harmonized, harmonized2020

- yield_ratio:

  threshold for cell yield over global average. crops in cells below
  threshold will be ignored

- cells:

  Number of cells to be returned (select "magpiecell" for 59199 cells or
  "lpjcell" for 67420 cells)

## Value

magpie object in cellular resolution

## Author

Kristine Karstens, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("GrowingPeriod", aggregate = FALSE)
} # }
```
