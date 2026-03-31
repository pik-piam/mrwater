# calcAvlWater

This function calculates water availability for MAgPIE retrieved from
LPJmL

## Usage

``` r
calcAvlWater(
  lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop =
    "ggcmi_phase3_nchecks_9ca735cb"),
  climatetype = "GSWP3-W5E5:historical",
  cells = "lpjcell",
  stage = "harmonized2020",
  seasonality = "grper"
)
```

## Arguments

- lpjml:

  Defines LPJmL version for crop/grass and natveg specific inputs

- climatetype:

  Switch between different climate scenarios

- cells:

  Number of cells to be returned (select "magpiecell" for 59199 cells or
  "lpjcell" for 67420 cells)

- stage:

  Degree of processing: raw, smoothed, harmonized, harmonized2020

- seasonality:

  grper (default): water available in growing period per year; total:
  total water available throughout the year; monthly: monthly water
  availability (for further processing, e.g. in calcEnvmtlFlow)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier, Kristine Karstens, Abhijeet Mishra

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("AvlWater", aggregate = FALSE)
} # }
```
