# calcWaterUseNonAg

This function extracts non-agricultural water demand

## Usage

``` r
calcWaterUseNonAg(
  selectyears = seq(1995, 2100, by = 5),
  cells = "lpjcell",
  datasource = "WATCH_ISIMIP_WATERGAP",
  usetype = "all",
  seasonality = "grper",
  harmonType = "average",
  lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop =
    "ggcmi_phase3_nchecks_9ca735cb"),
  climatetype = "GSWP3-W5E5:historical"
)
```

## Arguments

- selectyears:

  Years to be returned

- cells:

  Number of cells to be returned (select "magpiecell" for 59199 cells or
  "lpjcell" for 67420 cells)

- datasource:

  Data source to be used (e.g. WATERGAP2020)

- usetype:

  water use types (domestic, industry, electricity) and option to return
  withdrawals or consumption separated by ":" (e.g. "all:withdrawal")
  options for first argument: "total" (returns the sum over different
  water use types) or "all" ( returns all water use types (domestic,
  industry, electricity)) options for second argument: "all",
  "withdrawal", "consumption"

- seasonality:

  grper (default): non-agricultural water demand in growing period per
  year; total: non-agricultural water demand throughout the year

- harmonType:

  Type of time smoothing: average (average over 8-year time span around
  baseline year) or spline (time smoothing using spline method with 4
  degrees of freedom) or NULL (no smoothing)

- lpjml:

  Defines LPJmL version for crop/grass and natveg specific inputs

- climatetype:

  Switch between different climate scenarios for calcGrowingPeriod

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("WaterUseNonAg", aggregate = FALSE)
} # }
```
