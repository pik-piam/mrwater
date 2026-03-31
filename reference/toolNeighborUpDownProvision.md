# toolNeighborUpDownProvision

This function calculates water provision by surrounding grid cells for
upstream-downstream allocation set-up

## Usage

``` r
toolNeighborUpDownProvision(rs, transDist, years, scenarios, listNeighborIN)
```

## Arguments

- rs:

  River structure including information on neighboring cells

- transDist:

  Water transport distance allowed to fulfill locally unfulfilled water
  demand by surrounding cell water availability

- years:

  Vector of years for which neighbor allocation shall be applied

- scenarios:

  Vector of scenarios for which neighbor allocation shall be applied

- listNeighborIN:

  List of arrays required for the algorithm: yearlyRunoff, lakeEvap
  reserved flows from previous water allocation round (prevReservedWC,
  prevReservedWW) missing water at this stage of water allocation
  (missingWW, missingWC) discharge

## Value

magpie object in cellular resolution

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("RiverHumanUseAccounting", aggregate = FALSE)
} # }
```
