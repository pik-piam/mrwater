# toolRiverDischargeUpdate

This function calculates cellular discharge after reserving water uses
for consumption

## Usage

``` r
toolRiverDischargeUpdate(rs, runoffWOEvap, watCons)
```

## Arguments

- rs:

  River structure

- runoffWOEvap:

  Array that contains (runoff - lake evap)

- watCons:

  Array that contains water reserved for consumptive use

## Value

array in cellular resolution and all year and scenario dimensions

## Author

Felicitas Beier, Jens Heinke

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("RiverHumanUseAccounting", aggregate = FALSE)
} # }
```
