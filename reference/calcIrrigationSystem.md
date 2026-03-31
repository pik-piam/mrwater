# calcIrrigationSystem

This function returns the irrigation system share initialization

## Usage

``` r
calcIrrigationSystem(datasource)
```

## Arguments

- datasource:

  Data source to be used: Jaegermeyr (irrigation system share based on
  FAO 2014, ICID 2012 and Rohwer et al. 2007) or LPJmL (dominant
  irrigation system per country)

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("IrrigationSystem", datasource = "Jaegermeyr", aggregate = FALSE)
} # }
```
