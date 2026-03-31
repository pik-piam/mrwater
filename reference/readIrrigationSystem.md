# readIrrigationSystem

Read in irrigation system type for initialization

## Usage

``` r
readIrrigationSystem(subtype = "Jaegermeyr")
```

## Arguments

- subtype:

  Data source to be used: Jaegermeyr (irrigation system share based on
  FAO 2014, ICID 2012 and Rohwer et al. 2007) or LPJmL (dominant
  irrigation system per country)

## Value

MAgPIE object of at country-level

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("IrrigationSystem", convert = FALSE)
} # }
```
