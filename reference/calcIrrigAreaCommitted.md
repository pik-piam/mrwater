# calcIrrigAreaCommitted

calculates area reserved for irrigation based on area irrigated in
initialization year and depreciation parameter (set to 0.1)

## Usage

``` r
calcIrrigAreaCommitted(selectyears, iniyear)
```

## Arguments

- selectyears:

  select years

- iniyear:

  initialization year

## Value

magpie object in cellular resolution

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("IrrigAreaCommitted", aggregate = FALSE)
} # }
```
