# downloadJaegermeyr2015

Downloads irrigation system suitability per crop type and country-level
irrigation system shares from Jaegermeyr (2015)

## Usage

``` r
downloadJaegermeyr2015(subtype)
```

## Arguments

- subtype:

  Data to be downloaded: "systemShare": irrigation system share as
  provided in SI of Jaegermeyr et al. (2015), based on FAO 2014, ICID
  2012 and Rohwer et al. 2007); "systemSuitability": biophysical and
  technical irrigation system suitability by crop type (CFT) as provided
  in Table 2 of Jaegermeyr et al. (2015) based on Sauer et al. (2010)
  and Fischer et al (2012).

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("Jaegerymeyr2015", convert = FALSE)
} # }
```
