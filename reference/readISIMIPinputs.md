# readISIMIPinputs

Read in non-agricultural water demand data from ISIMIP inputs

## Usage

``` r
readISIMIPinputs(subtype = "ISIMIP3b:water:histsoc.waterabstraction")
```

## Arguments

- subtype:

  Data source to be read from including path separated by ":", subtype
  separated by "."

## Value

MAgPIE object of non-agricultural water demand at 0.5 cellular level in
mio. m^3

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("ISIMIPinputs",
subtype = "ISIMIP3b:water:histsoc.waterabstraction",
convert = "onlycorrect")
} # }
```
