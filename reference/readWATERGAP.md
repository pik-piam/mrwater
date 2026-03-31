# readWATERGAP

Read in non-agricultural water demand data from WATERGAP model

## Usage

``` r
readWATERGAP(subtype = "WATCH_ISIMIP_WATERGAP")
```

## Arguments

- subtype:

  Data source to be read from

## Value

MAgPIE object of non-agricultural water demand at 0.5 cellular level in
mio. m^3

## Author

Felicitas Beier, Abhijeet Mishra

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("WATERGAP", convert = "onlycorrect")
} # }
```
