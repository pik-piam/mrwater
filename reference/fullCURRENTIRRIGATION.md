# fullCURRENTIRRIGATION

Function that produces output for irrigation potentials under multiple
cropping on cellular resolution.

## Usage

``` r
fullCURRENTIRRIGATION(yieldcalib = "TRUE:TRUE:actual:irrig_crop")
```

## Arguments

- yieldcalib:

  If TRUE: LPJmL yields calibrated to FAO country yield in iniyear Also
  needs specification of refYields, separated by ":". Options: FALSE
  (for single cropping analyses) or "TRUE:actual:irrig_crop" (for
  multiple cropping analyses) If FALSE: uncalibrated LPJmL yields are
  used

## Author

Felicitas Beier
