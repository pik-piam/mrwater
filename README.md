# madrat based MAgPIE water Input Data Library

R package **mrwater**, version **1.3.3**

[![CRAN status](https://www.r-pkg.org/badges/version/mrwater)](https://cran.r-project.org/package=mrwater)  [![R build status](https://github.com/pik-piam/mrwater/workflows/check/badge.svg)](https://github.com/pik-piam/mrwater/actions) [![codecov](https://codecov.io/gh/pik-piam/mrwater/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrwater) [![r-universe](https://pik-piam.r-universe.dev/badges/mrwater)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

Provides functions for MAgPIE cellular input data generation 
             and stand-alone water calculations.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrwater")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("mrwater")        # mrwater Documentation
vignette("riverstructure") # mrwater River Structure
```

## Questions / Problems

In case of questions / problems please contact Felicitas Beier <beier@pik-potsdam.de>.

## Citation

To cite package **mrwater** in publications use:

Beier F, Heinke J, Karstens K, Bodirsky B, Dietrich J (2022). _mrwater: madrat based MAgPIE water Input Data Library_. R package version 1.3.3, <URL: https://github.com/pik-piam/mrwater>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrwater: madrat based MAgPIE water Input Data Library},
  author = {Felicitas Beier and Jens Heinke and Kristine Karstens and Benjamin Leon Bodirsky and Jan Philipp Dietrich},
  year = {2022},
  note = {R package version 1.3.3},
  url = {https://github.com/pik-piam/mrwater},
}
```
