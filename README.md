# madrat based MAgPIE Input Data Library

R package **mrwater**, version **0.4.62**

[![CRAN status](https://www.r-pkg.org/badges/version/mrwater)](https://cran.r-project.org/package=mrwater)   [![R build status](https://github.com/pik-piam/mrwater/workflows/check/badge.svg)](https://github.com/pik-piam/mrwater/actions) [![codecov](https://codecov.io/gh/pik-piam/mrwater/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/mrwater)

## Purpose and Functionality

Provides functions for MAgPIE cellular input data generation and stand-alone water calculations.


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

## Questions / Problems

In case of questions / problems please contact Felicitas Beier <beier@pik-potsdam.de>.

## Citation

To cite package **mrwater** in publications use:

Beier F, Heinke J, Dietrich J (2021). _mrwater: madrat based MAgPIE Input Data Library_. R package version 0.4.62.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrwater: madrat based MAgPIE Input Data Library},
  author = {Felicitas Beier and Jens Heinke and Jan Philipp Dietrich},
  year = {2021},
  note = {R package version 0.4.62},
}
```

