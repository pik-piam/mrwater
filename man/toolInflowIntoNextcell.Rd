% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolInflowIntoNextcell.R
\name{toolInflowIntoNextcell}
\alias{toolInflowIntoNextcell}
\title{toolInflowIntoNextcell}
\usage{
toolInflowIntoNextcell(cell, previous_inflow, cell_discharge)
}
\arguments{
\item{cell}{current cell or cells that are calculated in this step of the loop}

\item{previous_inflow}{inflow into nextcell}

\item{cell_discharge}{discharge in current cell}
}
\value{
magpie object
}
\description{
calculates inflow into nextcell for several cells with same
calcorder accounting for fact that one cell can discharge into same nextcell
}
\author{
Felicitas Beier
}
