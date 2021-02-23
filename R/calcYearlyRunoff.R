#' @title       calcYearlyRunoff
#' @description This function calculates yearly runoff from runoff on land and water provided by LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("YearlyRunoff", aggregate = FALSE) }
#'

calcYearlyRunoff <- function(selectyears="all", version="LPJmL4", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015") {

  ### Internal function: read in LPJmL data
  .getLPJmLData <- function(subtype, cfg) {
    x <- collapseNames(calcOutput("LPJmL", version=cfg$version, selectyears=cfg$selectyears,
                                  climatetype=cfg$climatetype, harmonize_baseline=cfg$harmonize_baseline, ref_year=cfg$ref_year, time=cfg$time, dof=cfg$dof, averaging_range=cfg$averaging_range,
                                  subtype=subtype, aggregate=FALSE))
    return(x)
  }

  ### Required inputs for River Routing:
  ## LPJmL water data
  cfg <- list(selectyears=selectyears, version=version, climatetype=climatetype,
              harmonize_baseline=harmonize_baseline, ref_year=ref_year,
              time=time, dof=dof, averaging_range=averaging_range)
  # Yearly runoff (mio. m^3 per yr) [smoothed & harmonized]
  yearly_runoff <- .getLPJmLData("runoff_lpjcell",     cfg)
  # Precipitation/Runoff on lakes and rivers from LPJmL (in mio. m^3 per year) [smoothed & harmonized]
  input_lake    <- .getLPJmLData("input_lake_lpjcell", cfg)

  # Calculate Runoff (on land and water)
  out <- yearly_runoff + input_lake

  # Correct dimension and names (NOTE: only until fully switched to standard of coordinate names)
  out <- addLocation(out)
  out <- collapseDim(out, dim=c("N", "region1"))
  out <- collapseDim(out, dim="iso")

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="yearly runoff",
    isocountries=FALSE))
}
