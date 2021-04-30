#' @title       calcYearlyRunoff
#' @description This function calculates yearly runoff from runoff on land and water provided by LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop. Note: Default version arguments need to be updated when new versions are used!
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
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

calcYearlyRunoff <- function(selectyears, lpjml, climatetype) {

  # Read in input data already time-smoothed and for climate scenarios harmonized to the baseline
  if (grepl("historical", climatetype)) {
    # Baseline is only smoothed (not harmonized)
    stage <- "smoothed"
  } else {
    # Climate scenarios are harmonized to baseline
    stage <- "harmonized2020"
  }

  ## Required inputs for River Routing:
  # Yearly runoff (mio. m^3 per yr) [smoothed & harmonized]
  yearly_runoff <- setYears(calcOutput("LPJmL_new", version=lpjml["natveg"], subtype="runoff",     climatetype=climatetype, stage=stage, years=selectyears, aggregate=FALSE), selectyears)

  # Precipitation/Runoff on lakes and rivers from LPJmL (in mio. m^3 per year) [smoothed & harmonized]
  input_lake    <- setYears(calcOutput("LPJmL_new", version=lpjml["natveg"], subtype="input_lake", climatetype=climatetype, stage=stage, years=selectyears, aggregate=FALSE), selectyears)

  ## Calculate Runoff (on land and water)
  out <- yearly_runoff + input_lake

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="yearly runoff",
    isocountries=FALSE))
}
