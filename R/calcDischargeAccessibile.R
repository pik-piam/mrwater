#' @title       calcDischargeInaccessibile
#' @description This function calculates the discharge that is inaccessible to humans
#'              based on the variability of monthly flows and natural discharge.
#'
#' @param climatetype          Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears          Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param variabilitythreshold Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#'
#' @importFrom magclass collapseNames
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution representing share of discharge that is reserved for environmental flows
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("DischargeAccessibile", aggregate=FALSE) }
#'

calcDischargeInaccessibile <- function(selectyears, climatetype, variabilitythreshold) {

  # Discharge Accessibility Share
  access_shr <- calcOutput("DischargeAccessibilityShare", selectyears=selectyears, climatetype=climatetype, variabilitythreshold=variabilitythreshold, aggregate=FALSE)

  # Read in natural discharge (in mio. m^3 / yr)
  discharge_nat   <- collapseNames(calcOutput("RiverNaturalFlows", selectyears=selectyears, climatetype=climatetype, aggregate=FALSE)[,,"discharge_nat"])

  # Calculate discharge accessible to humans (mio. m^3 / yr)
  out <- (1-access_shr) * discharge_nat

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA inaccessible discharge")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="Discharge that is inaccessible per cell per year",
    isocountries=FALSE))
}
