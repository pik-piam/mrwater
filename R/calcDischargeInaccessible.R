#' @title       calcDischargeInaccessible
#' @description This function calculates the discharge that is inaccessible to humans
#'              based on the variability of monthly flows and natural discharge.
#'
#' @param lpjml             LPJmL version required for respective inputs: natveg or crop
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears       Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @importFrom magclass collapseNames
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution representing discharge
#'         that is inaccessible to humans
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("DischargeInaccessible", aggregate = FALSE)
#' }
#'
calcDischargeInaccessible <- function(lpjml, selectyears, climatetype, accessibilityrule) {

  # Discharge Accessibility Share
  access_shr <- calcOutput("DischargeAccessibilityShare", selectyears = selectyears,
                           lpjml = lpjml, climatetype = climatetype,
                           accessibilityrule = accessibilityrule, aggregate = FALSE)

  # Read in natural discharge (in mio. m^3 / yr)
  discharge_nat   <- collapseNames(calcOutput("RiverNaturalFlows", selectyears = selectyears,
                                              lpjml = lpjml, climatetype = climatetype,
                                              aggregate = FALSE)[, , "discharge_nat"])

  # Calculate discharge accessible to humans (mio. m^3 / yr)
  out <- (1 - access_shr) * discharge_nat

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA inaccessible discharge")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "Discharge that is inaccessible per cell per year",
              isocountries = FALSE))
}
