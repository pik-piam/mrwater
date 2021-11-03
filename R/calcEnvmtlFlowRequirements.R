#' @title       calcEnvmtlFlowRequirements
#' @description This function calculates environmental flow requirements (EFR)
#'              based on EFR share calculated from LPJmL monthly discharge
#'
#' @param lpjml       LPJmL version used
#' @param selectyears Years to be returned
#'                    (Note: does not affect years of harmonization or smoothing)
#' @param climatetype Switch between different climate scenarios
#'                    or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod   EFR method used including selected strictness of EFRs
#'                    (Smakhtin:good, VMF:fair)
#'
#' @importFrom magclass collapseNames as.magpie
#' @importFrom madrat calcOutput
#'
#' @return magpie object with EFRs, LFRs and HFRs in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("EnvmtlFlowRequirements", aggregate = FALSE)
#' }
#'
calcEnvmtlFlowRequirements <- function(lpjml, selectyears, climatetype, efrMethod) {

  # Read in share of discharge to be reserved for environment (per cell)
  efrShare     <- calcOutput("EnvmtlFlowRequirementsShare", lpjml = lpjml,
                              climatetype = climatetype, efrMethod = efrMethod, aggregate = FALSE)

  # Read in natural discharge (in mio. m^3 / yr)
  natDischarge <- collapseNames(calcOutput("RiverNaturalFlows", lpjml = lpjml,
                                            selectyears = selectyears, climatetype = climatetype,
                                            aggregate = FALSE)[, , "discharge_nat"])

  # Calculate EFRs (mio. m^3 / yr)
  out <- efrShare * natDischarge

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA EFR, LFR or HFR")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "Environmental flow requirements per cell per year",
              isocountries = FALSE))
}
