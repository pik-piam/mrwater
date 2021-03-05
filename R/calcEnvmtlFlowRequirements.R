#' @title       calcEnvmtlFlowRequirements
#' @description This function calculates environmental flow requirements (EFR) for MAgPIE based on EFR share calculated from LPJmL monthly discharge following Smakthin et al. (2004)
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#'
#' @importFrom magclass collapseNames as.magpie
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("EnvmtlFlowRequirements", aggregate=FALSE) }
#'

calcEnvmtlFlowRequirements <- function(selectyears, climatetype="GSWP3-W5E5:historical") {

  # Read in share of discharge to be reserved for environment (per cell)
  EFR_magpie_frac <- calcOutput("EnvmtlFlowRequirementsShare", climatetype=climatetype, aggregate=FALSE)

  # Read in natural discharge (in mio. m^3 / yr)
  discharge_nat   <- collapseNames(calcOutput("RiverNaturalFlows", selectyears=selectyears, climatetype=climatetype, aggregate=FALSE)[,,"discharge_nat"])

  # Calculate EFRs (mio. m^3 / yr)
  EFR <- EFR_magpie_frac * discharge_nat

  # Check for NAs
  if (any(is.na(EFR))) {
    stop("produced NA EFR")
  }

  return(list(
    x=EFR,
    weight=NULL,
    unit="mio. m^3",
    description="Environmental flow requirements per cell per year",
    isocountries=FALSE))
}
