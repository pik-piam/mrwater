#' @title       calcEnvmtlFlowRequirementConstraint
#' @description This function calculates environmental flow requirements (EFR)
#'              that go beyond the requirements that can be fulfilled by
#'              inaccessible discharge (i.e. that constrain human uses)
#'              based on EFRs and inaccessible discharge calculated from
#'              LPJmL monthly discharge
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years to be returned
#'                          (Note: does not affect years of harmonization or smoothing)
#' @param climatetype       Switch between different climate scenarios
#'                          or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod         EFR method used including selected strictness of EFRs
#'                          (Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or
#'                          Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @importFrom magclass collapseNames as.magpie
#' @importFrom madrat calcOutput
#'
#' @return magpie object with EFRs, LFRs and HFRs in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("EnvmtlFlowRequirementConstraint", aggregate = FALSE)
#' }
#'
calcEnvmtlFlowRequirementsConstraint <- function(lpjml, selectyears, climatetype,
                                                 EFRmethod, accessibilityrule) {

  # Read in full volume of EFRs (in mio. m^3) including HFRs and LFRs
  EFR <- calcOutput("EnvmtlFlowRequirements", selectyears = selectyears,
                    climatetype = climatetype, lpjml = lpjml, EFRmethod = EFRmethod,
                    aggregate = FALSE)

  # Read in Inaccessible discharge (in mio. m^3)
  dischargeInaccess <- calcOutput("DischargeInaccessible", selectyears = selectyears,
                                  lpjml = lpjml, climatetype = climatetype,
                                  accessibilityrule = accessibilityrule, aggregate = FALSE)

  # Calculate HFRs that exceed inaccessible discharge
  constrainingHFR <- pmax(collapseNames(EFR[, , "HFR"]) - dischargeInaccess, 0)

  # Calculate EFRs (mio. m^3 / yr)
  out <- collapseNames(EFR[, , "LFR"]) + constrainingHFR

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA EFR in calcEFRconstraint")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "Environmental flow requirements per cell per year",
              isocountries = FALSE))
}
