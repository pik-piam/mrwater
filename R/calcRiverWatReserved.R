#' @title       calcRiverWatReserved
#' @description This function calculates water that is reserved for the environment,
#'              other human uses or is inaccessible and cannot be withdrawn for
#'              irrigation
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years to be returned
#'                          (Note: does not affect years of harmonization or smoothing)
#' @param climatetype       Switch between different climate scenarios
#'                          or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear           Initialization year of irrigation system
#' @param EFRmethod         EFR method used including selected strictness of EFRs
#'                          (Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param com_ag            If TRUE: currently already irrigated areas in initialization
#'                                   year are reserved for irrigation,
#'                          if FALSE: no irrigation areas are reserved (full irrigation potential)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#' @import mrmagpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverWatReserved", aggregate = FALSE)
#' }
#'
calcRiverWatReserved <- function(selectyears, iniyear, lpjml, climatetype,
                                 EFRmethod, accessibilityrule, com_ag) {

  # Discharge that is inaccessible to human uses (mio m^3)
  inaccessibleDischarge <- calcOutput("DischargeInaccessible", selectyears = selectyears,
                                       lpjml = lpjml, climatetype = climatetype,
                                       accessibilityrule = accessibilityrule, aggregate = FALSE)

  # Environmental flow requirements beyond already inaccessible water (mio. m^3) [used for Surplus Discharge Allocation]
  reservedEFR <- calcOutput("EnvmtlFlowRequirementsConstraint", selectyears = selectyears,
                            lpjml = lpjml, climatetype = climatetype,
                            EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, aggregate = FALSE)

  # Full volume of EFRs (in mio. m^3) [used for Actual Use Accounting]
  fullEFR <- collapseNames(calcOutput("EnvmtlFlowRequirements", selectyears = selectyears,
                                      climatetype = climatetype, lpjml = lpjml, EFRmethod = EFRmethod,
                                      aggregate = FALSE)[, , "EFR"])

  # Water reserved from previous river routing (including full EFRs)
  if (com_ag) {

    reservedRiverrouting <- calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
                                       lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod,
                                       selectyears = selectyears, iniyear = iniyear, aggregate = FALSE)

  } else {

    reservedRiverrouting <- calcOutput("RiverHumanUses", humanuse = "non_agriculture",
                                       lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod,
                                       selectyears = selectyears, iniyear = iniyear, aggregate = FALSE)

  }

  # Correct reserved from previous river routing by EFRs
  reservedRiverrouting <- collapseNames(reservedRiverrouting[, , "required_wat_min"])
  reservedRiverrouting <- reservedRiverrouting - fullEFR

  out <- reservedEFR + reservedRiverrouting + inaccessibleDischarge

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "Reserved discharge: Water that cannot be withdrawn for irrigation",
              isocountries = FALSE))
}
