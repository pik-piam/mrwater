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
#' @param efrMethod         EFR method used including selected strictness of EFRs
#'                          (Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param comAg             If TRUE: currently already irrigated areas in initialization
#'                                   year are reserved for irrigation,
#'                          if FALSE: no irrigation areas are reserved (full irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          (mask can be:
#'                          "none": no mask applied (only for development purposes)
#'                          "actual:total": currently multicropped areas calculated from total harvested areas
#'                                          and total physical areas per cell from readLanduseToolbox
#'                          "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                          "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                          "potential:endogenous": potentially multicropped areas given
#'                                                  temperature and productivity limits
#'                          "potential:exogenous": potentially multicropped areas given
#'                                                 GAEZ suitability classification)
#'                          (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
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
                                 efrMethod, accessibilityrule,
                                 comAg, multicropping, transDist) {

  # Discharge that is inaccessible to human uses (mio m^3)
  inaccessibleDischarge <- calcOutput("DischargeInaccessible", selectyears = selectyears,
                                      lpjml = lpjml, climatetype = climatetype,
                                      accessibilityrule = accessibilityrule, aggregate = FALSE)

  # Environmental flow requirements beyond already inaccessible water (mio. m^3) [used for Surplus Discharge Allocation]
  reservedEFR <- fullEFR <- new.magpie(cells_and_regions = getCells(inaccessibleDischarge),
                                       years = getYears(inaccessibleDischarge),
                                       names = c("on", "off"),
                                       fill = 0,
                                       sets = c("x.y.iso", "year", "EFR"))
  reservedEFR[, , "on"]  <- calcOutput("EnvmtlFlowRequirementsConstraint", selectyears = selectyears,
                                       lpjml = lpjml, climatetype = climatetype,
                                       efrMethod = efrMethod, accessibilityrule = accessibilityrule, aggregate = FALSE)

  # Full volume of EFRs (in mio. m^3) [used for Actual Use Accounting]
  fullEFR[, , "on"]      <- collapseNames(calcOutput("EnvmtlFlowRequirements", selectyears = selectyears,
                                                     climatetype = climatetype, lpjml = lpjml, efrMethod = efrMethod,
                                                     aggregate = FALSE)[, , "EFR"])

  # Water reserved from previous river routing (including full EFRs)
  if (comAg) {

    reservedRiverrouting <- calcOutput("RiverHumanUseAccounting",
                                       iteration = "committed_agriculture", transDist = transDist,
                                       lpjml = lpjml, climatetype = climatetype,
                                       efrMethod = efrMethod, multicropping = multicropping,
                                       selectyears = selectyears, iniyear = iniyear,
                                       accessibilityrule = accessibilityrule,
                                       rankmethod = NULL, gainthreshold = NULL,
                                       cropmix = NULL, yieldcalib = NULL,
                                       irrigationsystem = NULL, landScen = NULL,
                                       comAg = comAg, aggregate = FALSE)
  } else {

    reservedRiverrouting <- calcOutput("RiverHumanUseAccounting",
                                       iteration = "non_agriculture", transDist = transDist,
                                       lpjml = lpjml, climatetype = climatetype,
                                       efrMethod = efrMethod, multicropping = multicropping,
                                       selectyears = selectyears, iniyear = iniyear,
                                       accessibilityrule = accessibilityrule,
                                       rankmethod = NULL, gainthreshold = NULL,
                                       cropmix = NULL, yieldcalib = NULL,
                                       irrigationsystem = NULL, landScen = NULL,
                                       comAg = comAg, aggregate = FALSE)
  }

  # Correct reserved from previous river routing by EFRs
  reservedRiverrouting <- collapseNames(reservedRiverrouting[, , "reservedWW"])
  reservedRiverrouting <- reservedRiverrouting - fullEFR

  out <-  reservedRiverrouting + reservedEFR + inaccessibleDischarge

  # Test
  if (any(out < 0)) {
    stop("calcRiverWatReserved produced negative water volumes.")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "Reserved discharge:
                              Water that cannot be withdrawn for irrigation",
              isocountries = FALSE))
}
