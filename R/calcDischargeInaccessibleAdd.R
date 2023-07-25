#' @title       calcDischargeInaccessibleAdd
#' @description This function calculates water that is inaccessible to humans
#'              but not part of EFRs.
#'              Reason: Inacessible discharge is highly variable discharge that can also
#'              serve as high flow requirements (HFR) for EFRs.
#'              This has to be accounted in the Discharge Allocation Algorithm
#'              for the determination of potential irrigation.
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years to be returned
#'                          (Note: does not affect years of harmonization or smoothing)
#' @param climatetype       Switch between different climate scenarios
#'                          or historical baseline "GSWP3-W5E5:historical"
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param efrMethod         EFR method used including selected strictness of EFRs
#'                          (Smakhtin:good, VMF:fair)
#'
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("DischargeInaccessibleAdd", aggregate = FALSE)
#' }
#'

calcDischargeInaccessibleAdd <- function(selectyears = selectyears,
                                         lpjml = lpjml, climatetype = climatetype,
                                         accessibilityrule = accessibilityrule,
                                         efrMethod = efrMethod) {

  # Discharge that is inaccessible to humans due to variability
  inaccessibleDischarge <- calcOutput("DischargeInaccessible", selectyears = selectyears,
                                      lpjml = lpjml, climatetype = climatetype,
                                      accessibilityrule = accessibilityrule,
                                      aggregate = FALSE)

  # Environmental flow requirements that can be fulfilled with inaccessible discharge
  inaccessibleEFR       <- calcOutput("EnvmtlFlowRequirementsInaccess", selectyears = selectyears,
                                      lpjml = lpjml, climatetype = climatetype,
                                      accessibilityrule = accessibilityrule,
                                      efrMethod = efrMethod,
                                      aggregate = FALSE)

  # Prepare output to be returned
  out <- new.magpie(cells_and_regions = getItems(inaccessibleDischarge, dim = 1),
                    years = getItems(inaccessibleDischarge, dim = 2),
                    names = c("on", "off"),
                    fill = 0,
                    sets = c("x", "y", "iso", "year", "EFR"))

  out[, , "on"]  <- inaccessibleDischarge - inaccessibleEFR
  out[, , "off"] <- inaccessibleDischarge

  # Check for NAs and negative values
  if (any(round(out, digits = 6) < 0)) {
    stop("mrwater::calcDischargeInaccessibleAdd produced negative water volumes.")
  }
  if (any(is.na(out))) {
    stop("mrwater::calcDischargeInaccessibleAdd produced NA values.")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("Water that is inaccessible ",
                              "for additional irrigation"),
              isocountries = FALSE))
}
