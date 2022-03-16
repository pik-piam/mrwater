#' @title fullMULTICROPPING
#' @description Function that produces output for irrigation potentials
#'              under multipl cropping on cellular resolution.
#'
#' @param suitability   "endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set
#'
#' @author Felicitas Beier
#'
#' @export

fullMULTICROPPING <- function(suitability = "endogenous") {

  # Standard settings
  multicropping     <- TRUE
  iniyear           <- 2010
  selectyears       <- 2010
  plotyear          <- 2010
  ssp               <- "ssp2"

  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_9ca735cb")
  climatetype       <- "GSWP3-W5E5:historical"

  irrigationsystem  <- "initialization"
  gtrange           <- c(0, 250, 500, 1000, 2000, 3000)
  efrMethod         <-  "VMF:fair"
  accessibilityrule <- "CV:2"
  allocationrule    <- "optimization"
  rankmethod        <- "USD_ha:TRUE"
  thresholdtype     <- "USD_ha"
  gainthreshold     <- 500
  protectLand       <- "HalfEarth"
  yieldcalib        <- TRUE
  cropmix           <- "hist_total"


  ################
  # MAIN RESULTS #
  ################

  # Yield gain through irrigation under multiple cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = TRUE, aggregate = FALSE,
             file = paste0("yieldgain_USDha_multiple", ".mz"))

  # Yield gain through irrigation under single cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = FALSE, aggregate = FALSE,
             file = paste0("yieldgain_USDha_single", ".mz"))

  ##############
  # VALIDATION #
  ##############

  # Multiple cropping suitability per crop calculated based on crop and grass productivity
  calcOutput("MulticroppingYieldIncrease", output = "multicroppingSuitability",
              lpjml = lpjml[["crop"]], climatetype = climatetype,
              selectyears = selectyears, suitability = "endogenous",
              aggregate = FALSE, file = "suitMC_LPJmL.mz")

  # Multiple cropping suitability according to GAEZ
  calcOutput("MultipleCroppingZones", layers = 8,
             aggregate = FALSE, file = "suitMC_GAEZ.mz")


}
