#' @title fullPREPROCESSING
#' @description Function that produces the objects for Technical and Economic
#'              Irrigation Potentials within land and water boundaries
#'
#' @param protectLand       Land protection scenario (e.g. HalfEarth, BH_IFL, NULL)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          (mask can be:
#'                          "none": no mask applied (only for development purposes)
#'                          "actual:total": currently multicropped areas calculated from total harvested areas
#'                                          and total physical areas per cell from LandInG
#'                          "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                          "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                          "potential:endogenous": potentially multicropped areas given
#'                                                  temperature and productivity limits
#'                          "potential:exogenous": potentially multicropped areas given
#'                                                 GAEZ suitability classification)
#'                          (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param lpjml             LPJmL version required for respective inputs: natveg or crop
#' @param climatetype       Switch between different climate scenarios or
#'                          historical baseline "GSWP3-W5E5:historical"
#'
#' @author Felicitas Beier
#'
#' @importFrom stringr str_split
#'
#' @export

fullPREPROCESSING <- function(protectLand = "HalfEarth",
                              yieldcalib = "TRUE:TRUE:actual:irrig_crop",
                              multicropping = "TRUE:potential:endogenous",
                              cropmix = "hist_total",
                              climatetype = "MRI-ESM2-0:ssp370",
                              lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                                        crop = "ggcmi_phase3_nchecks_9ca735cb")) {
  # Preprocessing settings
  lpjYears         <- seq(1995, 2100, by = 5)
  iniyear          <- 1995

  # mrwater settings for MAgPIE
  gt               <- 100
  transDist        <- 100
  landScen         <- paste("potCropland", protectLand, sep = ":")
  irrigationsystem <- "initialization"

  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"
  allocationrule    <- "optimization"
  rankmethod        <- "USD_m3:GLO:TRUE"

  ################
  # MAIN RESULTS #
  ################
  # Multiple cropping factor (spatially explicit) from LandInG
  # does this have to be capped?

  # Crop-specific irrigation water requirements
  # (a) single cropping
  # (b) multiple cropping

  # Areas equipped for irrigation
  # -> committed irrigated areas (from calcIrrigAreaCommitted!)

  # Potentially irrigated areas (PIA)
  # -> including groundwater
  # for potCropland
  # How to include protection scenarios? Additional scenario dimension or separate files?
  # in MAgPIE: for past time steps: EFP off; scenarios start in 2020
  # How to ensure fade-in of EFP? diff between (EFP-off and EFP-on) * fader?

  # Water availability (PIWW, PIWC)
  # From renewable sources
  # From fossil groundwater


  # Potentially irrigated area from renewable water sources (PIA)
  calcOutput("IrrigAreaPotential", cropAggregation = TRUE,
              lpjml = lpjml, climatetype = climatetype,
              selectyears = lpjYears, iniyear = iniyear,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              gainthreshold = gt, irrigationsystem = irrigationsystem,
              landScen = landScen,
              cropmix = cropmix, comAg = TRUE,
              multicropping = multicropping,
              transDist = transDist, fossilGW = FALSE,
              aggregate = FALSE, file = "potIrrigArea.mz")
  calcOutput("IrrigAreaPotential", cropAggregation = TRUE,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = lpjYears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,
             gainthreshold = gt, irrigationsystem = irrigationsystem,
             landScen = landScen,
             cropmix = cropmix, comAg = TRUE,
             multicropping = multicropping,
             transDist = transDist, fossilGW = TRUE,
             aggregate = FALSE, file = "potIrrigArea_fossilGW.mz")
  ## To Do
  # (1) return PIA, PIWC, PIWW for renewable water sources & PIA, PIWC, PIWW from fossil groundwater (i.e. diff between with GW and without GW) --> separate function!
  # (1) switch to aggregate = "cluster" (but need to switch to different clustering first)

  # make function that combines all required scenarios:
  # GW on and off; EFP on and off; different SSPs

  # Potential irrigation water consumption (PIWC)

  # Potential irrigation water withdrawal (PIWW)

  ### each for:
  ## all magpie years
  ## all SSPs

}
