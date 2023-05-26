#' @title fullIRRIGATIONPOTENTIAL
#' @description Function that produces the objects for Technical and Economic
#'              Irrigation Potentials within land and water boundaries
#'
#' @param efrMethod         EFR method used including selected strictness of EFRs
#'                          (Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or
#'                          Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness
#'                          of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst")
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          USD_ha (USD per hectare) for relative area return, or
#'                          USD_m3 (USD per cubic meter) for relative volumetric return;
#'                          USD for absolute return (total profit);
#'                          Price aggregation:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices
#'                          and boolean indicating fullpotential (TRUE, i.e. cell
#'                          receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                          receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param gainthreshold     Threshold of yield improvement potential required for
#'                          water allocation in upstreamfirst algorithm
#'                          (in same unit as in rankmethod)
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
#'                                          and total physical areas per cell from readLanduseToolbox
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
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#'
#' @author Felicitas Beier
#'
#' @importFrom stringr str_split
#'
#' @export

fullIRRIGATIONPOTENTIAL <- function(efrMethod = "VMF:fair", accessibilityrule = "CV:2",
                                    transDist = 0,
                                    allocationrule = "optimization",
                                    rankmethod = "USD_m3:GLO:TRUE",
                                    gainthreshold = 500,
                                    protectLand = "HalfEarth",
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
  landScen         <- paste("potCropland", protectLand, sep = ":")
  irrigationsystem <- "initialization"

  ################
  # MAIN RESULTS #
  ################
  # Potentially irrigated area (PIA)
  calcOutput("IrrigAreaPotential", cropAggregation = TRUE,
              lpjml = lpjml, climatetype = climatetype,
              selectyears = lpjYears, iniyear = iniyear,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              gainthreshold = gt, irrigationsystem = irrigationsystem,
              landScen = landScen,
              cropmix = cropmix, comAg = TRUE,
              multicropping = multicropping, transDist = 100,
              aggregate = FALSE, file = "potIrrigArea.mz") # Note: switch to aggregate = "cluster" (but need to switch to different clustering first)

  # Potential irrigation water consumption (PIWC)

  # Potential irrigation water withdrawal (PIWW)

  ### each for:
  ## all magpie years
  ## all SSPs

}
