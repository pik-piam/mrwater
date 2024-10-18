#' @title       calcEFRviolations
#' @description This function calculates grid cell specific violation of
#'              environmental flow requirements
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years for which irrigatable area is calculated
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          (e.g. Q75: 0.25, Q50: 0.5)
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
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (in USD per hectare)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear           Initialization year of irrigation system
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, or one of the scenarios available in calcConservationPriorities,
#'                             e.g., 30by20, BH, BH_IFL, PBL_HalfEarth,
#'                             or NA for no protection).
#'                          For case of no land protection select "NA" in second part of argument
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param comAg             If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          (mask can be:
#'                          "none": no mask applied (only for development purposes)
#'                          "actual:total": currently multicropped areas calculated from total harvested areas
#'                                          and total physical areas per cell from readLandInG
#'                          "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                          "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                          "potential:endogenous": potentially multicropped areas given
#'                                                  temperature and productivity limits
#'                          "potential:exogenous": potentially multicropped areas given
#'                                                 GAEZ suitability classification)
#'                          (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand
#' @param scenario          Combination of EFP ("on", "off") and
#'                          non-ag. water use scenario ("ssp2", "ISIMIP", ...)
#'                          separated by "."
#' @param cellular          If TRUE: cellular data returned.
#'                          If FALSE: aggregated to basins
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getCells getItems
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Benjamin Leon Bodirsky
#'
#' @examples
#' \dontrun{
#' calcOutput("EFRviolations", aggregate = FALSE)
#' }
#'
calcEFRviolations <- function(lpjml, selectyears, climatetype, efrMethod, transDist,
                              accessibilityrule, rankmethod, yieldcalib, allocationrule,
                              gainthreshold, irrigationsystem, iniyear,
                              landScen, cropmix, comAg, multicropping,
                              scenario, cellular = TRUE) {
  # Check
  if (!is.numeric(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }

  # River structure attributes
  rs     <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))
  rs$iso <- toolGetMappingCoord2Country()$iso

  discharge <- collapseNames(calcOutput("RiverDischargeAllocation",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, transDist = transDist,
                                        efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                        allocationrule = allocationrule,
                                        gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                                        iniyear = iniyear, landScen = landScen,
                                        cropmix = cropmix, comAg = comAg,
                                        multicropping = multicropping,
                                        aggregate = FALSE)[, , "discharge"][, , scenario])

  envFlow   <- collapseNames(calcOutput("EnvmtlFlowRequirements", selectyears = selectyears,
                                        lpjml = lpjml, climatetype = climatetype,
                                        efrMethod = efrMethod, aggregate = FALSE)[, , "EFR"])

  violation <- discharge - envFlow          # To Do: I think this is not correct
  violation[violation > 0] <- 0

  # Check correct cell order:
  if (any(getCells(violation) != paste(rs$coordinates, rs$iso, sep = "."))) {
    stop("Wrong cell order!")
  }
  getItems(violation, dim = "basin", maindim = 1) <- as.character(rs$endcell)

  if (!cellular) {
    violation <- dimSums(violation, dim = c(1.1, 1.2, 1.3))
  }

  return(list(x            = violation,
              weight       = NULL,
              unit         = "km^3 per year",
              description  = "violation of environmental flow requirements per grid cell",
              isocountries = FALSE))
}
