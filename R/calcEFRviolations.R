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
#' @param rankmethod        Method of calculating the rank:
#'                          "meancellrank": mean over cellrank of proxy crops,
#'                          "meancroprank": rank over mean of proxy crops (normalized),
#'                          "meanpricedcroprank": rank over mean of proxy crops (normalized using price),
#'                          "watervalue": rank over value of irrigation water;
#'                          and fullpotentail TRUE/FALSE separated by ":"
#'                          (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area).
#'                          FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param thresholdtype     Thresholdtype of yield improvement:
#'                          TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (same unit as thresholdtype)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear           Initialization year of irrigation system
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param com_ag            If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#' @param scenario          Combination of EFP ("on", "off") and
#'                          non-ag. water use scenario ("ssp2", "ISIMIP", ...)
#'                          separated by "."
#' @param cellular          If TRUE: cellular data returned.
#'                          If FALSE: aggregated to basins
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getCells getItems
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Benjamin Leon Bodirsky
#'
#' @examples
#' \dontrun{
#' calcOutput("EFRviolations", aggregate = FALSE)
#' }
#'
calcEFRviolations <- function(lpjml, selectyears, climatetype, efrMethod,
                            accessibilityrule, rankmethod, yieldcalib, allocationrule,
                            thresholdtype, gainthreshold, irrigationsystem, iniyear,
                            landScen, cropmix, com_ag, multicropping,
                            scenario, cellular = TRUE) {

  # Check
  if (!is.numeric(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }

  # River structure attributes
  rs     <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))
  rs$iso <- readRDS(system.file("extdata/mapCoords2Country.rds", package = "mrcommons"))$iso


  discharge <- collapseNames(calcOutput("RiverSurplusDischargeAllocation",
                                        output = "discharge", selectyears = selectyears,
                                        lpjml = lpjml, climatetype = climatetype,
                                        efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                        allocationrule = allocationrule, thresholdtype = thresholdtype,
                                        gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                                        iniyear = iniyear, landScen = landScen,
                                        cropmix = cropmix, com_ag = com_ag,
                                        multicropping = multicropping, aggregate = FALSE)[, , scenario])

  envFlow   <- collapseNames(calcOutput("EnvmtlFlowRequirements", lpjml = lpjml, selectyears = selectyears,
                             climatetype = climatetype, efrMethod = efrMethod, aggregate = FALSE)[, , "EFR"])

  violation <- discharge - envFlow
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
