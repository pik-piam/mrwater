#' @title       calcWaterAvlMAgPIE
#' @description This function returns the potential water quantity
#'              by scenario and source available for withdrawal
#'              for agriculture in MAgPIE.
#'              Optionally, total water availability and consumptive
#'              water availability can be returned.
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
#'                          across cells of river basin ("optimization", "upstreamfirst")
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (in USD per hectare)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear           Initialization year of irrigation system
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, or one of the scenarios available
#'                             in calcConservationPriorities,
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
#' @param fossilGW          If TRUE: non-renewable groundwater can be used.
#'                          If FALSE: non-renewable groundwater cannot be used.
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                          "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                          separated by ":"
#'                          (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#' @param usagetype         Water usage type to be returned.
#'                          Options: "withdrawal", "consumption"
#' @param countryAggregation TRUE (grid cell data is aggregated to country-level),
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells mbind add_dimension new.magpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterAvlMAgPIE", aggregate = FALSE)
#' }

calcWaterAvlMAgPIE <- function(lpjml, selectyears, climatetype, efrMethod,
                               accessibilityrule, rankmethod, yieldcalib, allocationrule,
                               gainthreshold, irrigationsystem, iniyear,
                               landScen, cropmix, comAg, fossilGW,
                               multicropping, transDist, usagetype, countryAggregation = FALSE) {

  # potential water usage (PIWW and PIWC)
  potWaterUsage <- calcOutput("PotWater", lpjml = lpjml, climatetype = climatetype,
                              selectyears = selectyears, efrMethod = efrMethod,
                              accessibilityrule = accessibilityrule, rankmethod = rankmethod,
                              yieldcalib = yieldcalib, allocationrule = allocationrule,
                              gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                              iniyear = iniyear, landScen = landScen, cropmix = cropmix,
                              comAg = comAg, fossilGW = fossilGW, multicropping = multicropping,
                              transDist = transDist, aggregate = FALSE)

  if (usagetype == "withdrawal") {
    potWaterUsage  <- potWaterUsage[, , c("wat_ag_ww", "wat_gw_ww", "wat_tot_ww")]
  } else if (usagetype == "consumption") {
    potWaterUsage  <- potWaterUsage[, , c("wat_ag_wc", "wat_gw_wc", "wat_tot_wc")]
  }
  getItems(potWaterUsage, dim = "wtype") <- c("ren_ag", "nonren_ag", "tot")
  names(dimnames(potWaterUsage))[[3]] <- "EFP.scen.source"

  # renewable water resources available for agriculture
  watAvlAg <- potWaterUsage[, , "ren_ag"] - collapseNames(potWaterUsage[, , "nonren_ag"])
  # non-renewable (fossil) groundwater resources (based on current excessive withdrawals)
  watGW <- potWaterUsage[, , "nonren_ag"]
  # water (renewable and groundwater) reserved for non-agricultural usage
  watReservedNonAg <- potWaterUsage[, , "tot"] - collapseNames(potWaterUsage[, , "ren_ag"])
  getItems(watReservedNonAg, dim = "source") <- "res_nonAg"

  out <- mbind(watAvlAg, watGW, watReservedNonAg)

  if (any(out < 0)) {
    stop("calcWaterAvlMAgPIE returns negative values for water availability.
         Please check what's wrong and correct.")
  }

  description  <- "potential water availability for different uses by source"
  # Aggregate to country level
  if (countryAggregation) {
    out <- dimSums(out, dim = c("x", "y"))
    description <- paste0(description, " at country level resolution")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
