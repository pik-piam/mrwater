#' @title       calcIrrigationReturn
#' @description calculates return achieved on potentially irrigated area
#'
#' @param country           Character string of country or vector of countries
#'                          for which irrigation profit shall be returned.
#'                          Options: GLO, or any iso3 country code
#' @param lpjml             LPJmL version used
#' @param selectyears       Years for which irrigatable area is calculated
#' @param iniyear           Initialization year for initial croparea
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
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param potentialWat      If TRUE: potential available water and areas used,
#'                          if FALSE: currently reserved water on current irrigated cropland used
#' @param comAg             If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          ("endogenous": suitability for multiple cropping determined
#'                                         by rules based on grass and crop productivity
#'                          "exogenous": suitability for multiple cropping given by
#'                                       GAEZ data set),
#'                          separated by ":"
#'                          (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#' @param profitType        Unit according of irrigation profit:
#'                          USD_ha (USD per hectare) for relative area return, or
#'                          USD_m3 (USD per cubic meter) for relative volumetric return;
#'                          USD for absolute return (total profit);
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigationReturn", aggregate = FALSE)
#' }
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames

calcIrrigationReturn <- function(country, lpjml, selectyears, iniyear, climatetype, efrMethod,
                                accessibilityrule, rankmethod, yieldcalib, allocationrule,
                                gainthreshold, irrigationsystem, landScen,
                                cropmix, potentialWat, comAg, multicropping, transDist,
                                profitType) {

  ######################
  ### Read in Inputs ###
  ######################
  # Area Irrigated given chosen scenario
  irrigArea <- collapseNames(calcOutput("IrrigAreaPotential",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, iniyear = iniyear,
                                        efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                        allocationrule = allocationrule, gainthreshold = gainthreshold,
                                        irrigationsystem = irrigationsystem, landScen = landScen,
                                        cropmix = cropmix, potentialWat = potentialWat, comAg = comAg,
                                        multicropping = multicropping, transDist = transDist,
                                        aggregate = FALSE)[, , "irrigatable"])

  # Yield gain per crop (in USD/ha)
  yieldGain <- calcOutput("IrrigCropYieldGain",
                          priceAgg = str_split(rankmethod, pattern = ":")[[1]][2],
                          lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib,
                          multicropping = multicropping, aggregate = FALSE)
  # set negative yield gains to 0
  yieldGain[yieldGain < 0] <- 0


  # Irrigation water requirements per crop (in m^3 per ha per yr)
  irrigWat <- calcOutput("ActualIrrigWatRequirements",
                         selectyears = selectyears, iniyear = iniyear,
                         lpjml = lpjml, climatetype = climatetype,
                         irrigationsystem = irrigationsystem, multicropping = multicropping,
                         aggregate = FALSE)

  # Share of crop area by crop type
  cropareaShr <- calcOutput("CropAreaShare",
                            iniyear = iniyear, cropmix = cropmix,
                            aggregate = FALSE)

  # Level of aggregation to be returned
  if (country == "GLO") {
    country <- getCells(irrigArea)
  }

  ####################
  ### Calculations ###
  ####################
  # Irrigated croparea per crop (in Mha)
  irrigCroparea <- irrigArea * cropareaShr

  # Total profit (in mio. USD)
  totalProfit <- dimSums(irrigCroparea[country, , getItems(yieldGain,
                                                           dim = 3)] * yieldGain[country, , ],
                         dim = c("x", "y", "iso", "crop"))
  # Total irrigated area (in Mha)
  totalIrrigArea <- dimSums(irrigCroparea,
                            dim = c("x", "y", "iso", "crop"))
  # Total water use (in mio. m^3)
  totalWater <- dimSums(irrigCroparea[country, , getItems(yieldGain, dim = 3)] *
                          irrigWat[country, , getItems(yieldGain, dim = 3)],
                        dim = c("x", "y", "iso", "crop"))

  if (profitType == "USD") {

    # total profits achieved by irrigated area (mio. USD)
    out <- totalProfit
    u   <- "mio. USD"

  } else if (profitType == "USD_ha") {

    # average profit achieved on irrigated area (mio. USD / mio. ha --> USD/ha)
    out <- totalProfit / totalIrrigArea
    u   <- "USD per hectare"

  } else if (profitType == "USD_m3") {

    # average profit achieved per volume of irrigation water (mio. USD / mio. m^3 --> USD/m3)
    out <- totalProfit / totalWater
    u   <- "USD per m^3"

  } else {
    stop("Please select unit of Irrigation Return to be reported via the profitType argument")
  }

  # check for NAs and negative values
  if (any(is.na(out))) {
    stop("produced NA irrigatable area")
  }
  if (any(out < 0)) {
    stop("produced negative irrigatable area")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = u,
              description  = "Profit through irrigation achieved at the
                              respective (potentially) irrigated area",
              isocountries = FALSE))
}
