#' @title       calcRevenue
#' @description calculates revenue on selected area under selected management
#'
#' @param management        management in terms of irrigation and multiple cropping practices
#'                          "actual": irrigation and multiple cropping as reported by the
#'                                    Landuse Toolbox;
#'                          "single_rainfed"; "single_irrigated";
#'                          "multiple_rainfed"; "multiple_irrigated"
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
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
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("Revenue", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames

calcRevenue <- function(management, landScen,
                        lpjml, climatetype, selectyears, iniyear,
                        efrMethod, accessibilityrule,
                        rankmethod, yieldcalib, allocationrule, gainthreshold,
                        irrigationsystem, cropmix, potentialWat, comAg,
                        multicropping, transDist) {

  #########################
  ### Extract Arguments ###
  #########################
  priceAgg <- unlist(strsplit(rankmethod, split = ":"))[2]

  if (grepl(pattern = "single", x = management)) {
    m <- FALSE
  } else {
    m <- "TRUE:potential:endogenous"
  }

  ######################
  ### Read in Inputs ###
  ######################
  # read in cellular lpjml yields (in USD/ha)
  yields <- calcOutput("YieldsValued",
                       lpjml = lpjml, climatetype = climatetype,
                       iniyear = iniyear, selectyears = selectyears,
                       yieldcalib = yieldcalib,
                       priceAgg = priceAgg,
                       multicropping = m,
                       aggregate = FALSE)
  # read in area for which revenue shall be calculated (in Mha)
  irrigArea <- collapseNames(calcOutput("IrrigAreaPotential",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, iniyear = iniyear,
                                        efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                        allocationrule = allocationrule, gainthreshold = gainthreshold,
                                        irrigationsystem = irrigationsystem, landScen = landScen,
                                        cropmix = cropmix, comAg = comAg,
                                        multicropping = multicropping, transDist = transDist,
                                        aggregate = FALSE)[, , "irrigatable"])

  ####################
  ### Calculations ###
  ####################
  # Revenue (in mio. USD)
  out <- irrigArea * yields[, , strsplit(management, split = "_")[[1]][2]]

  # check for NAs and negative values
  if (any(is.na(out))) {
    stop("produced NA irrigatable area")
  }
  if (any(out < 0)) {
    stop("produced negative irrigatable area")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. USD",
              description  = "Revenue achieved on selected area",
              isocountries = FALSE))
}
