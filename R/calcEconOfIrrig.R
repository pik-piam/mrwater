#' @title       calcEconOfIrrig
#' @description calculates potentially irrigated area for different gainthresholds
#'              subject to land and water constraints
#'
#' @param scenario          non-agricultural water use scenario
#' @param output            output to be displayed: irrigated area "IrrigArea" or
#'                          available water volume "wat_ag_ww" "wat_ag_wc"
#' @param gtrange           range of x-axis (gainthreshold) to be depicted on the curve
#' @param lpjml             LPJmL version required for respective inputs: natveg or crop
#' @param selectyears       years for which irrigatable area is calculated
#' @param iniyear           initialization year
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
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
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
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
#' @param potentialWat      if TRUE: potential available water and areas used,
#'                          if FALSE: currently reserved water on current irrigated cropland used
#' @param comAg             if TRUE: the currently already irrigated areas
#'                                   in initialization year are reserved for irrigation,
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
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcEconOfIrrig(aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames mbind add_dimension
#' @importFrom stringr str_split
#'
#' @export

calcEconOfIrrig <- function(scenario, output, gtrange, selectyears, iniyear,
                            lpjml, climatetype, efrMethod, accessibilityrule,
                            rankmethod, yieldcalib, allocationrule,
                            irrigationsystem, landScen, cropmix, transDist,
                            potentialWat = TRUE, comAg, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  if (output == "IrrigArea") {

    x <- collapseNames(calcOutput("IrrigAreaPotential", gainthreshold = 0,
                                  selectyears = selectyears, iniyear = iniyear,
                                  climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib,
                                  allocationrule = allocationrule,
                                  irrigationsystem = irrigationsystem,
                                  landScen = landScen, cropmix = cropmix, potentialWat = potentialWat,
                                  comAg = comAg, multicropping = multicropping,
                                  transDist = transDist,
                                  aggregate = FALSE)[, , "irrigatable"][, , scenario])

    d <- "Irrigatable Area for different gainthresholds"
    u <- "Mha"

  } else {

    x <- collapseNames(calcOutput("WaterUsePotential", gainthreshold = 0,
                                  selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib,
                                  allocationrule = allocationrule,
                                  irrigationsystem = irrigationsystem, iniyear = iniyear,
                                  landScen = landScen, cropmix = cropmix, comAg = comAg,
                                  multicropping = multicropping, transDist = transDist,
                                  aggregate = FALSE)[, , output][, , scenario])
    # transform from mio. m^3 to km^3:
    # (1 km^3 = 1e+09 m^3)
    # (1 mio. = 1e+06)
    x <- x / 1000
    d <- "Water Use Potential for different gainthresholds"
    u <- "km^3"

  }

  x <- add_dimension(x, dim = 3.1, add = "GT", nm = "0")

  if (gtrange[1] == 0) {
    gtrange <- gtrange[-1]
  }

  for (gainthreshold in gtrange) {

    if (output == "IrrigArea") {

      tmp <- collapseNames(calcOutput("IrrigAreaPotential", gainthreshold = gainthreshold,
                                      selectyears = selectyears, iniyear = iniyear,
                                      lpjml = lpjml, climatetype = climatetype,
                                      accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      irrigationsystem = irrigationsystem,
                                      landScen = landScen, cropmix = cropmix, potentialWat = potentialWat,
                                      comAg = comAg, multicropping = multicropping, transDist = transDist,
                                      aggregate = FALSE)[, , "irrigatable"][, , scenario])
    } else {

      tmp <- collapseNames(calcOutput("WaterUsePotential", gainthreshold = gainthreshold,
                                      climatetype = climatetype, lpjml = lpjml,
                                      selectyears = selectyears, iniyear = iniyear,
                                      accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      irrigationsystem = irrigationsystem,
                                      landScen = landScen, cropmix = cropmix, comAg = comAg,
                                      multicropping = multicropping, transDist = transDist,
                                      aggregate = FALSE)[, , output][, , scenario])
      tmp <- tmp / 1000
    }

    tmp <- add_dimension(tmp, dim = 3.1, add = "GT", nm = as.character(gainthreshold))

    x   <- mbind(x, tmp)
  }

  out <- collapseNames(x)

  return(list(x            = out,
              weight       = NULL,
              unit         = u,
              description  = d,
              isocountries = FALSE))
}
