#' @title       calcEconOfIrrig
#' @description calculates potentially irrigated area for different gainthresholds
#'              subject to land and water constraints
#'
#' @param scenario         non-agricultural water use scenario
#' @param output           output to be displayed: irrigated area "IrrigArea" or
#'                         available water volume "wat_ag_ww" "wat_ag_wc"
#' @param GT_range         range of x-axis (gainthreshold) to be depicted on the curve
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param iniyear          initialization year
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param landScen         Land availability scenario consisting of two parts separated by ":":
#'                         1. available land scenario (currCropland, currIrrig, potCropland)
#'                         2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                         For case of no land protection select "NA"
#'                         or do not specify second part of the argument
#' @param cropmix          Selected cropmix (options:
#'                         "hist_irrig" for historical cropmix on currently irrigated area,
#'                         "hist_total" for historical cropmix on total cropland,
#'                         or selection of proxycrops)
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation,
#'                         if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
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
#' @importFrom magclass dimSums collapseNames
#' @importFrom stringr str_split
#'
#' @export

calcEconOfIrrig <- function(scenario, output, GT_range, selectyears, iniyear,
                            lpjml, climatetype, efrMethod, accessibilityrule,
                            rankmethod, yieldcalib, allocationrule, thresholdtype,
                            irrigationsystem, landScen, cropmix,
                            potential_wat = TRUE, com_ag, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  if (output == "IrrigArea") {

    x <- collapseNames(calcOutput("IrrigatableArea", gainthreshold = 0,
                                  selectyears = selectyears, iniyear = iniyear,
                                  climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                  landScen = landScen, cropmix = cropmix, potential_wat = potential_wat,
                                  com_ag = com_ag, multicropping = multicropping,
                                  aggregate = FALSE)[, , "irrigatable"][, , scenario])

    d <- "Irrigatable Area for different gainthresholds"
    u <- "Mha"

  } else {

    x <- collapseNames(calcOutput("WaterPotUse", gainthreshold = 0,
                                  selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear,
                                  landScen = landScen, cropmix = cropmix, com_ag = com_ag,
                                  multicropping = multicropping, aggregate = FALSE)[, , output][, , scenario])
    # transform from mio. m^3 to km^3:
    # (1 km^3 = 1e+09 m^3)
    # (1 mio. = 1e+06)
    x <- x / 1000
    d <- "Water Use Potential for different gainthresholds"
    u <- "km^3"

  }

  x <- add_dimension(x, dim = 3.1, add = "GT", nm = "0")

  if (GT_range[1] == 0) {
    GT_range <- GT_range[-1]
  }

  for (gainthreshold in GT_range) {

    if (output == "IrrigArea") {

      tmp <- collapseNames(calcOutput("IrrigatableArea", gainthreshold = gainthreshold,
                                      selectyears = selectyears, iniyear = iniyear,
                                      lpjml = lpjml, climatetype = climatetype,
                                      accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                      landScen = landScen, cropmix = cropmix, potential_wat = potential_wat,
                                      com_ag = com_ag, multicropping = multicropping,
                                      aggregate = FALSE)[, , "irrigatable"][, , scenario])
    } else {

      tmp <- collapseNames(calcOutput("WaterPotUse", gainthreshold = gainthreshold,
                                      climatetype = climatetype, lpjml = lpjml,
                                      selectyears = selectyears, iniyear = iniyear,
                                      accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                      landScen = landScen, cropmix = cropmix, com_ag = com_ag,
                                      multicropping = multicropping, aggregate = FALSE)[, , output][, , scenario])
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
