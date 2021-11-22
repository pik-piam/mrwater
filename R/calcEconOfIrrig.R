#' @title       calcEconOfIrrig
#' @description calculates potentially irrigated area for different gainthresholds
#'              subject to land and water constraints
#'
#' @param scenario         non-agricultural water use scenario
#' @param season           single, double, triple
#' @param output           output to be displayed: irrigated area "IrrigArea" or
#'                         available water volume "wat_ag_ww" "wat_ag_wc"
#' @param GT_range         range of x-axis (gainthreshold) to be depicted on the curve
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
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
#'                         1. landScen (currCropland, currIrrig, potCropland)
#'                         2. for curr-scenarios: initialization year;
#'                         for pot-scenarios: protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_FF, NA).
#'                         For case of pot-scenario without land protection select "NA"
#'                         or do not specify second part of the argument
#' @param cropmix          cropmix for which irrigation yield improvement is calculated
#'                         can be selection of proxycrop(s) for calculation of average yield gain
#'                         or hist_irrig or hist_total for historical cropmix
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

calcEconOfIrrig <- function(scenario, season, output, GT_range, lpjml, selectyears, climatetype, efrMethod, accessibilityrule, rankmethod, yieldcalib, allocationrule, thresholdtype, irrigationsystem, landScen, cropmix, potential_wat = TRUE, com_ag, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  iniyear <- as.numeric(as.list(strsplit(landScen, split = ":"))[[1]][2])

  if (output == "IrrigArea") {
    x <- collapseNames(calcOutput("IrrigatableArea", gainthreshold = 0,
                                  selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                  landScen = landScen, cropmix = cropmix, potential_wat = potential_wat,
                                  com_ag = com_ag, multicropping = multicropping,
                                  aggregate = FALSE)[, , "irrigatable"][, , scenario][, , season])

    d <- "Irrigatable Area for different gainthresholds"
    u <- "Mha"
  } else {
    x <- collapseNames(calcOutput("WaterPotUse", gainthreshold = 0,
                                  selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear,
                                  landScen = landScen, cropmix = cropmix, com_ag = com_ag,
                                  multicropping = multicropping, aggregate = FALSE)[, , output][, , scenario][, , season])
    # transform from mio. m^3 to km^3:
    # (1 km^3 = 1e+09 m^3)
    # (1 mio. = 1e+06)
    x <- x / 1000
    d <- "Water Use Potential for different gainthresholds"
    u <- "km^3"
  }

  # separation of multicropping layers and combine to one magpie object
  if (multicropping) {

    mc <- calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)
    mc <- mc[, , "irrigated"] - mc[, , "rainfed"]
    x1 <- x2 <- x3 <- x
    x1[mc != 0] <- 0
    x1 <- add_dimension(x1, dim = 3.1, add = "MC", nm = "addMC0")
    x2[mc != 1] <- 0
    x2 <- add_dimension(x2, dim = 3.1, add = "MC", nm = "addMC1")
    x3[mc != 2] <- 0
    x3 <- add_dimension(x3, dim = 3.1, add = "MC", nm = "addMC2")

    x <- mbind(x1, x2, x3)

  }

  x <- add_dimension(x, dim = 3.1, add = "GT", nm = "0")

  if (GT_range[1] == 0) {
    GT_range <- GT_range[-1]
  }

  for (gainthreshold in GT_range) {

    if (output == "IrrigArea") {
      tmp <- collapseNames(calcOutput("IrrigatableArea", gainthreshold = gainthreshold,
                                      lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                                      accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                      landScen = landScen, cropmix = cropmix, potential_wat = potential_wat,
                                      com_ag = com_ag, multicropping = multicropping,
                                      aggregate = FALSE)[, , "irrigatable"][, , scenario][, , season])
    } else {
      tmp <- collapseNames(calcOutput("WaterPotUse", gainthreshold = gainthreshold,
                                      lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                                      accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear,
                                      landScen = landScen, cropmix = cropmix, com_ag = com_ag,
                                      multicropping = multicropping, aggregate = FALSE)[, , output][, , scenario][, , season])
      tmp <- tmp / 1000
    }

    # separation of multicropping layers and combine to one magpie object
    if (multicropping) {

      tmp1 <- tmp2 <- tmp3 <- tmp
      tmp1[mc != 0] <- 0
      tmp1 <- add_dimension(tmp1, dim = 3.1, add = "MC", nm = "addMC0")
      tmp2[mc != 1] <- 0
      tmp2 <- add_dimension(tmp2, dim = 3.1, add = "MC", nm = "addMC1")
      tmp3[mc != 2] <- 0
      tmp3 <- add_dimension(tmp3, dim = 3.1, add = "MC", nm = "addMC2")

      tmp <- mbind(tmp1, tmp2, tmp3)

    }

    tmp <- add_dimension(tmp, dim = 3.1, add = "GT", nm = as.character(gainthreshold))

    x   <- mbind(x, tmp)
  }

  out <- collapseNames(x)

  # if (multicropping) {
   # getSets(out) <- c("region", "year", "GT", "MC", "EFP")

  # } else {
 #   getSets(out) <- c("x", "y", "iso", "year", "GT", "EFP")
  # }

  return(list(x            = out,
              weight       = NULL,
              unit         = u,
              description  = d,
              isocountries = FALSE))
}
