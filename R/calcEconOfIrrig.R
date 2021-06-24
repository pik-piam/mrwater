#' @title       calcEconOfIrrig
#' @description calculates potentially irrigated area for different gainthresholds
#'              subject to land and water constraints
#'
#' @param region           regional resolution (can be country iso-code,
#'                         region name and respective mapping "EUR:H12", "GLO" for global)
#' @param scenario         non-agricultural water use scenario
#' @param output           output to be displayed: irrigated area "IrrigArea" or
#'                         available water volume "wat_ag_ww" "wat_ag_wc"
#' @param GT_range         range of x-axis (gainthreshold) to be depicted on the curve
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
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
#' calcEconOfIrrig(region = "GLO", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom stringr str_split
#'
#' @export

calcEconOfIrrig <- function(region = "GLO", scenario, output, GT_range, lpjml, selectyears, climatetype, EFRmethod, accessibilityrule, rankmethod, yieldcalib, allocationrule, thresholdtype, irrigationsystem, avlland_scen, cropmix, potential_wat = TRUE, com_ag, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  iniyear <- as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])

  if (output == "IrrigArea") {
    x <- collapseNames(calcOutput("IrrigatableArea", gainthreshold = 0,
                                  selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                  avlland_scen = avlland_scen, cropmix = cropmix, potential_wat = potential_wat,
                                  com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"][, , scenario])

    d <- "Irrigatable Area for different gainthresholds"
    u <- "Mha"
  } else {
    x <- collapseNames(calcOutput("WaterPotUse", gainthreshold = 0,
                                  selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear,
                                  avlland_scen = avlland_scen, cropmix = cropmix, com_ag = com_ag,
                                  multicropping = multicropping, aggregate = FALSE)[, , output][, , scenario])
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
    x1 <- x2 <- x3 <- x
    x1[mc[, , "irrigated"] != 1] <- 0
    x1 <- add_dimension(x1, dim = 3.1, add = "MC", nm = "mc1")
    x2[mc[, , "irrigated"] != 2] <- 0
    x2 <- add_dimension(x2, dim = 3.1, add = "MC", nm = "mc2")
    x3[mc[, , "irrigated"] != 3] <- 0
    x3 <- add_dimension(x3, dim = 3.1, add = "MC", nm = "mc3")

    x <- mbind(x1, x2, x3)

  }

  # sum up over regional dimension and create data frame
  x <- toolRegionSums(x = x, region = region)
  x <- add_dimension(x, dim = 3.1, add = "GT", nm = "0")

  if (GT_range[1] == 0) {
    GT_range <- GT_range[-1]
  }

  for (gainthreshold in GT_range) {

    if (output == "IrrigArea") {
      tmp <- collapseNames(calcOutput("IrrigatableArea", gainthreshold = gainthreshold,
                                      lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                                      accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                      avlland_scen = avlland_scen, cropmix = cropmix, potential_wat = potential_wat,
                                      com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"][, , scenario])
    } else {
      tmp <- collapseNames(calcOutput("WaterPotUse", gainthreshold = gainthreshold,
                                      lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                                      accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear,
                                      avlland_scen = avlland_scen, cropmix = cropmix, com_ag = com_ag,
                                      multicropping = multicropping, aggregate = FALSE)[, , output][, , scenario])
      tmp <- tmp / 1000
    }

    # separation of multicropping layers and combine to one magpie object
    if (multicropping) {

      tmp1 <- tmp2 <- tmp3 <- tmp
      tmp1[mc[, , "irrigated"] != 1] <- 0
      tmp1 <- add_dimension(tmp1, dim = 3.1, add = "MC", nm = "mc1")
      tmp2[mc[, , "irrigated"] != 2] <- 0
      tmp2 <- add_dimension(tmp2, dim = 3.1, add = "MC", nm = "mc2")
      tmp3[mc[, , "irrigated"] != 3] <- 0
      tmp3 <- add_dimension(tmp3, dim = 3.1, add = "MC", nm = "mc3")

      tmp <- mbind(tmp1, tmp2, tmp3)

    }

    # sum up over regional dimension and create data frame
    tmp <- toolRegionSums(x = tmp, region = region)
    tmp <- add_dimension(tmp, dim = 3.1, add = "GT", nm = as.character(gainthreshold))

    x   <- mbind(x, tmp)
  }

  out          <- x
  if (multicropping) {
    getSets(out) <- c("region", "year", "GT", "MC", "EFP")

  } else {
    getSets(out) <- c("region", "year", "GT", "EFP")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = u,
              description  = d,
              isocountries = FALSE))
}
