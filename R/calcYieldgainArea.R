#' @title       calcYieldgainArea
#' @description reports potentially irrigated area depending on gainthreshold
#'              and land constraint only
#'
#' @param region        Regional resolution (can be country iso-code, "GLO" for global, or
#'                      region name and respective mapping "EUR:H12")
#' @param GT_range      Range of gainthreshold for calculation of potentially
#'                      irrigated areas
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param selectyears   Years for which irrigatable area is calculated
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod     EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield) or
#'                      calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                      smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                      smoothed_calib (smoothed LPJmL yield potentials, not harmonized, calibrated for proxycrops)
#' @param thresholdtype TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param avlland_scen  Land availability scenario (currCropland, currIrrig, potIrrig)
#'                      combination of land availability scenario and initialization year separated by ":".
#'                      protection scenario separated by "_" (only relevant when potIrrig selected):
#'                      WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix       Cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcYieldgainArea(GT_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom stringr str_split
#'
#' @export

calcYieldgainArea <- function(region = "GLO", GT_range, lpjml, selectyears,
                                climatetype, EFRmethod, yieldcalib, thresholdtype,
                                avlland_scen, cropmix, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  x <- calcOutput("IrrigatableAreaUnlimited", gainthreshold = 0,
                  selectyears = selectyears, lpjml = lpjml, climatetype = climatetype,
                  cropmix = cropmix, yieldcalib = yieldcalib,
                  thresholdtype = thresholdtype, multicropping = multicropping,
                  avlland_scen = avlland_scen, aggregate = FALSE)
  d <- "Potentially Irrigated Area only considering land constraint"
  u <- "Mha"

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

  # sum up over regional dimension
  x <- toolRegionSums(x = x, region = region)
  x <- add_dimension(x, dim = 3.1, add = "GT", nm = "0")


  if (GT_range[1] == 0) {
    GT_range <- GT_range[-1]
  }

  for (gainthreshold in GT_range) {

    tmp <- calcOutput("IrrigatableAreaUnlimited", gainthreshold = gainthreshold,
                    selectyears = selectyears, lpjml = lpjml, climatetype = climatetype,
                    cropmix = cropmix, yieldcalib = yieldcalib,
                    thresholdtype = thresholdtype, multicropping = multicropping,
                    avlland_scen = avlland_scen, aggregate = FALSE)

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

    # sum up over regional dimension
    tmp <- toolRegionSums(x = tmp, region = region)
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
