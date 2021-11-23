#' @title       calcYieldgainArea
#' @description reports potentially irrigated area depending on gainthreshold
#'              and land constraint only
#'
#' @param rangeGT       Range of gainthreshold for calculation of potentially
#'                      irrigated areas
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param selectyears   Years for which irrigatable area is calculated
#' @param iniyear       Initialization year for cropland area
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod     EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                      If FALSE: uncalibrated LPJmL yields are used
#' @param thresholdtype TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param landScen      Land availability scenario consisting of two parts separated by ":":
#'                      1. landScen (currCropland, currIrrig, potCropland)
#'                      2. for curr-scenarios: initialization year;
#'                      for pot-scenarios: protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_FF, NA).
#'                      For case of pot-scenario without land protection select "NA"
#'                      or do not specify second part of the argument
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
#' calcYieldgainArea(rangeGT = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames
#' @importFrom stringr str_split
#'
#' @export

calcYieldgainArea <- function(rangeGT, lpjml, selectyears, iniyear,
                              climatetype, efrMethod, yieldcalib, thresholdtype,
                              landScen, cropmix, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  x <- calcOutput("IrrigatableAreaUnlimited", gainthreshold = 0,
                  selectyears = selectyears, iniyear = iniyear,
                  lpjml = lpjml, climatetype = climatetype,
                  cropmix = cropmix, yieldcalib = yieldcalib,
                  thresholdtype = thresholdtype, multicropping = multicropping,
                  landScen = landScen, aggregate = FALSE)
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

    # calculate harvested area
    x2 <- 2 * x2
    x3 <- 3 * x3

    x <- mbind(x1, x2, x3)

  }

  x <- add_dimension(x, dim = 3.1, add = "GT", nm = "0")

  if (rangeGT[1] == 0) {
    rangeGT <- rangeGT[-1]
  }

  for (gainthreshold in rangeGT) {

    tmp <- calcOutput("IrrigatableAreaUnlimited", gainthreshold = gainthreshold,
                    selectyears = selectyears, iniyear = iniyear,
                    lpjml = lpjml, climatetype = climatetype,
                    cropmix = cropmix, yieldcalib = yieldcalib,
                    thresholdtype = thresholdtype, multicropping = multicropping,
                    landScen = landScen, aggregate = FALSE)

    if (multicropping) {

      tmp1 <- tmp2 <- tmp3 <- tmp
      tmp1[mc != 0] <- 0
      tmp1 <- add_dimension(tmp1, dim = 3.1, add = "MC", nm = "addMC0")
      tmp2[mc != 1] <- 0
      tmp2 <- add_dimension(tmp2, dim = 3.1, add = "MC", nm = "addMC1")
      tmp3[mc != 2] <- 0
      tmp3 <- add_dimension(tmp3, dim = 3.1, add = "MC", nm = "addMC2")

      # calculate harvested area
      tmp2 <- 2 * tmp2
      tmp3 <- 3 * tmp3

      tmp <- mbind(tmp1, tmp2, tmp3)

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
