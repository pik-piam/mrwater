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
#'                      1. available land scenario (currCropland, currIrrig, potCropland)
#'                      2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_FF, NA).
#'                      For case of no land protection select "NA"
#'                      or do not specify second part of the argument
#' @param cropmix       Selected cropmix (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
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

  x <- vector(mode = "list", length = length(rangeGT))
  i <- 0

  for (gainthreshold in rangeGT) {

    i <- i + 1

    tmp <- calcOutput("IrrigatableAreaUnlimited", gainthreshold = gainthreshold,
                    selectyears = selectyears, iniyear = iniyear,
                    lpjml = lpjml, climatetype = climatetype,
                    cropmix = cropmix, yieldcalib = yieldcalib,
                    thresholdtype = thresholdtype, multicropping = multicropping,
                    landScen = landScen, aggregate = FALSE)

    if (multicropping) {

      if (length(selectyears) > 1) {
        stop("Please select one year only when multicropping is selected
             or adjust code in calcYieldGainArea respectively")
      }

      mc <- calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)
      mc <- mc[, , "irrigated"] - mc[, , "rainfed"]

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

    x[[i]]   <- tmp
  }

  out <- mbind(x)
  out <- collapseNames(out)

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Potentially irrigated area only considering land constraint",
              isocountries = FALSE))
}
