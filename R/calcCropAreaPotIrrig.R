#' @title       calcCropAreaPotIrrig
#' @description This function calculates croparea that is potentially available
#'              for irrigated agriculture per crop given a chosen cropmix
#'
#' @param cropmix       Cropmix for which croparea share is calculated
#'                      (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops as vector)
#' @param landScen      Land availability scenario consisting of two parts separated by ":":
#'                      1. available land scenario (currCropland, currIrrig, potCropland)
#'                      2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                      For case of no land protection select "NA"
#'                      or do not specify second part of the argument
#' @param iniyear       Initialization year for current cropland area
#' @param selectyears   Years to be returned
#' @param comagyear     If NULL: total potential croparea is used;
#'                      if !NULL: already irrigated area is subtracted;
#'                      year specified here is the year of the initialization
#'                      used for cropland area initialization in calcIrrigatedArea (e.g. NULL, 1995, 2010)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("CropAreaPotIrrig", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput toolSplitSubtype
#' @importFrom magclass collapseNames getCells getYears getNames dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcCropAreaPotIrrig <- function(selectyears, comagyear, iniyear,
                                 cropmix, landScen) {

  # Setting selection for cropmix
  if (grepl("hist", cropmix)) {
    if (grepl("currIrrig", landScen)) {
      cropmix <- "hist_irrig"
    } else if (grepl("currCropland", landScen)) {
      cropmix <- "hist_total"
    }
  }

  ### Read in data ###
  # Land area that can potentially be used for irrigated agriculture
  # given assumptions set in the arguments [in Mha]
  land <- calcOutput("AreaPotIrrig",
                     selectyears = selectyears, iniyear = iniyear,
                     comagyear = NULL, landScen = landScen,
                     aggregate = FALSE)

  # share of crop area by crop type
  cropareaShr <- setYears(calcOutput("CropAreaShare",
                                     iniyear = iniyear, cropmix = cropmix,
                                     aggregate = FALSE),
                          NULL)

  # crop-specific area available for potential irrigation
  out <- cropareaShr * land

  # Areas that are already irrigated (by committed agricultural uses)
  if (!is.null(comagyear)) {

    # subtract physical area already reserved for irrigation with renewable water resources
    # by committed agricultural uses in water allocation algorithm
    # (to avoid double accounting)
    comIrrigArea <- collapseNames(calcOutput("IrrigAreaCommitted",
                                             selectyears = selectyears, iniyear = comagyear,
                                             aggregate = FALSE))
    out <- out - comIrrigArea
  }

  # Checks
  if (any(is.na(out))) {
    stop("Function calcCropAreaPotIrrig produced NA values")
  }

  if (any(round(out, digits = 6) < 0)) {
    stop("Function calcCropAreaPotIrrig produced negative values")
  } else {
    # set negatives due to numerical reasons to 0
    out[out < 0] <- 0
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = paste0("Croparea potentially available for irrigated ",
                                    "agriculture per crop type"),
              isocountries = FALSE))
}
