#' @title       calcCropAreaPotIrrig
#' @description This function calculates croparea that is potentially available
#'              for irrigated agriculture per crop given a chosen cropmix
#'
#' @param selectyears Years to be returned
#' @param comagyear   If NULL: total potential croparea is used;
#'                    if !NULL: already irrigated area is subtracted;
#'                    year specified here is the year of the initialization
#'                    used for cropland area initialization in calcIrrigatedArea (e.g. NULL, 1995, 2010)
#' @param iniyear     Initialization year for current cropland area
#' @param cropmix     Cropmix for which croparea share is calculated
#'                    (options:
#'                    "hist_irrig" for historical cropmix on currently irrigated area,
#'                    "hist_total" for historical cropmix on total cropland,
#'                    or selection of proxycrops as vector)
#' @param landScen    Land availability scenario consisting of two parts separated by ":":
#'                    1. available land scenario (currCropland, currIrrig, potCropland)
#'                    2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                    For case of no land protection select "NA"
#'                    or do not specify second part of the argument
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

  # land area that can potentially be used for irrigated agriculture
  # given assumptions set in the arguments [in Mha]
  land <- calcOutput("AreaPotIrrig",
                     selectyears = selectyears, iniyear = iniyear,
                     comagyear = comagyear, landScen = landScen,
                     aggregate = FALSE)

  # share of corp area by crop type
  cropareaShr <- calcOutput("CropAreaShare",
                            iniyear = iniyear, cropmix = cropmix,
                            aggregate = FALSE)

  # land area per crop
  out <- land * cropareaShr

  # Checks
  if (any(is.na(out))) {
    stop("Function calcCropAreaPotIrrig produced NA values")
  }

  if (any(round(out) < 0)) {
    stop("Function calcCropAreaPotIrrig produced negative values")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "croparea potentially available for irrigated
                              agriculture per crop types",
              isocountries = FALSE))
}
