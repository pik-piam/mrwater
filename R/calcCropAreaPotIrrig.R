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

  # Setting selection for historical period
  if (grepl("hist", cropmix)) {
    # To ensure that the crop-specific areas match,
    # the cropmix argument has to be adjusted
    # depending on the chosen setting
    if (grepl("currIrrig", landScen)) {
      # When only currently irrigated areas are considered
      # the irrigated cropmix must be chosen
      cropmix <- "hist_irrig"
    } else {
      # When no committed agricultural areas are reserved
      # the total historical cropmix must be chosen
      cropmix <- "hist_total"
      # Special treatment of committed agricultural case
      # takes place further down
    }
  } else {
    warning("Please note: when proxy crops are selected as cropmix,
    there are most likely mismatches in the area accounting of the historical period.
    For analyses focusing on historical periods: please choose the historical cropmix.
    For future analysis that are supposed to meet the initialization year,
    choose a wise setting (check: calcCropAreaPotIrrig)") # To Do
  }

  ### Read in data ###
  # Land area that can potentially be used for irrigated agriculture
  # given assumptions set in the arguments including reservation
  # (i.e. subtracting) already committed agriculture [in Mha]
  land <- calcOutput("AreaPotIrrig",
                     selectyears = selectyears, iniyear = iniyear,
                     comagyear = comagyear, landScen = landScen,
                     aggregate = FALSE)

  if (!is.null(comagyear)) {
    # committed agricultural areas have already been subtracted
    # remaining areas are rainfed (i.e. rainfed cropmix)
    cropmix <- "hist_rainf"
  }

  # share of crop area by crop type
  cropareaShr <- setYears(calcOutput("CropAreaShare",
                                    iniyear = iniyear, cropmix = cropmix,
                                    aggregate = FALSE),
                          NULL)

  out <- cropareaShr * land

  # Checks
  if (any(is.na(out))) {
    stop("Function calcCropAreaPotIrrig produced NA values")
  }

  if (any(round(out, digits = 3) < 0)) {
    stop("Function calcCropAreaPotIrrig produced negative values")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = paste0("croparea potentially available for irrigated",
                                    "agriculture per crop types"),
              isocountries = FALSE))
}
