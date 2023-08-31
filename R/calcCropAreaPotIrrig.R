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
#' @param fossilGW      This argument is only relevant when comagyear != NULL
#'                      as it determines the area that is set aside as it is already
#'                      reserved by the committed agriculture iteration.
#'                      If TRUE: non-renewable groundwater can be used.
#'                      If FALSE: non-renewable groundwater cannot be used.
#' @param lpjml         if comagyear != NULL: LPJmL version used to calculate committed
#'                      agricultural use
#' @param climatetype   if comagyear != NULL: climate scenario used to calculate committed
#'                      agricultural use
#'                      or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod     if comagyear != NULL: EFR method used to calculate committed
#'                      agricultural use (e.g., Smakhtin:good, VMF:fair)
#' @param multicropping TRUE or FALSE (for committed agricultural area accounting)
#' @param transDist     if comagyear != NULL: Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand by surrounding cell water availability
#'                      of committed agricultural uses
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
                                 cropmix, landScen, fossilGW,
                                 lpjml, climatetype, efrMethod,
                                 multicropping, transDist) {

  # Check before proceeding
  if (!is.logical(multicropping)) {
    stop("calcCropAreaPotIrrig requires logical
         in multicropping argument.")
  }

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
      # Special treatment of committed agricutlural case
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
  # (i.e. subtracting) already committed agriculture that is fulfilled with surface water
  # availability [in Mha]
  # Note: areas reserved with fossilGW are not considered as they are added in the end
  land <- calcOutput("AreaPotIrrig",
                    selectyears = selectyears, iniyear = iniyear,
                    comagyear = comagyear, landScen = landScen,
                    lpjml = lpjml, climatetype = climatetype,
                    efrMethod = efrMethod, fossilGW = fossilGW,
                    multicropping = multicropping, transDist = transDist,
                    aggregate = FALSE)

  # share of crop area by crop type
  cropareaShr <- setYears(calcOutput("CropAreaShare",
                                    iniyear = iniyear, cropmix = cropmix,
                                    aggregate = FALSE),
                          NULL)

  ### Calculations ###
  # Crop-specific area that is potentially available
  # to be irrigated
  if (!is.null(comagyear)) {
    # If committed agricultural areas are considered
    # the correct cropmix has to be chosen for all areas that are provided
    # as potentially available land for irrigation to the algorithm.

    ### Read in additional data ###
    # Total potentially available area without consideration of previously
    # committed areas [in Mha]
    total <- calcOutput("AreaPotIrrig",
      selectyears = selectyears, iniyear = iniyear,
      comagyear = NULL, landScen = landScen,
      lpjml = lpjml, climatetype = climatetype,
      efrMethod = efrMethod,
      multicropping = multicropping, transDist = transDist,
      aggregate = FALSE)

    # Area that would be committed to agriculture [in Mha]
    comAgArea <- dimSums(calcOutput("IrrigAreaCommitted",
                                    selectyears = selectyears, iniyear = iniyear,
                                    aggregate = FALSE),
                        dim = "crop")
    comAgArea <- add_dimension(comAgArea, dim = 3.1, add = "EFP", nm = getItems(land, dim = 3.1))
    comAgArea <- add_dimension(comAgArea, dim = 3.2, add = "scen", nm = getItems(land, dim = 3.2))
    comAgArea <- comAgArea[, , getItems(land, dim = 3)]
    if (!identical(
      collapseNames(comAgArea[, , "on.ISIMIP"]),
      collapseNames(comAgArea[, , "off.ssp2"]))) {
      stop("Something went wrong in the dimension expansion of mrwater::calcCropAreaPotIrrig.")
    }

    # Cropmixes for crop-specific area accounting
    if (grepl("currIrrig", landScen)) {
      rfdCropmix <- irrCropmix <- totCropmix <- setYears(calcOutput("CropAreaShare",
                                                                    iniyear = iniyear,
                                                                    cropmix = "hist_irrig",
                                                                    aggregate = FALSE),
                                                         NULL)

    } else {
      # rainfed cropmix
      rfdCropmix <- setYears(calcOutput("CropAreaShare",
                                        iniyear = iniyear, cropmix = "hist_rainf",
                                        aggregate = FALSE),
                             NULL)
      # irrigated cropmix
      irrCropmix <- setYears(calcOutput("CropAreaShare",
                                        iniyear = iniyear, cropmix = "hist_irrig",
                                        aggregate = FALSE),
                             NULL)
      # total cropmix
      totCropmix <- setYears(calcOutput("CropAreaShare",
                                        iniyear = iniyear, cropmix = "hist_total",
                                        aggregate = FALSE),
                             NULL)
    }

    ### Calculations ###
    # Area that is actually committed as seen by the algorithm
    actComAgArea <- total - land
    if (any(round(actComAgArea, digits = 6) < 0)) {
      stop("There seems to be a mismatch in committed agricultural areas.
            Please check starting in mrwater::calcCropAreaPotIrrig")
    }

    # Additional area of currently irrigated area that has not been reserved
    currIRnores <- comAgArea - actComAgArea
    if (any(round(currIRnores, digits = 6) < 0)) {
      stop("There seems to be a mismatch in committed agricultural areas.
        Please check starting in mrwater::calcCropAreaPotIrrig")
    }

    # Currently rainfed areas that may be potentially irrigated [in Mha]
    currRF <- total - comAgArea
    if (any(round(currRF, digits = 6) < 0)) {
      stop("There seems to be a mismatch in committed agricultural areas.
      Please check starting in mrwater::calcCropAreaPotIrrig")
    }

    # crop-specific land area that is available for potential irrigation
    out <- currRF * rfdCropmix + currIRnores * irrCropmix

    ### Checks ###
    if (any(round(total * totCropmix - out - actComAgArea * irrCropmix, digits = 6) < 0)) {
      stop("There is a mismatch in the crop-specific irrigation area accounting.
        Please check what is wrong starting in mrwater::calcCropAreaPotIrrig")
    }
  } else {
    # When no areas have been reserved for committed agricultural use
    # the total available land area is multiplied with the chosen cropmix
    out <- land * cropareaShr
  }

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
