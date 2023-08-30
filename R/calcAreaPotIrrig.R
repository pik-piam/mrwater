#' @title       calcAreaPotIrrig
#' @description This function calculates land that is potentially available
#'              for irrigated agriculture
#'
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
#' @param lpjml         If comagyear != NULL: LPJmL version used to calculate committed
#'                      agricultural use
#' @param climatetype   If comagyear != NULL: climate scenario used to calculate committed
#'                      agricultural use
#'                      or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod     If comagyear != NULL: EFR method used to calculate committed
#'                      agricultural use (e.g., Smakhtin:good, VMF:fair)
#' @param multicropping TRUE or FALSE (for committed agricultural area accounting)
#' @param transDist     If comagyear != NULL: Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand by surrounding cell water availability
#'                      of committed agricultural uses
#' @param scenarios     Scenarios for object dimension consisting of "EFP.scenario".
#'                      Default is c("on.ISIMIP",  "on.ssp1", "on.ssp2", "on.ssp3",
#'                                   "off.ISIMIP", "off.ssp1", "off.ssp2", "off.ssp3")
#'                      If scenarios change, they have to be adjusted in the default 
#'                      setting of this function.
#'
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("AreaPotIrrig", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput toolSplitSubtype
#' @importFrom magclass collapseNames getCells getYears getNames dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcAreaPotIrrig <- function(selectyears, comagyear, iniyear, landScen,
                             lpjml, climatetype, efrMethod, fossilGW,
                             multicropping, transDist,
                             scenarios = c("on.ISIMIP",  "on.ssp1", "on.ssp2", "on.ssp3",
                                           "off.ISIMIP", "off.ssp1", "off.ssp2", "off.ssp3")) {

  # retrieve function arguments
  protectSCEN <- as.list(strsplit(landScen, split = ":"))[[1]][2]

  if (is.na(protectSCEN) || protectSCEN == "NULL" || protectSCEN == "NA") {
    protectSCEN <- NA
  }

  landScen <- as.list(strsplit(landScen, split = ":"))[[1]][1]

  ######################
  ### Protected area ###
  ######################
  # read in protected area
  protectArea <- calcOutput("ProtectArea", cells = "lpjcell", aggregate = FALSE)
  # To Do (FELI): Switch to newest protected area function & double check which landarea is the base

  # select protection scenario
  if (!is.na(protectSCEN)) {

    # protection scenario
    protectArea <- collapseNames(protectArea[, , protectSCEN])

  } else {

    # no land protection
    protectArea       <- collapseNames(protectArea[, , "WDPA"])
    protectArea[, , ] <- 0
  }

  #####################################################
  ### Available land (dependent on chosen scenario) ###
  #####################################################

  if (grepl("potCropland", landScen)) {

    # read in suitable land based on Zabel [in mio. ha]
    # excluding land that is marginal under irrigated conditions (< suitability index of 0.33)
    landEXCLmarginal <- collapseNames(calcOutput("AvlCropland", aggregate = FALSE,
                       marginal_land = "no_marginal:irrigated",
                       cells = "lpjcell"))
    # including land that is marginal under irrigated conditions (< suitability index of 0.33)
    landINCLmarginal  <- collapseNames(calcOutput("AvlCropland", aggregate = FALSE,
                       marginal_land = "all_marginal:irrigated",
                       cells = "lpjcell"))
    marginalLand <- landINCLmarginal - landEXCLmarginal
    # marginal lands are prioritized in protection
    # (subtract marginal areas to avoid double counting)
    protectArea  <- pmax(protectArea - marginalLand, 0)

    landAVL <- landEXCLmarginal


  } else if (grepl("curr", landScen)) {

    if (landScen == "currCropland") {

      # Total current physical cropland per cell:
      landAVL <- dimSums(calcOutput("CropareaAdjusted", iniyear = iniyear,
                                 aggregate = FALSE),
                      dim = 3)

    }

    if (landScen == "currIrrig") {

      # Total irrigated physical cropland per cell:
      landAVL <- dimSums(collapseNames(calcOutput("CropareaAdjusted", iniyear = iniyear,
                                               aggregate = FALSE)[, , "irrigated"]),
                      dim = 3)
    }

  } else {
    stop("Please choose an existing available land scenario in landScen argument:
         currIrrig (only currently irrigated cropland available for irrigated agriculture),
         currCropland (only current cropland areas available for irrigated agriculture),
         potCropland (suitable land is available for irrigated agriculture excluding marginal land)")
  }


  ####################################
  ### Calculate non-protected area ###
  ####################################
  # total land area (Note: constant over the years.)
  landarea <- setYears(collapseNames(dimSums(readSource("LUH2v2", subtype = "states_1995to1996",
                                                        convert = "onlycorrect")[, "y1995", ],
                                             dim = 3)),
                       NULL)

  # area that is not protected
  areaNOprotect <- landarea - protectArea

  #########################################################
  ### Land that is potentially available for irrigation ###
  #########################################################
  # Combine land scenario and protection component
  out <- pmin(areaNOprotect, landAVL)

  # Transform object dimensionality
  .transformObject <- function(x, gridcells, years, names) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = gridcells,
                          years = years,
                          names = names,
                          fill = 0,
                          sets = c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out <- object0 + x
    return(out)
  }
  out <- .transformObject(x = out,
                          gridcells = getItems(out, dim = 1),
                          years = selectyears,
                          names = scenarios)

  # Areas that are already irrigated (by committed agricultural uses)
  if (!is.null(comagyear)) {

    # Committed Agricultural Areas under multiple cropping
    if (multicropping != FALSE) {
      multicropping <- "TRUE:actual:irrig_crop"
    }

    # subtract physical area already reserved for irrigation with renewable water resources
    # by committed agricultural uses in water allocation algorithm
    # (to avoid double accounting)
    comIrrigArea <- collapseNames(calcOutput("IrrigAreaActuallyCommitted",
                                             fossilGW = fossilGW,
                                             selectyears = selectyears, iniyear = comagyear,
                                             lpjml = lpjml, climatetype = climatetype,
                                             efrMethod = efrMethod,
                                             multicropping = multicropping, transDist = transDist,
                                             aggregate = FALSE))
    if (any(scenarios != unique(getItems(dimSums(comIrrigArea, dim = "crop"), dim = 3)))) {
      stop("Apparently the number of scenarios or format has changed.
           Please adjust default argument of mrwater::calcAreaPotIrrig accordingly.")
    }
    comIrrigArea <- collapseNames(dimSums(comIrrigArea, dim = "crop"))
    out          <- out - comIrrigArea
  }

  # Checks
  if (any(is.na(out))) {
    stop("mrwater::calcAreaPotIrrig produced NA values")
  }

  if (any(round(out, digits = 6) < 0)) {
    stop("mrwater::calcAreaPotIrrig produced negative values")
  }

  # correct negative land availability due to rounding imprecision
  out[out < 0] <- 0

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Area potentially available for irrigated agriculture",
              isocountries = FALSE))
}
