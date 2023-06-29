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
#' @param lpjml         if comagyear != NULL: LPJmL version used to calculate committed
#'                      agricultural use
#' @param climatetype   if comagyear != NULL: climate scenario used to calculate committed
#'                      agricultural use
#'                      or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod     if comagyear != NULL: EFR method used to calculate committed
#'                      agricultural use (e.g., Smakhtin:good, VMF:fair)
#' @param multicropping TRUE or FALSE (for committed agricultural area accounting)
#' @param transDist      if comagyear != NULL: Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#'                       of committed agricultural uses
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
                             lpjml, climatetype, efrMethod,
                             multicropping, transDist) {

  # retrieve function arguments
  protectSCEN <- as.list(strsplit(landScen, split = ":"))[[1]][2]

  if (is.na(protectSCEN) || protectSCEN == "NULL" || protectSCEN == "NA") {
    protectSCEN <- NA
  }

  landScen    <- as.list(strsplit(landScen, split = ":"))[[1]][1]

  ######################
  ### Protected area ###
  ######################
  # read in protected area
  protectArea <- calcOutput("ProtectArea", cells = "lpjcell", aggregate = FALSE)
  # FELI: Switch to newest protected area function & double check which landarea is the base

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


  # Adjust dimensionality (necessary when >1 year selected)
  tmp <- out
  out <- new.magpie(cells_and_regions  = getCells(out),
                     years             = selectyears,
                     names             = getNames(out),
                     fill              = 1)
  out <- tmp * out
  getSets(out) <- c("x", "y", "iso", "year", "data")

  # Areas that are already irrigated (by committed agricultural uses)
  if (!is.null(comagyear)) {

    # Committed Agricultural Areas under multiple cropping
    if (multicropping) {
      multicropping <- "TRUE:actual:irrig_crop"
    }

    # subtract physical area already reserved for irrigation by committed agricultural uses
    # (to avoid double accounting)
    comIrrigArea <- collapseNames(calcOutput("IrrigAreaActuallyCommitted",
                                              selectyears = selectyears, iniyear = comagyear,
                                              lpjml = lpjml, climatetype = climatetype,
                                              efrMethod = efrMethod,
                                              multicropping = multicropping, transDist = transDist,
                                              aggregate = FALSE))
    comIrrigArea <- collapseNames(dimSums(comIrrigArea, dim = 3))
    out          <- out - comIrrigArea
  }

  # correct negative land availability due to mismatch of available land
  # and protected land or rounding imprecision
  out[out < 0] <- 0

  # Checks
  if (any(is.na(out))) {
    stop("Function AreapotCropland produced NA values")
  }

  if (any(round(out, digits = 3) < 0)) {
    stop("Function AreapotCropland produced negative values")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "area potentially available for irrigated agriculture",
              isocountries = FALSE))
}
