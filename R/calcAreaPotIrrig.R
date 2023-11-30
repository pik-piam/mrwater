#' @title       calcAreaPotIrrig
#' @description This function calculates land that is potentially available
#'              for irrigated agriculture
#'
#' @param landScen      Land availability scenario consisting of two parts separated by ":":
#'                      1. available land scenario (currCropland, currIrrig, potCropland)
#'                      2. protection scenario (WDPA, or one of the scenarios available in calcConservationPriorities,
#'                         e.g., 30by20, BH, BH_IFL, PBL_HalfEarth,
#'                         or NA for no protection).
#'                      For case of no land protection select "NA" in second part of argument
#'                      or do not specify second part of the argument
#' @param iniyear       Initialization year for current cropland area
#' @param selectyears   Years to be returned
#' @param comagyear     If NULL: total potential croparea is used;
#'                      if !NULL: already irrigated area is subtracted;
#'                      year specified here is the year of the initialization
#'                      used for cropland area initialization in calcIrrigatedArea (e.g. NULL, 1995, 2010)
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
#' @importFrom magclass collapseNames getCells getYears getNames dimSums time_interpolate
#' @importFrom mrcommons toolGetMappingCoord2Country
#' @importFrom mstools toolHoldConstant

calcAreaPotIrrig <- function(selectyears, comagyear, iniyear, landScen) {

  # transform selectyears to numeric
  if (is.character(selectyears)) {
    selectyears <- as.integer(gsub("y", "", selectyears))
  }

  # retrieve function arguments
  protectSCEN <- as.list(strsplit(landScen, split = ":"))[[1]][2]

  if (is.na(protectSCEN) || protectSCEN == "NULL" || protectSCEN == "NA") {
    protectSCEN <- NA
  }

  landScen <- as.list(strsplit(landScen, split = ":"))[[1]][1]

  # total land area (Note: constant over the years)
  # excluding urban area
  landarea <- dimSums(calcOutput("LanduseInitialisation",
                                 cellular = TRUE, cells = "lpjcell",
                                 nclasses = "seven", input_magpie = TRUE,
                                 years = "y1995",
                                 aggregate = FALSE)[, , "urban", invert = TRUE],
                      dim = 3)
  landarea <- toolFillYears(setYears(landarea,
                                     iniyear),
                            selectyears)

  # To Do: include urban land expansion (for differnet scenarios)
  # and make output of calcAreaPotIrrig scenario-specific
  # Note: then urban area must be left in above!
  # Note: follow-up functions must be adjusted
  # exclude urban area
  # urbanLand <- calcOutput("UrbanLandFuture", subtype = "LUH2v2",
  #                         timestep = "yearly", cells = "lpjcell",
  #                         aggregate = FALSE)[, selectyears, ]
  # getItems(urbanLand, dim = 3) <- gsub("SSP", "ssp", getItems(urbanLand, dim = 3))

  # Read in suitable land for irrigation based on Zabel [in mio. ha]
  # excluding land that is marginal under irrigated conditions (< suitability index of 0.33)
  landEXCLmarginal <- toolFillYears(setYears(collapseNames(calcOutput("AvlCropland", luhBaseYear = iniyear,
                                                                      aggregate = FALSE,
                                                                      marginal_land = "no_marginal:irrigated",
                                                                      cells = "lpjcell")),
                                             iniyear),
                                    selectyears)
  # Correct mismatch areas between Zabel and LUH
  landEXCLmarginal <- pmin(landEXCLmarginal, landarea)

  # Read in areas that are already irrigated
  comIrrigArea <- collapseNames(calcOutput("IrrigAreaCommitted",
                                           selectyears = selectyears, iniyear = iniyear,
                                           aggregate = FALSE))
  comIrrigArea <- collapseNames(dimSums(comIrrigArea, dim = "crop"))

  # areas that are currently irrigated must also be suitable under irrigated conditions
  landEXCLmarginal <- pmax(landEXCLmarginal, comIrrigArea)

  ######################
  ### Protected area ###
  ######################
  # Future protection scenarios
  conservationAreas <- toolFillYears(setYears(calcOutput("ConservationPriorities",
                                                         nclasses = "seven", cells = "lpjcell",
                                                         aggregate = FALSE),
                                              iniyear),
                                     selectyears)
  conservationAreas <- dimSums(conservationAreas[, , "urban", invert = TRUE],
                               dim = 3.2)
  conservationAreas <- add_columns(conservationAreas, dim = 3, addnm = "WDPA", fill = 0)

  # WDPA protection baseline
  wdpa <- dimSums(calcOutput("ProtectedAreaBaseline", nclasses = "seven",
                             cells = "lpjcell", magpie_input = TRUE,
                             aggregate = FALSE)[, , "urban", invert = TRUE],
                  dim = 3)
  if (any(selectyears > as.integer(gsub("y", "", tail(getItems(wdpa, dim = 2), n = 1))))) {
    wdpa <- toolHoldConstant(x = wdpa, years = selectyears)
  }
  if (!identical(numeric(0),
                 setdiff(selectyears, as.integer(gsub("y", "", getItems(wdpa, dim = 2)))))) {
    wdpa <- time_interpolate(dataset = wdpa,
                             interpolated_year = selectyears,
                             integrate_interpolated_years = TRUE,
                             extrapolation_type = "linear")
  }
  wdpa <- wdpa[, selectyears, ]

  # Protected areas consist of WDPA baseline protection and
  # additional protection by scenario
  protectArea <- conservationAreas + wdpa

  # select protection scenario
  if (!is.na(protectSCEN)) {

    # protection scenario
    protectArea <- collapseNames(protectArea[, , protectSCEN])

  } else {

    # no land protection
    protectArea       <- collapseNames(protectArea[, , "WDPA"])
    protectArea[, , ] <- 0
  }

  # Correct mismatch between protected area and landarea
  protectArea <- pmin(protectArea, landarea)

  #####################################################
  ### Available land (dependent on chosen scenario) ###
  #####################################################

  if (grepl("potCropland", landScen)) {

    # All land that is suitable for cropping under irrigated conditions according to Zabel
    # can be used for irrigation
    landAVL <- landEXCLmarginal

    # Treatment of protected areas
    # read in suitable land for irrigation based on Zabel [in mio. ha]
    # including land that is marginal under irrigated conditions (< suitability index of 0.33)
    landINCLmarginal <- toolFillYears(setYears(collapseNames(calcOutput("AvlCropland", luhBaseYear = iniyear,
                                                                        aggregate = FALSE,
                                                                        marginal_land = "all_marginal:irrigated",
                                                                        cells = "lpjcell")),
                                               iniyear),
                                      selectyears)
    # Correct mismatch areas between Zabel and LUH
    landEXCLmarginal <- pmin(landINCLmarginal, landarea)
    # areas that are currently irrigated must also be suitable under irrigated conditions
    landINCLmarginal <- pmax(landINCLmarginal, comIrrigArea)
    # calculate marginal land
    marginalLand <- landINCLmarginal - landEXCLmarginal
    # marginal lands are prioritized in protection
    # (subtract marginal areas to avoid double counting)
    protectArea  <- pmax(protectArea - marginalLand, 0)

  } else if (grepl("curr", landScen)) {

    if (landScen == "currCropland") {

      # Total current physical cropland per cell:
      landAVL <- dimSums(calcOutput("CropareaAdjusted", iniyear = iniyear,
                                    aggregate = FALSE),
                         dim = 3)
      # Only cropland that is suitable under irrigated conditions according
      # to Zabel can be used for irrigation
      landAVL <- pmin(landAVL, landEXCLmarginal)

    }

    if (landScen == "currIrrig") {

      # Total irrigated physical cropland per cell:
      landAVL <- dimSums(collapseNames(calcOutput("CropareaAdjusted", iniyear = iniyear,
                                                  aggregate = FALSE)[, , "irrigated"]),
                         dim = 3)

      # Only cropland that is suitable under irrigated conditions according
      # to Zabel can be used for irrigation
      landAVL <- pmin(landAVL, landEXCLmarginal)
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
  # area that is not protected
  areaNOprotect <- landarea - protectArea

  # correct areas where more area is protected than land is available
  if (any(areaNOprotect < 0)) {
    stop("There are negative values for the areas that are not protected in
          mrwater::calcAreaPotIrrig. This should no longer be the case when
          using the LanduseIntialisation & ConservationPriorities.
          Please double-check!")
    areaNOprotect[areaNOprotect < 0] <- 0
  }

  #########################################################
  ### Land that is potentially available for irrigation ###
  #########################################################
  # Combine land scenario and protection component
  out <- pmin(areaNOprotect, landAVL)

  # Areas that are already irrigated (by committed agricultural uses)
  if (!is.null(comagyear)) {
    # subtract physical area already reserved for irrigation
    out <- out - comIrrigArea
    if (!is.na(protectSCEN)) {
      # correct negative areas that can occur due to protection
      out <- pmax(out, 0)
    }
  }

  # Checks
  if (any(is.na(out))) {
    stop("mrwater::calcAreaPotIrrig produced NA values")
  }

  if (any(round(out, digits = 6) < 0)) {
    stop("mrwater::calcAreaPotIrrig produced negative values")
  }

  # correct negative land availability caused by numerical reasons
  out[out < 0] <- 0

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Area potentially available for irrigated agriculture",
              isocountries = FALSE))
}
