#' @title       calcAreaPotIrrig
#' @description This function calculates land that is potentially available
#'              for irrigated agriculture
#'
#' @param landScen      Land availability scenario consisting of two parts separated by ":":
#'                      1. available land scenario (currCropland, currIrrig, potCropland)
#'                      2. protection scenario (WDPA, or one of the scenarios available in calcConservationPriorities,
#'                         e.g., 30by30, BH, BH_IFL, PBL_HalfEarth,
#'                         or NA for no protection).
#'                      For case of no land protection select "NA" in second part of argument
#'                      or do not specify second part of the argument.
#' @param cropAggregation TRUE (aggregate over crop types), FALSE (returns area per crop type)
#' @param cropmix         Cropmix for which potential areas are calculated
#'                        (options:
#'                        "hist_irrig" for historical cropmix on currently irrigated area,
#'                        "hist_rainf" for historical cropmix on currently irrigated area,
#'                        "hist_total" for historical cropmix on total cropland,
#'                        or selection of proxycrops)
#' @param iniyear       Initialization year for current cropland area
#' @param selectyears   Years to be returned
#' @param comAg         If TRUE: committed irrigated areas are subtracted,
#'                      if FALSE: total potential croparea is used
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("AreaPotIrrig", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput toolSplitSubtype toolFillYears
#' @importFrom magclass collapseNames getCells getYears getNames dimSums time_interpolate
#' @importFrom mstools toolHoldConstant toolGetMappingCoord2Country

calcAreaPotIrrig <- function(selectyears, comAg,
                             cropAggregation, cropmix,
                             iniyear, landScen) {

  ### To Do: check where and how this function is used and maybe re-write to crop-specific, so that it is always consistent

  # transform selectyears to numeric
  if (is.character(selectyears)) {
    selectyears <- as.integer(gsub("y", "", selectyears))
  }

  # retrieve function arguments
  protectSCEN <- as.list(strsplit(landScen, split = ":"))[[1]][2]

  if (is.na(protectSCEN) || identical(protectSCEN, "NULL") || identical(protectSCEN, "NA")) {
    protectSCEN <- NA
  }

  landSCEN <- as.list(strsplit(landScen, split = ":"))[[1]][1]

  # Setting selection for cropmix
  if (any(grepl("hist", cropmix))) {
    # If current irrigation is chosen as land scenario,
    # the crop mix should be hist_irrig accordingly
    if (grepl("currIrrig", landSCEN)) {
      cropmix <- "hist_irrig"
    }
  }
  ### To Do: Discuss with Jens, Jan, Benni: using proxycrops with comAg can cause inconsistencies
  ###        comAg fades out over time, let proxycrops fade in over time?
  ###        Or: better use hist cropmix throughout? Maybe less inconsistent then trying to fix inconsistencies / adding areas?

  # share of crop area by crop type for iniyear and chosen cropmix
  cropareaShr <- setYears(calcOutput("CropAreaShare",
                                     iniyear = iniyear, cropmix = cropmix,
                                     aggregate = FALSE),
                          NULL)


  # total land area (Note: constant over the years)
  # excluding urban area times croparea share to obtain crop-specific areas
  landarea <- cropareaShr * dimSums(calcOutput("LanduseInitialisation",
                                               cellular = TRUE,
                                               nclasses = "seven", input_magpie = TRUE,
                                               years = iniyear,
                                               aggregate = FALSE)[, , "urban", invert = TRUE],
                                    dim = 3)
  landarea <- toolFillYears(setYears(landarea,
                                     iniyear),
                            selectyears)

  # To Do: include urban land expansion (for different scenarios)
  # and make output of calcAreaPotIrrig scenario-specific
  # Note: then urban area must be left in above!
  # Note: follow-up functions must be adjusted
  # exclude urban area
  # urbanLand <- calcOutput("UrbanLandFuture", subtype = "LUH3",
  #                         timestep = "yearly",
  #                         aggregate = FALSE)[, selectyears, ]
  # getItems(urbanLand, dim = 3) <- gsub("SSP", "ssp", getItems(urbanLand, dim = 3)) # nolint: comment_code_linter

  # Read in suitable land for irrigation based on Zabel [in mio. ha]
  # excluding land that is marginal under irrigated conditions (< suitability index of 0.33)
  landEXCLmarginal <- cropareaShr * collapseNames(calcOutput("AvlCropland", luhBaseYear = iniyear,
                                                             aggregate = FALSE,
                                                             marginal_land = "no_marginal:irrigated"))
  landEXCLmarginal <- toolFillYears(setYears(landEXCLmarginal,
                                             iniyear),
                                    selectyears)

  # Correct mismatch areas between Zabel and LanduseInitialisation data
  landEXCLmarginal <- pmin(landEXCLmarginal, landarea)

  # Read in areas that are already irrigated
  comIrrigArea <- collapseNames(calcOutput("IrrigAreaCommitted",
                                           selectyears = selectyears, iniyear = iniyear,
                                           aggregate = FALSE))

  # areas that are currently irrigated must also be suitable under irrigated conditions
  landEXCLmarginal <- pmax(landEXCLmarginal, comIrrigArea)

  ######################
  ### Protected area ###
  ######################
  if (!is.na(protectSCEN)) {
    # Read in protected area data

    # Future protection scenarios
    conservationAreas <- toolFillYears(setYears(calcOutput("ConservationPriorities",
                                                           nclasses = "seven",
                                                           aggregate = FALSE),
                                                iniyear),
                                       selectyears)
    conservationAreas <- dimSums(conservationAreas[, , "urban", invert = TRUE],
                                 dim = 3.2)
    conservationAreas <- add_columns(conservationAreas, dim = 3, addnm = "WDPA", fill = 0)

    # WDPA protection baseline
    wdpa <- dimSums(calcOutput("ProtectedAreaBaseline", nclasses = "seven",
                               magpie_input = TRUE,
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

    # select protection scenario and multiply with croparea to obtain crop-specific areas
    # assuming same protection share excluded from each crop
    protectArea <- cropareaShr * collapseNames(protectArea[, , protectSCEN])

  } else {

    # no land protection
    protectArea       <- landarea
    protectArea[, , ] <- 0
  }

  # Correct mismatch between protected area and landarea
  protectArea <- pmin(protectArea, landarea)

  #####################################################
  ### Available land (dependent on chosen scenario) ###
  #####################################################

  if (grepl("potCropland", landSCEN)) {

    # All land that is suitable for cropping under irrigated conditions according to Zabel
    # can be used for irrigation
    landAVL <- landEXCLmarginal

    # Treatment of protected areas
    # read in suitable land for irrigation based on Zabel [in mio. ha]
    # including land that is marginal under irrigated conditions (< suitability index of 0.33)
    landINCLmarginal <- cropareaShr * collapseNames(calcOutput("AvlCropland", luhBaseYear = iniyear,
                                                               aggregate = FALSE,
                                                               marginal_land = "all_marginal:irrigated"))
    landINCLmarginal <- toolFillYears(setYears(landINCLmarginal,
                                               iniyear),
                                      selectyears)

    # Correct mismatch areas between Zabel and LanduseInitialisation data
    landINCLmarginal <- pmin(landINCLmarginal, landarea)

    # areas that are currently irrigated must also be suitable under irrigated conditions
    landINCLmarginal <- pmax(landINCLmarginal, comIrrigArea)

    # calculate marginal land
    marginalLand <- landINCLmarginal - landEXCLmarginal
    # marginal lands are prioritized in protection
    # (subtract marginal areas to avoid double counting)
    protectArea  <- pmax(protectArea - marginalLand, 0)

  } else if (grepl("curr", landSCEN)) {

    if (landSCEN == "currCropland") {

      # Crop-specific current physical cropland per cell:
      landAVL <- toolFillYears(dimSums(calcOutput("CropareaAdjusted", iniyear = iniyear,
                                                  aggregate = FALSE),
                                       dim = "irrigation"),
                               selectyears)
      # Only cropland that is suitable under irrigated conditions according
      # to Zabel can be used for irrigation
      landAVL <- pmin(landAVL, landEXCLmarginal)
    }

    if (landSCEN == "currIrrig") {

      # Crop-specific irrigated physical cropland per cell:
      landAVL <-  toolFillYears(collapseNames(calcOutput("CropareaAdjusted", iniyear = iniyear,
                                                         aggregate = FALSE)[, , "irrigated"]),
                                selectyears)

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
  if (comAg) {

    # sanity check
    if (cropmix == "hist_rainf") {
      warning("Is the combination of arguments `cropmix = hist_rainf` and `comAg = TRUE` intended? ",
              "It likely leads to mismatches in areas and potentially to negative CropAreaPotIrrig values.")
    }

    # subtract physical area already reserved for irrigation
    out <- out - comIrrigArea
    if (!is.na(protectSCEN)) {
      # correct negative areas that can occur due to protection
      out <- pmax(out, 0)
    }
  }

  # Aggregation over crops
  if (cropAggregation) {
    out <- dimSums(out, dim = "crop")
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
