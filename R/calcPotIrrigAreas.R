#' @title       calcPotIrrigAreas
#' @description Calculates Potentially Irrigated Areas (PIAs) per crop
#'              given available water and land
#'
#' @param cropAggregation   TRUE (aggregated Potentially Irrigated Areas (PIAs)),
#'                          FALSE (crop-specific PIAs)
#' @param lpjml             LPJmL version used
#' @param climatetype       Switch between different climate scenarios or
#'                          historical baseline "GSWP3-W5E5:historical"
#' @param selectyears       Years for which irrigatable area is calculated
#' @param iniyear           Initialization year for initial croparea
#' @param efrMethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          USD_ha (USD per hectare) for relative area return, or
#'                          USD_m3 (USD per cubic meter) for relative volumetric return;
#'                          USD for absolute return (total profit);
#'                          Price aggregation:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices
#'                          and boolean indicating fullpotential (TRUE, i.e. cell
#'                          receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                          receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (in USD per hectare)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, or one of the scenarios available in calcConservationPriorities,
#'                             e.g., 30by20, BH, BH_IFL, PBL_HalfEarth,
#'                             or NA for no protection).
#'                          For case of no land protection select "NA" in second part of argument
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param comAg             If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param fossilGW          If TRUE: non-renewable groundwater can be used.
#'                          If FALSE: non-renewable groundwater cannot be used.
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          (mask can be:
#'                          "none": no mask applied (only for development purposes)
#'                          "actual:total": currently multicropped areas calculated from total harvested areas
#'                                          and total physical areas per cell from LandInG
#'                          "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                          "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                          "potential:endogenous": potentially multicropped areas given
#'                                                  temperature and productivity limits
#'                          "potential:exogenous": potentially multicropped areas given
#'                                                 GAEZ suitability classification)
#'                          (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("PotIrrigAreas", aggregate = FALSE)
#' }
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames add_dimension add_columns mbind

calcPotIrrigAreas <- function(cropAggregation,
                              lpjml, climatetype,
                              selectyears, iniyear,
                              efrMethod, accessibilityrule,
                              rankmethod, yieldcalib, allocationrule,
                              gainthreshold, irrigationsystem, landScen,
                              cropmix, comAg, fossilGW,
                              multicropping, transDist) {

  # Ensure that cropmix argument is set correctly
  if (grepl("hist", cropmix)) {
    if (grepl("currIrrig", landScen)) {
      cropmix <- "hist_irrig"
    } else if (grepl("currCropland", landScen)) {
      cropmix <- "hist_total"
    }
  }

  ## Read in (renewable and non-renewable) water available for irrigation (in mio. m^3)
  #  including committed agricultural water (if activated)
  avlWat <- calcOutput("PotWater", selectyears = selectyears,
                       lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod,
                       accessibilityrule = accessibilityrule, rankmethod = rankmethod,
                       yieldcalib = yieldcalib, allocationrule = allocationrule,
                       gainthreshold = gainthreshold,
                       irrigationsystem = irrigationsystem, iniyear = iniyear,
                       landScen = landScen, cropmix = cropmix,
                       comAg = comAg, multicropping = multicropping,
                       transDist = transDist, fossilGW = fossilGW,
                       aggregate = FALSE)
  avlWatWC <- collapseNames(avlWat[, , "wat_ag_wc"])
  avlWatWW <- collapseNames(avlWat[, , "wat_ag_ww"])
  # extract right order of third dimension
  scenarios <- getItems(avlWatWW, dim = 3)

  if (comAg) {

    # multiple cropping as of current multiple cropping pattern
    m <- as.logical(stringr::str_split(multicropping, ":")[[1]][1])
    if (m) {
      m <- "TRUE:actual:irrig_crop"
    } else {
      m <- FALSE
    }
    comagyear <- iniyear

    # Actually committed irrigated area (crop-specific) (in Mha)
    # including non-renewable groundwater (if activated)
    comAgArea <- calcOutput("IrrigAreaActuallyCommitted",
                            fossilGW = fossilGW,
                            lpjml = lpjml, climatetype = climatetype,
                            selectyears = selectyears, iniyear = iniyear,
                            efrMethod = efrMethod, multicropping = m,
                            transDist = transDist, aggregate = FALSE)
    getSets(comAgArea) <- c("x", "y", "iso", "year", "crop", "EFP", "scen")
    comAgArea <- collapseNames(dimOrder(comAgArea, perm = c(2, 3, 1), dim = 3))

    # Water actually committed to agriculture (in mio. m^3)
    # including non-renewable groundwater (if activated)
    comWatAct <- calcOutput("WaterUseActuallyCommittedAg",
                            lpjml = lpjml, climatetype = climatetype,
                            selectyears = selectyears, iniyear = iniyear,
                            multicropping = m, efrMethod = efrMethod,
                            fossilGW = fossilGW, transDist = transDist,
                            aggregate = FALSE)
    comWatWW <- collapseNames(dimSums(comWatAct, dim = "crop")[, , "withdrawal"])
    comWatWC <- collapseNames(dimSums(comWatAct, dim = "crop")[, , "consumption"])

    # Water committed to intensify currently irrigated areas to full multiple
    # cropping potential (in mio. m^3)
    if (as.logical(stringr::str_split(multicropping, ":")[[1]][1])) {
      # water required to expand multiple cropping in already irrigated areas and can
      # be fulfilled by renewable water resources
      currHumanAdd <- calcOutput("RiverHumanUseAccounting",
                                 iteration = "committed_agriculture_fullMulticropping",
                                 lpjml = lpjml, climatetype = climatetype,
                                 transDist = transDist, comAg = NULL,
                                 efrMethod = efrMethod, multicropping = multicropping,
                                 selectyears = selectyears, iniyear = iniyear,
                                 accessibilityrule = NULL,
                                 rankmethod = NULL, gainthreshold = NULL,
                                 cropmix = NULL, yieldcalib = NULL,
                                 irrigationsystem = NULL, landScen = NULL,
                                 aggregate = FALSE)
      comWatWC <- comWatWC + collapseNames(currHumanAdd[, , "currHumanWCtotal"])
      comWatWW <- comWatWW + collapseNames(currHumanAdd[, , "currHumanWWtotal"])
    }
  } else {
    comagyear <- NULL

    # No water or areas committed to current agricultural uses
    comAgArea <- 0
    comWatWW <- comWatWC <- avlWatWC
    comWatWW[, , ] <- 0
    comWatWC[, , ] <- 0
  }

  # Water available for potential additional irrigation
  # beyond committed ag. use considering (renewable and non-renewable)
  # availability (in mio. m^3)
  avlWatWW <- avlWatWW - comWatWW
  avlWatWC <- avlWatWC - comWatWC

  # Check
  if (any(round(avlWatWW, digits = 6) < 0) || any(round(avlWatWC, digits = 6) < 0)) {
    stop("In calcPotIrrigAreas: available water for additional irrigation beyond
         committed agricultural use became negative. This should not be the case.
         Please double check. A wild guess: this may be related to the non-renewable
         groundwater implementation.")
  }
  # Correct rounding imprecision
  avlWatWW[avlWatWW < 0] <- 0
  avlWatWC[avlWatWC < 0] <- 0

  # Irrigation water requirements for selected cropmix and irrigation system per cell (in mio. m^3)
  # required for irrigation of additional irrigation (beyond committed agriculture)
  watReq   <- calcOutput("FullIrrigationRequirement", selectyears = selectyears,
                         lpjml = lpjml, climatetype = climatetype, iniyear = iniyear,
                         irrigationsystem = irrigationsystem, landScen = landScen,
                         cropmix = cropmix, multicropping = multicropping,
                         comagyear = comagyear,
                         aggregate = FALSE)
  watReqWW <- watReqWC <- new.magpie(cells_and_regions = getItems(avlWatWW, dim = 1),
                                     years = getItems(avlWatWW, dim = 2),
                                     names = scenarios,
                                     fill = NA)

  watReqWW[, , ] <- collapseNames(watReq[, , "withdrawal"])
  watReqWC[, , ] <- collapseNames(watReq[, , "consumption"])

  # Read in area that can potentially be irrigated
  # (excluding already committed areas if comAg is activated)
  areaPotIrrig <- calcOutput("AreaPotIrrig",
                             selectyears = selectyears, iniyear = iniyear,
                             landScen = landScen, comagyear = comagyear,
                             aggregate = FALSE)

  # share of requirements that can be fulfilled given available water, when >1 whole area can be irrigated
  irrigareaWW <- pmin(avlWatWW / watReqWW, 1) * areaPotIrrig
  # cells with no water requirements also get no irrigated area assigned
  irrigareaWW[watReqWW == 0] <- 0
  irrigareaWW <- add_dimension(irrigareaWW, dim = 3.4, add = "type",
                               nm = "irrigatable_ww")

  irrigareaWC <- pmin(avlWatWC / watReqWC, 1) * areaPotIrrig
  # cells with no water requirements also get no irrigated area assigned
  irrigareaWC[watReqWC == 0] <- 0
  irrigareaWC <- add_dimension(irrigareaWC, dim = 3.4, add = "type",
                               nm = "irrigatable_wc")

  irrigatableArea <- pmin(collapseNames(irrigareaWW), collapseNames(irrigareaWC))
  irrigatableArea <- add_dimension(irrigatableArea, dim = 3.4, add = "type",
                                   nm = "irrigatable")

  # share of crop area by crop type given chosen cropmix
  cropareaShr <- calcOutput("CropAreaShare",
                            iniyear = iniyear, cropmix = cropmix,
                            aggregate = FALSE)
  # Exclude areas where no water is required for irrigation
  # from additionally irrigated areas as it is not required there
  # and rainfed production is assumed.
  cropIrrigReq <- calcOutput("ActualIrrigWatRequirements",
                             irrigationsystem = irrigationsystem,
                             selectyears = selectyears, iniyear = iniyear,
                             lpjml = lpjml, climatetype = climatetype,
                             multicropping = multicropping, aggregate = FALSE)
  # check
  if (any(cropIrrigReq[, , "consumption"] == 0 & cropIrrigReq[, , "withdrawal"] != 0)) {
    stop("Check what's wrong in irrigation water requirements of
          mrwater::calcPotIrrigAreas")
  }
  cropIrrigReq <- collapseNames(cropIrrigReq[, , "consumption"])
  # where no water is required for irrigation, no irrigation takes place
  cropareaShr[cropIrrigReq == 0] <- 0

  # crop-specific potentially irrigated area
  out <- collapseNames(irrigatableArea * cropareaShr)

  # Fix dimensions
  if (comAg) {
    comAgArea <- comAgArea[, , getItems(out, dim = 3)]
  }
  out <- out + comAgArea

  # Checks
  if (any(is.na(out))) {
    stop("mrwater::calcPotIrrigAreas produced NA irrigatable area")
  }
  if (any(round(out, digits = 6) < 0)) {
    stop("mrwater::calcPotIrrigAreas produced negative irrigatable area")
  }

  if (cropAggregation) {
    out         <- dimSums(out, dim = "crop")
    description <- paste0("Potentially irrigated area (total) ",
                          "given land and water constraints")
  } else {
    description <- paste0("Crop-specific area that can be irrigated ",
                          "given land and water constraints")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = description,
              isocountries = FALSE))
}
