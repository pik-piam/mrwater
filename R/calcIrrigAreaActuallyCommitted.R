#' @title       calcIrrigAreaActuallyCommitted
#' @description calculates area reserved for irrigation based on area irrigated
#'              in initialization and available water resources
#'
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios
#'                      or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned
#'                      (Note: does not affect years of harmonization or smoothing)
#' @param iniyear       Initialization year of irrigation system
#' @param efrMethod     EFR method used including selected strictness of EFRs
#'                      (Smakhtin:good, VMF:fair)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLanduseToolbox
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#' @param fossilGW       If TRUE: non-renewable groundwater can be used.
#'                       If FALSE: non-renewable groundwater cannot be used.
#' @param iteration      Default: "committed_agriculture",
#'                       Special case: "committed_agriculture_fullPotential".
#'                       Special case should only be used for calculation of
#'                       full multicropping potential committed agricultural area
#'                       for case of Current Irrigation.
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigAreaActuallyCommitted", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames collapseDim new.magpie getCells getNames

calcIrrigAreaActuallyCommitted <- function(iteration = "committed_agriculture",
                                           lpjml, climatetype,
                                           selectyears, iniyear,
                                           efrMethod, fossilGW,
                                           multicropping, transDist) {

  ## Current Uses
  if (as.logical(stringr::str_split(multicropping, ":")[[1]][1])) {
    if (grepl(pattern = "fullPotential", x = iteration)) {
      m <- multicropping
    } else {
      m <- "TRUE:actual:irrig_crop"
    }
  } else {
    m <- FALSE
  }

  ######################
  ### Read in Inputs ###
  ######################
  # Read in cropland area (by crop) from crop area initialization (in Mha)
  comArea <- calcOutput("IrrigAreaCommitted",
                        selectyears = selectyears, iniyear = iniyear,
                        aggregate = FALSE)

  # Irrigation water requirements per cell per crop
  # given irrigation system (in m^3 per hectare per year)
  cropIrrigReq <- calcOutput("ActualIrrigWatRequirements",
                             irrigationsystem = "initialization",
                             selectyears = selectyears, iniyear = iniyear,
                             lpjml = lpjml, climatetype = climatetype,
                             multicropping = m, aggregate = FALSE)

  # Water already committed to irrigation (in mio. m^3)
  comWater <- calcOutput("RiverHumanUseAccounting",
                          iteration = iteration, fossilGW = NULL,
                          lpjml = lpjml, climatetype = climatetype,
                          transDist = transDist, comAg = NULL,
                          efrMethod = efrMethod, multicropping = m,
                          selectyears = selectyears, iniyear = iniyear,
                          accessibilityrule = NULL,
                          rankmethod = NULL, gainthreshold = NULL,
                          cropmix = NULL, yieldcalib = NULL,
                          irrigationsystem = NULL, landScen = NULL,
                          aggregate = FALSE)
  comWatWW <- collapseNames(comWater[, , "currHumanWWtotal"])
  comWatWC <- collapseNames(comWater[, , "currHumanWCtotal"])

  ####################
  ### Calculations ###
  ####################
  # Total irrigation requirements per cell (in mio. m^3)
  totalIrrigReqWW <- collapseNames(dimSums(cropIrrigReq[, , "withdrawal"] * comArea,
                                  dim = "crop"))
  totalIrrigReqWC <- collapseNames(dimSums(cropIrrigReq[, , "consumption"] * comArea,
                                  dim = "crop"))

  # Share of Area that is irrigated given limited renewable water availability
  # Note: Areas that are reported to be irrigated
  wwShr <- ifelse(totalIrrigReqWW > 0,
                    comWatWW / totalIrrigReqWW,
                  1)
  wcShr <- ifelse(totalIrrigReqWC > 0,
                    comWatWC / totalIrrigReqWC,
                  1)

  ### Checks ###
  if (any(round(wwShr - wcShr, digits = 4) != 0)) {
    stop("There seems to be a mismatch in consumption and withdrawal
         in calcIrrigAreaActuallyCommitted.
         Please make sure that the fulfilled ratio is the same
         for consumption and withdrawal")
  }
  if (any(round(wcShr, digits = 4) > 1)) {
    stop("In calcAreaActuallyCommitted: Water requirements are over-fulfilled.
          This should not be the case in the initialization year. Please double-check!
          A wild guess: it might be related to the fossil groundwater calculation.")
  }

  # Area Actually Committed for Irrigation given available renewable water resource (in Mha)
  out <- comArea * collapseNames(wcShr)

  # Fossil groundwater use to fulfill committed agricultural water use
  if (fossilGW) {
    gw <- calcOutput("NonrenGroundwatUse",
                      output = "comAg", multicropping = m,
                      lpjml = lpjml, climatetype = climatetype,
                      selectyears = selectyears, iniyear = iniyear,
                      aggregate = FALSE)
    comWatWW <- comWatWW + collapseNames(gw[, , "withdrawal"])
    comWatWC <- comWatWC + collapseNames(gw[, , "consumption"])

    # Share of Area that is irrigated given limited water availability
    # under consideration of fossil GW
    wwShr <- collapseNames(ifelse(totalIrrigReqWW > 0,
                                    comWatWW / totalIrrigReqWW,
                                  1))
    wcShr <- collapseNames(ifelse(totalIrrigReqWC > 0,
                                    comWatWC / totalIrrigReqWC,
                                  1))

    # In future time steps (after initialization year) [because of depreciation of irrigated areas]
    # and under an environmental flow policy scenario,
    # and in the single-cropping scenario [because default is current multiple cropping],
    # and for lower chosen transport distance than for determination of non-renewable groundwater,
    # the irrigated area may be over-fulfilled by non-renewable groundwater.
    # These values are capped to 1:
    wcShr[wcShr > 1] <- 1
    wwShr[wwShr > 1] <- 1

    ### This could introduce a mismatch between wcShr and wwShr... where wcShr > 1 -> wwShr also needs to be > 1, right?

    if (any(round(wwShr[, iniyear, "off"] - wcShr[, iniyear, "off"], digits = 4) != 0)) {
      stop("There seems to be a mismatch in consumption and withdrawal
          in calcIrrigAreaActuallyCommitted.
          Please make sure that the fulfilled ratio is the same
          for consumption and withdrawal")
    }

    # Area Actually Committed for Irrigation given available water (in Mha)
    out[, , "off"] <- comArea * wcShr[, , "off"]
    # Note: In an environmental flow scenario, non-renewable groundwater cannot be used

    #### @JENS: Or should it be added to both EFR.on and off?
    # --> Yes; for both "on" and "off" should be included

  }

  # check for NAs and negative values
  if (any(is.na(out))) {
    stop("calcIrrigAreaActuallyCommitted produced NA irrigated areas")
  }
  if (any(round(out, digits = 6) < 0)) {
    stop("calcIrrigAreaActuallyCommitted produced negative irrigated areas")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Cropland area reserved for irrigation per crop",
              isocountries = FALSE))
}
