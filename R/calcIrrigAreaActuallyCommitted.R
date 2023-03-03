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
#' @importFrom utils tail

calcIrrigAreaActuallyCommitted <- function(iteration = "committed_agriculture",
                                           lpjml, climatetype, selectyears, iniyear,
                                           efrMethod,
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
  comArea <- calcOutput("IrrigAreaCommitted", selectyears = selectyears,
                        iniyear = iniyear, aggregate = FALSE)

  # Irrigation water requirements per cell per crop
  # given irrigation system (in m^3 per hectare per year)
  cropIrrigReq <- calcOutput("ActualIrrigWatRequirements",
                             irrigationsystem = "initialization",
                             selectyears = selectyears, iniyear = iniyear,
                             lpjml = lpjml, climatetype = climatetype,
                             multicropping = m, aggregate = FALSE)

  # Water already committed to irrigation (in mio. m^3)
  comWater <- calcOutput("RiverHumanUseAccounting",
                          iteration = iteration,
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
  totalIrrigReq <- dimSums(cropIrrigReq * comArea, dim = "crop")

  # Share of Area that is irrigated given limited water availability
  wwShr <- ifelse(totalIrrigReq[, , "withdrawal"] > 0,
                    comWatWW / collapseNames(totalIrrigReq[, , "withdrawal"]),
                  0)
  wcShr <- ifelse(totalIrrigReq[, , "consumption"] > 0,
                    comWatWC / collapseNames(totalIrrigReq[, , "consumption"]),
                  0)

  if (any(round(wwShr - wcShr, digits = 4) != 0)) {
    stop("There seems to be a mismatch in consumption and withdrawal
         in calcIrrigAreaActuallyCommitted.
         Please make sure that the fulfilled ratio is the same
         for consumption and withdrawal")
  }

  # Area Actually Committed for Irrigation given available water (in Mha)
  out <- comArea * wcShr

  # check for NAs and negative values
  if (any(is.na(out))) {
    stop("produced NA irrigation water requirements")
  }
  if (any(out < 0)) {
    stop("produced negative irrigation water requirements")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Cropland area reserved for irrigation per crop",
              isocountries = FALSE))
}
