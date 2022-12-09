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
#'                      ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                      separated by ":"
#'                      (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
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

calcIrrigAreaActuallyCommitted <- function(lpjml, climatetype, selectyears, iniyear,
                                           efrMethod,
                                           multicropping, transDist) {

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
                             multicropping = multicropping, aggregate = FALSE)

  # Water already committed to irrigation (in mio. m^3)
  comWater <- calcOutput("RiverHumanUseAccounting",
                          iteration = "committed_agriculture",
                          lpjml = lpjml, climatetype = climatetype,
                          transDist = transDist, comAg = NULL,
                          efrMethod = efrMethod, multicropping = multicropping,
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
              unit         = "mio. ha",
              description  = "Cropland area reserved for irrigation per crop",
              isocountries = FALSE))
}
