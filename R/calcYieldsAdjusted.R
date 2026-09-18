#' @title       calcYieldsAdjusted
#' @description This function returns irrigated and rainfed yields for MAgPIE crops
#'              in the format required by mrwater.
#'
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical" for yields
#' @param selectyears   Years to be returned by the function
#' @param iniyear       Year to be used for cropland of yield calibration
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                      If FALSE: uncalibrated LPJmL yields are used
#' @param calibDetails  Determines FAO yield calibration with two arguments separated by ":":
#'                      aggregation: country, continent, GLO
#'                      average: in number of years or NULL
#'                      (e.g. country:5)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from LandInG
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("YieldsAdjusted", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass getSets add_dimension

calcYieldsAdjusted <- function(lpjml, climatetype,
                               iniyear, selectyears,
                               yieldcalib, calibDetails = "country:5",
                               multicropping) {

  # Extraction of yield calibration arguments
  if (!is.logical(yieldcalib)) {
    refYields  <- strsplit(yieldcalib, split = ":")[[1]][-1]
    # boolean for calibration or not
    yieldcalib <- as.logical(strsplit(yieldcalib, split = ":")[[1]][1])
    # calibration details for FAO yields
    aggregation <- strsplit(calibDetails, split = ":")[[1]][1]
    if (strsplit(calibDetails, split = ":")[[1]][2] == "NULL") {
      average <- NULL
    } else {
      average <- as.numeric(strsplit(calibDetails, split = ":")[[1]][2])
    }
  }

  if (yieldcalib) {

    # reference yield to be calibrated to
    tmp <- refYields[1]

    if (tmp == "FALSE") {
      # single-cropping case (standard calibration)
      refYields <- as.logical(refYields)

    } else if (tmp == "TRUE") {
      # multiple cropping case
      if (length(refYields) > 1) {

        for (i in 2:length(refYields)) {
          tmp <- paste(tmp, refYields[i], sep = ":")
        }
        refYields <- tmp

      } else {
        stop("Please specify which reference yield to calibrate the yields to
              in the case of yield calibration in mrwater::calcYieldsAdjusted")
      }
    }

    # read in cellular LPJmL yields calibrated to FAO country values of iniyear [in tDM/ha]
    yields <- calcOutput("YieldsMAgPIEcrops",
                         lpjml = lpjml,
                         climatetype = climatetype,
                         selectyears = selectyears,
                         multicropping = multicropping,
                         calibration = list(refYear = iniyear,
                                            refYields = refYields,
                                            areaSource = "LandInG",
                                            average = average,
                                            aggregation = aggregation),
                         aggregate = FALSE)

    description <- "LPJmL yields calibrated to FAO yield levels for all different (MAgPIE) crop types"

  } else {

    # read in cellular LPJmL yields [in tDM/ha]
    yields <- calcOutput("YieldsMAgPIEcrops",
                         lpjml = lpjml,
                         climatetype = climatetype,
                         selectyears = selectyears,
                         multicropping = multicropping,
                         calibration = NULL,
                         aggregate = FALSE)

    description <- "LPJmL yields for all different (MAgPIE) crop types"

  }

  # only crops (pasture is not irrigated)
  yields <- yields[, , "pasture", invert = TRUE]

  # order third dimension as common in mrwater
  yields <- dimOrder(yields, perm = c(2, 1), dim = 3)

  # Check for NAs
  if (any(is.na(yields))) {
    stop("Function calcYieldsAdjusted produced NAs")
  }

  return(list(x            = yields,
              weight       = NULL,
              unit         = "tDM per ha",
              description  = description,
              isocountries = FALSE))
}
