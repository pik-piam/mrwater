#' @title       calcNonrenGroundwatUse
#' @description Calculates current non-renewable (fossil) groundwater use
#'              as negative difference between
#'              water availability from renewable sources and
#'              human water use (consumption and withdrawal).
#'              The water use in the initialization year is fixed in the future as
#'              non-renewable groundwater.
#'
#' @param output        "total": total groundwater use (non-agricultural and agricultural),
#'                      "nonAg": groundwater use in non-agricultural sector (industry, domestic, electricity),
#'                      "comAg": groundwater use in agricultural sector (currently irrigated area)
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned
#' @param iniyear       Initialization year
#' @param transDistGW   Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand by surrounding cell water availability
#'                      to determine missing water that is tagged as non-renewable groundwater
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
#' @return cellular magpie object
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("NonrenGroundwatUse", aggregate = FALSE)
#' }
#'
#' @importFrom magclass collapseNames dimSums
#' @importFrom magpiesets findset
#' @importFrom stringr str_split
#'
#' @export

calcNonrenGroundwatUse <- function(output, lpjml, climatetype,
                                   transDistGW = 100, multicropping = "TRUE:actual:irrig_crop",
                                   selectyears, iniyear) {

  # Non-renewable groundwater use is determined based on unfulfilled water demands of the past
  missingWat <- calcOutput("MissingWater", output = output,
                           lpjml = lpjml, climatetype = climatetype,
                           transDistGW = transDistGW, multicropping = multicropping,
                           selectyears = selectyears, iniyear = iniyear,
                           aggregate = FALSE)

  # Groundwater is calculated based on full water use of the past given all available water (EFP off).
  # In the past, the non-agriculture scenarios are identical. Therefore, ISIMIP can be used as representative case.
  scenNonAg  <- "ISIMIP"
  scenEFP    <- "off"
  missingWat <- collapseNames(missingWat[, , paste(scenEFP, scenNonAg, sep = ".")])

  # Missing water of past time steps is attributed to non-renewable groundwater.
  # For the future, it is fixed to the missing water of the last time step.
  lastYr <- as.numeric(gsub("y", "", tail(magpiesets::findset("past"), n = 1)))

  if (is.character(selectyears)) {
    selectyears <- as.numeric(gsub("y", "", selectyears))
  }
  if (is.character(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }
  if (iniyear > selectyears[1]) {
    stop(paste0("Please align the initialization year ('iniyear') ",
                "with the selected years ('selectyears'). ",
                "The 'iniyear' has to be the first year of 'selectyears' ",
                "or lie in the past of it."))
  }
  if (iniyear > lastYr) {
    stop(paste0("Please align the initialization year ('iniyear') ",
                "with the last year in the past (see magpiesets::findset('past')). ",
                "The 'iniyear' has to be smaller or equal to the last year."))
  }
  # include iniyear in selectyears
  if (length(intersect(iniyear, selectyears)) < 1) {
    cmbYrs <- c(iniyear, selectyears)
  } else {
    cmbYrs <- selectyears
  }
  # selectyears of the past (including iniyear)
  pstYrs <- cmbYrs[cmbYrs <= lastYr]
  # selectyears of the future
  ftrYrs <- cmbYrs[cmbYrs > lastYr]

  ######################
  ### Prepare output ###
  ######################
  out <- new.magpie(fill = NA,
                    cells_and_regions = getItems(missingWat, dim = 1),
                    years = selectyears,
                    names = c("withdrawal", "consumption"))
  getSets(out) <- c("x", "y", "iso", "year", "wtype")

  # In past time steps, missing water is accounted as fossil groundwater
  out[, pstYrs, ] <- missingWat[, pstYrs, ]
  # In future time steps, fossil groundwater is held constant
  out[, ftrYrs, ] <- missingWat[, lastYr, ]

  ##############
  ### Checks ###
  ##############
  if (any(round(out, digits = 4) < 0, na.rm = TRUE)) {
    stop("calcNonrenGroundwatUse produced negative values")
  }
  if (any(is.na(out))) {
    stop("calcNonrenGroundwatUse produced NAs")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("usable non-renewable groundwater volume ",
                                    "for human consumption and withdrawal"),
              isocountries = FALSE))
}
