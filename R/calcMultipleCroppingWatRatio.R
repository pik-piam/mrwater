#' @title       calcMultipleCroppingWatRatio
#' @description This function returns the ratio of off-season to main-season
#'              irrigation water requirements
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for grassland input
#' @param climatetype   Climate model or historical baseline "GSWP3-W5E5:historical"
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MultipleCroppingWatRatio", aggregate = FALSE)
#' }
#'
#' @importFrom mrcommons toolGetMappingCoord2Country
#' @importFrom magclass new.magpie getYears getNames
#' @importFrom stats runif

calcMultipleCroppingWatRatio <- function(selectyears, lpjml, climatetype) {

  # Read in LPJmL blue water consumption of grass in main-season and off-season
  #### monthly or yearly???
  # irrWat <- calcOutput("LPJmL_new", subtype = "cwater_b_monthly_grass", ##### ??????
  #                   version = lpjml["crop"], climatetype = climatetype, stage = "smoothed",
  #                   aggregate = FALSE, years = selectyears)
  #
  # out <- pmax(0, irrWat[,,"year"] / irrWat[,,"grper"] - 1)

  #### PLACEHOLDER UNTIL LPJML OUTPUTS ARE READY ####
  mapping <- toolGetMappingCoord2Country(pretty = TRUE)
  out     <- new.magpie(cells_and_regions = paste(mapping$coords, mapping$iso, sep = "."),
                        years = selectyears,
                        fill = NA)
  out[, , ]    <- runif(length(out), 0.7, 1.1)
  getSets(out) <- c("x", "y", "iso", "year", "data")
  #### PLACEHOLDER UNTIL LPJML OUTPUTS ARE READY ####

  # Checks
  if (any(is.na(out))) {
    stop("produced NA multiple cropping water requirement ratios")
  }

  return(list(
    x            = out,
    weight       = NULL,
    unit         = "1",
    description  = "ratio of off-season to main-season irrigation water requirements",
    isocountries = FALSE))
}
