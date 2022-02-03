#' @title       calcMultipleCroppingYieldRatio
#' @description This function returns the ratio of off-season to main-season yields
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for grassland input
#' @param climatetype   Climate model or historical baseline "GSWP3-W5E5:historical"
#'
#' @examples
#' \dontrun{
#' calcOutput("MultipleCroppingYieldRatio", aggregate = FALSE)
#' }
#'
#' @importFrom mrcommons toolGetMappingCoord2Country
#' @importFrom magclass new.magpie getYears getNames

calcMultipleCroppingYieldRatio <- function(selectyears, lpjml, climatetype) {

  # Read in LPJmL grass NPP (net primary productivity in gC/m2/time)
  #### monthly or yearly???
  # npp <- calcOutput("LPJmL_new", subtype = "npp",
  #                   version = lpjml["crop"], climatetype = climatetype, stage = "smoothed",
  #                   aggregate = FALSE, years = selectyears)
  #
  # out <- pmax(0, npp[,,"year"] / npp[,,"grper"] - 1)

  #### PLACEHOLDER UNTIL LPJML OUTPUTS ARE READY ####
  mapping <- toolGetMappingCoord2Country(pretty = TRUE)
  out     <- new.magpie(cells_and_regions = paste(mapping$coords, mapping$iso, sep = "."),
                        years = selectyears,
                        fill = NA)
  out[, , ]    <- 0.8
  out          <- add_dimension(out, dim = 3.1, add = "irrigation",
                                nm = c("rainfed", "irrigated"))
  getSets(out) <- c("x", "y", "iso", "year", "irrigation")
  #### PLACEHOLDER UNTIL LPJML OUTPUTS ARE READY ####

  # Checks
  if (any(is.na(out))) {
    stop("produced NA multiple cropping yield ratios")
  }

  return(list(
    x            = out,
    weight       = NULL,
    unit         = "1",
    description  = "yield ratio of off-season to main-season yields",
    isocountries = FALSE))
}
