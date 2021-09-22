#' @title       calcYieldsAdjusted
#' @description This function returns irrigated and rainfed yields for magpie crops.
#'              It can either return LPJmL potential yields of different stages or
#'              LPJmL yields calibrated to FAO country yields
#'
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical" for yields
#' @param selectyears   Years to be returned by the function
#' @param iniyear       Year to be used for cropland of yield calibration
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                      If FALSE: uncalibrated LPJmL yields are used
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("YieldsAdjusted", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput toolAggregate
#' @importFrom magclass collapseNames getNames getCells getSets dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcYieldsAdjusted <- function(lpjml, climatetype, iniyear, selectyears, yieldcalib) {

  if (yieldcalib) {

    # read in cellular LPJmL yields calibrated to FAO country values of iniyear [in tDM/ha]
    yields <- calcOutput("YieldsCalibrated", source = c(lpjml = lpjml[["crop"]], isimip = NULL),
                         climatetype = climatetype, refYear = iniyear,
                         cells = "lpjcell", aggregate = FALSE)[, selectyears, ]

    description <- "LPJmL yields calibrated to FAO yield levels for all different (MAgPIE) crop types"

  } else {

    # read in cellular LPJmL yields [in tDM/ha]
    yields <- setYears(calcOutput("Yields", source = c(lpjml = lpjml[["crop"]], isimip = NULL),
                                  cells = "lpjcell", climatetype = climatetype, years = selectyears, aggregate = FALSE),
                       selectyears)

    description <- "LPJmL yields for all different (MAgPIE) crop types"

  }

  # only crops (pasture is not irrigated)
  yields     <- yields[, , "pasture", invert = T]
  # set correct dimension names
  getSets(yields)[c("d3.1", "d3.2")] <- c("MAG", "irrigation")

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
