#' @title       calcActualIrrigWatRequirements
#' @description This function calculates actual irrigation water requirements
#'              per cell given a certain irrigation system
#'
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop
#' @param selectyears Years to be returned
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear     Initialization year (for weight by cropland)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @seealso
#' \code{\link{calcIrrigationSystem}}, \code{\link{calcIrrigWatRequirements}}
#'
#' @examples
#' \dontrun{
#' calcOutput("ActualIrrigWatRequirements", aggregate = FALSE)
#' }
#'
#' @importFrom magclass dimSums collapseNames getCells getSets
#' @importFrom madrat calcOutput
#' @importFrom mrcommons toolGetMappingCoord2Country

calcActualIrrigWatRequirements <- function(lpjml, selectyears, climatetype, iniyear) {

  # irrigation water requirement per crop per system (in m^3 per ha per yr)
  irrigReq   <- calcOutput("IrrigWatRequirements", aggregate = FALSE,
                            lpjml = lpjml, selectyears = selectyears, climatetype = climatetype)
  irrigReq   <- irrigReq[, , "pasture", invert = TRUE]

  # irrigation system share (share of irrigated area)
  irrigSystemShr    <- calcOutput("IrrigationSystem", source = "Jaegermeyr", aggregate = FALSE)

  # total irrigation water requirements per crop given irrigation system share (in m^3 per ha per yr)
  irrigReq          <- dimSums(irrigSystemShr * irrigReq, dim = "system")
  getSets(irrigReq) <- c("x", "y", "iso", "year", "crop", "type")

  # Check for NAs and negative values
  if (any(is.na(irrigReq))) {
    stop("produced NA irrigation water requirements")
  }
  if (any(irrigReq < 0)) {
    stop("produced negative irrigation water requirements")
  }

  # irrigated cropland area as weight
  # Note physical=FALSE: harvested area (multi-cropping allowed)
  irrigArea <- calcOutput("CropareaAdjusted", years = iniyear, sectoral = "kcr",
                           cells = "lpjcell", physical = TRUE, cellular = TRUE,
                           irrigation = TRUE, aggregate = FALSE)
  # Small additive term to account for places with 0 irrigated area
  irrigArea <- collapseNames(irrigArea[, , "irrigated"]) + 1e-9

  return(list(x            = irrigReq,
              weight       = irrigArea,
              unit         = "m^3 per ha per yr",
              description  = "Irrigation water requirements for irrigation for
                             different crop types under selected irrigation system share per cell",
              isocountries = FALSE))
}
