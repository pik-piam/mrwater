#' @title       calcIrrigAreaCommitted
#' @description calculates area reserved for irrigation based on area irrigated
#'              in initialization year and depreciation parameter (set to 0.1)
#'
#' @param iniyear      initialization year
#' @param selectyears  select years
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigAreaCommitted", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames collapseDim new.magpie getCells getNames
#' @importFrom utils tail

calcIrrigAreaCommitted <- function(selectyears, iniyear) {

  # Set depreciation parameter
  depreciation <- 0.1

  # Read in data: crop- and water supply type specific croparea
  # (in Mha) in initialization year:
  tmp <- collapseNames(calcOutput("CropareaAdjusted", iniyear = iniyear,
                                   aggregate = FALSE)[, , "irrigated"])

  # Prepare year arguments for further usage
  if (is.character(selectyears)) {
    selectyears <- as.numeric(gsub("y", "", selectyears))
  }
  if (is.character(iniyear)) {
    iniyear     <- as.numeric(gsub("y", "", iniyear))
  }
  if (iniyear > selectyears[1]) {
    stop(paste0("Please align the initialization year ('iniyear') ",
                 "with the selected years ('selectyears'). ",
                 "The 'iniyear' has to be the first year of 'selectyears' ",
                 "or lie in the past of it."))
  }

  # Empty object to be filled with area reserved for irrigation in current and future time steps
  irrigArea <- new.magpie(cells_and_regions = getCells(tmp),
                          years = seq(iniyear, tail(selectyears, 1), by = 1),
                          names = getNames(tmp),
                          sets = c("x.y.iso", "year", "crop"))

  # Each year certain share (parameter: "depreciation") of irrigated cropland is lost
  # Note: Depreciation in yearly time-steps!
  for (y in (iniyear:tail(selectyears, 1))) {
    # irrigated area in respective year
    irrigArea[, y, ] <- tmp
    # depreciation of irrigated area
    tmp              <- tmp * (1 - depreciation)
  }

  # select years to be returned
  irrigArea <- irrigArea[, selectyears, ]

  # check for NAs and negative values
  if (any(is.na(irrigArea))) {
    stop("produced NA irrigation water requirements")
  }
  if (any(irrigArea < 0)) {
    stop("produced negative irrigation water requirements")
  }

  return(list(x            = irrigArea,
              weight       = NULL,
              unit         = "Mha",
              description  = "Cropland area reserved for irrigation per crop",
              isocountries = FALSE))
}
