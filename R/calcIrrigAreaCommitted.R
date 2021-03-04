#' @title       calcIrrigAreaCommitted
#' @description calculates area reserved for irrigation based on area irrigated in initialization year and depreciation parameter
#'
#' @param iniyear      initialization year
#' @param selectyears  select years
#' @param depreciation parameter defining yearly depreciation rate at which previously cropland committed to irrigation becomes "unreserved" for irrigation (default: 0.1)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigAreaCommitted", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames collapseDim new.magpie getCells getNames
#' @importFrom magpiesets addLocation
#' @importFrom mrcommons toolGetMappingCoord2Country

calcIrrigAreaCommitted <- function(selectyears, iniyear, depreciation=0.1) {

  # Read in data: crop- and water supply type specific crop area (in Mha) in initialization year:
  tmp <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  map                     <- toolGetMappingCoord2Country()
  getCells(tmp)           <- paste(map$coords, map$iso, sep=".")
  names(dimnames(tmp))[1] <- "x.y.iso"
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

  # Retrieve irrigated area (per crop)
  tmp <- collapseNames(tmp[,,"irrigated"])

  # Empty object to be filled with area reserved for irrigation in current and future time steps
  irrig_area <- new.magpie(getCells(tmp), selectyears, getNames(tmp))
  names(dimnames(irrig_area))[1] <- "x.y.iso"

  # Each year certain share (parameter: "depreciation") of irrigated cropland is lost
  for (y in (1:length(selectyears))) {
    # irrigated area in respective year
    irrig_area[,selectyears[y],] <- tmp
    # adjust yearly depreciation rate to time steps
    timegap <- selectyears[y+1] - selectyears[y]
    dep_adj <- (1 - depreciation)^timegap
    # depreciation of irrigated area
    tmp     <- tmp * dep_adj
  }

  # check for NAs and negative values
  if (any(is.na(irrig_area))) {
    stop("produced NA irrigation water requirements")
  }
  if (any(irrig_area<0)) {
    stop("produced negative irrigation water requirements")
  }

  return(list(
    x=irrig_area,
    weight=NULL,
    unit="mio. ha",
    description="Cropland area reserved for irrigation per crop",
    isocountries=FALSE))
}
