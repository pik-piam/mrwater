#' @title       calcIrrigatedArea
#' @description calculates area reserved for irrigation based on area irrigated in initialization year and depreciation parameter
#'
#' @param iniyear      initialization year
#' @param selectyears  select years
#' @param depreciation parameter defining yearly depreciation rate at which previously irrigated cropland becomes "unreserved" for irrigation
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigatedArea", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames collapseDim new.magpie getCells getNames
#' @importFrom magpiesets addLocation

calcIrrigatedArea <- function(selectyears=seq(1995,2100,by=5), iniyear=1995, depreciation=0.1){

  # Read in data: crop- and water supply type specific crop area (in Mha) in initialization year:
  tmp <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)
  # Retrieve irrigated area (per crop)
  tmp <- collapseNames(tmp[,,"irrigated"])

  # Total harvested areas retrieved by calcCroparea can be lower or higher than arable land because of multicropping or fallow land
  # Correct irrigated area to match total arable land???????

  # Empty object to be filled with area reserved for irrigation in current and future time steps
  irrig_area <- new.magpie(getCells(tmp), selectyears, getNames(tmp))

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

  # Dimension and element names
  irrig_area <- addLocation(irrig_area)
  irrig_area <- collapseDim(irrig_area, dim=c("N", "region1"))
  irrig_area <- collapseDim(irrig_area, dim="iso")

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
