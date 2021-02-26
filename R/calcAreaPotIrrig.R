#' @title       calcAreaPotIrrig
#' @description This function calculates area that can potentially be used for irrigation given assumptions defined in arguments
#'
#' @param selectyears   years to be returned
#' @param iniarea       if TRUE: already irrigated area is subtracted, if FALSE: total potential land area is used
#' @param iniyear       year of initialization for cropland area initialization
#' @param protect_scen  land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("AreaPotIrrig", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getCells getYears getNames

calcAreaPotIrrig <- function(selectyears, iniyear, iniarea, protect_scen) {

  # read in suitable land [in mio. ha]
  land <- collapseNames(calcOutput("AvlLandSi", aggregate=FALSE, cells="lpjcell")[,,"si0"])

  # No irrigation in protected areas (if protection scenario is activated) [in mio. ha]
  if (!is.null(protect_scen)) {
    tmp <- collapseNames(calcOutput("ProtectArea", aggregate=FALSE)[,,protect_scen])
    tmp <- addLocation(tmp)
    #### expand to 67k cells (temporairly until read/calcProtectArea is adjusted) ####
    protect_area                  <- new.magpie(cells_and_regions = getCells(land), years = getYears(tmp), names = getNames(tmp), fill = 0)
    protect_area[getCells(tmp),,] <- tmp
    #### expand to 67k cells (temporairly until read/calcProtectArea is adjusted) ####

    # total land area
    landarea     <- calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="lpjcell", irrigation=FALSE, years="y1995")
    landarea     <- dimSums(landarea, dim=3)
    # area available for cropland
    avl_cropland <- landarea - protect_area
    # land suitable for (sustainable) (irrigated) agriculture
    land         <- pmin(avl_cropland, land)
  }

  # Areas that are already irrigated (by committed agricultural uses)
  if (iniarea) {
    # subtract area already reserved for irrigation by committed agricultural uses [in mio. ha] (to avoid double accounting)
    irrig_area_com  <- calcOutput("IrrigatedArea", selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)
    irrig_area_com  <- collapseNames(dimSums(irrig_area_com, dim=3))
    land            <- land - irrig_area_com
  }

  # correct negative land availability due to mismatch of available land and protected land or rounding imprecision
  land[land<0] <- 0

  # Checks
  if (any(is.na(land))) {
    stop("produced NA full irrigation requirements")
  }

  return(list(
    x=land,
    weight=NULL,
    unit="Mha",
    description="land potentially available for irrigated agriculture",
    isocountries=FALSE))
}
