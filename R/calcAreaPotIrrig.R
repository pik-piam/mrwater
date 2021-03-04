#' @title       calcAreaPotIrrig
#' @description This function calculates area that can potentially be used for irrigation given assumptions defined in arguments
#'
#' @param selectyears   years to be returned
#' @param iniareayear   if !NULL: already irrigated area is subtracted; if NULL: total potential land area is used; year specified here is the year of the initialization used for cropland area initialization in calcIrrigatedArea
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

calcAreaPotIrrig <- function(selectyears, iniareayear, protect_scen) {

  # read in suitable land [in mio. ha]
  land <- collapseNames(calcOutput("AvlLandSi", aggregate=FALSE, cells="lpjcell")[,,"si0"])

  # No irrigation in protected areas (if protection scenario is activated) [in mio. ha]
  if (!is.null(protect_scen)) {
    tmp <- collapseNames(calcOutput("ProtectArea", aggregate=FALSE)[,,protect_scen])
    tmp <- collapseDim(addLocation(tmp), dim=c("region","cell"))
    #### expand to 67k cells (temporairly until read/calcProtectArea is adjusted) ####
    protect_area                  <- new.magpie(cells_and_regions = getCells(land), years = getYears(tmp), names = getNames(tmp), fill = 0)
    protect_area[getCells(tmp),,] <- tmp
    #### expand to 67k cells (temporairly until read/calcProtectArea is adjusted) ####

    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    map                     <- toolGetMappingCoord2Country()
    getCells(tmp)           <- paste(map$coords, map$iso, sep=".")
    names(dimnames(tmp))[1] <- "x.y.iso"
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

    # total land area
    #### INCLUDE READ SOURCE HERE INSTEAD!! (SEE MRCOMMONS)
    landarea <- calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="lpjcell", irrigation=FALSE, years="y1995")
    landarea <- dimSums(landarea, dim=3)
    landarea <- collapseDim(addLocation(landarea), dim=c("N","cell"))
    # area available for cropland
    avl_cropland <- landarea - protect_area
    # land suitable for (sustainable) (irrigated) agriculture
    land         <- pmin(avl_cropland, land)
  }

  # Areas that are already irrigated (by committed agricultural uses)
  if (!is.null(iniareayear)) {
    # subtract area already reserved for irrigation by committed agricultural uses [in mio. ha] (to avoid double accounting)
    irrig_area_com  <- calcOutput("IrrigAreaCommitted", selectyears=selectyears, iniyear=iniareayear, aggregate=FALSE)
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
