#' @title       calcAreaPotIrrig
#' @description This function calculates area that can potentially be used for irrigation given assumptions defined in arguments
#'
#' @param selectyears      years to be returned
#' @param iniarea          if TRUE: already irrigated area is subtracted, if FALSE: total potential land area is used
#' @param iniyear          year of initialization for cropland area initialization
#' @param protect_scen     land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection
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
  ### increase avlLandSi according to min(LUH_cropland) (in calcAvlLandSi); an Stelle wo multipliziert si0>=croplandarea_LUH

  if (iniarea) {
    # subtract area already reserved for irrigation by committed agricultural uses [in mio. ha]
    crops_grown    <- calcOutput("IrrigatedArea", selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)
    crops_grown    <- collapseNames(dimSums(crops_grown, dim=3))
    land           <- land - crops_grown
  }
  # Total harvested areas retrieved by calcCroparea can be lower or higher than arable land because of multicropping or fallow land
  land[land<0] <- 0

  # No irrigation in protected areas (if protection scenario is activated) [in mio. ha]
  if (!is.null(protect_scen)) {
    tmp <- collapseNames(calcOutput("ProtectArea", aggregate=FALSE)[,,protect_scen])
    tmp <- addLocation(tmp)
    ### expand to 67k cells!
    protect <- new.magpie(cells_and_regions = getCells(land), years = getYears(tmp), names = getNames(tmp), fill = 0)
    protect[getCells(tmp),,] <- tmp
    land    <- land - protect
    # gesamtfläche der Zelle (landarea!!); davon protected area; wenn weniger cropland übrig als genutzt: reduziert
  }

  # correct negative land availability due to mismatch of available land and protected land
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
