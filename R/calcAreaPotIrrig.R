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
#' @importFrom magclass collapseNames

calcAreaPotIrrig <- function(selectyears, iniyear, iniarea, protect_scen) {

  # read in suitable land [in mio. ha]
  land <- collapseNames(calcOutput("AvlLandSi", aggregate=FALSE, cells="lpjcell")[,,"si0"])

  if (iniarea) {
    # subtract area already reserved for irrigation by committed agricultural uses [in mio. ha]
    crops_grown    <- calcOutput("IrrigatedArea", selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)
    crops_grown    <- collapseNames(dimSums(crops_grown, dim=3))
    land           <- land - crops_grown
  }
  # negative values may occur because AvlLandSi is based on Ramankutty data and Croparea based on LUH -> set to 0
  land[land<0] <- 0 ####should not be the case anymore!!!!!

  # No irrigation in protected areas (if protection scenario is activated) [in mio. ha]
  if (!is.null(protect_scen)) {
    protect <- collapseNames(calcOutput("ProtectArea", aggregate=FALSE)[,,protect_scen])
    land    <- land - protect
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
