#' @title       calcAreaPotIrrig
#' @description This function calculates area that can potentially be used for irrigation given assumptions defined in arguments
#'
#' @param selectyears   years to be returned
#' @param iniyear       initialization year for cropland area. Note: only applies for curr_irrig and curr_cropland scenarios of avlland_scen
#' @param comagyear     if NULL: total potential croparea is used; if !NULL: already irrigated area is subtracted; year specified here is the year of the initialization used for cropland area initialization in calcIrrigatedArea
#' @param avlland_scen  land availability scenario: curr_irrig (only currently irrigated cropland available for irrigated agriculture), curr_cropland (only current cropland areas available for irrigated agriculture), pot_irrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#' @param protect_scen  land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection. Note: Only active when avlland_scen is chosen
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("AreaPotIrrig", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getCells getYears getNames dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcAreaPotIrrig <- function(selectyears, iniyear, comagyear, avlland_scen, protect_scen) {

  if (avlland_scen == "pot_irrig") {
    # read in suitable land [in mio. ha]
    land <- collapseNames(calcOutput("AvlLandSi", aggregate=FALSE, cells="lpjcell")[,,"si0"])

    # No irrigation in protected areas (if protection scenario is activated) [in mio. ha]
    if (!is.null(protect_scen)) {
      # read in protected area of selected scenario
      tmp <- collapseNames(calcOutput("ProtectArea", aggregate=FALSE)[,,protect_scen])
      #### expand to 67k cells (temporarily until read/calcProtectArea is adjusted) ####
      tmp <- collapseDim(addLocation(tmp), dim=c("region","cell"))
      protect_area                  <- new.magpie(cells_and_regions=getCells(collapseDim(land,dim="iso")), years=getYears(tmp), names=getNames(tmp), fill=0, sets=c("x.y.iso", "t", "data"))
      protect_area[getCells(tmp),,] <- tmp
      #### expand to 67k cells (temporarily until read/calcProtectArea is adjusted) ####

      # total land area (Note: constant over the years.)
      landarea <- setYears(collapseNames(dimSums(readSource("LUH2v2", subtype="states", convert="onlycorrect")[,"y1995",], dim=3)), NULL)
      landarea <- dimSums(landarea, dim=3)

      # area available for cropland
      avl_cropland <- landarea - protect_area
      # land suitable for (sustainable) (irrigated) agriculture
      land         <- pmin(avl_cropland, land)
    }

  } else if (avlland_scen == "curr_cropland") {
    # Total current cropland per cell:
    land <- dimSums(calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=FALSE, aggregate=FALSE), dim=3)
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    map                      <- toolGetMappingCoord2Country()
    getCells(land)           <- paste(map$coords, map$iso, sep=".")
    names(dimnames(land))[1] <- "x.y.iso"
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

  } else if (avlland_scen == "curr_irrig") {
    # Total irrigated cropland per cell:
    land <- dimSums(collapseNames(calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)[,,"irrigated"]), dim=3)
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    map                      <- toolGetMappingCoord2Country()
    getCells(land)           <- paste(map$coords, map$iso, sep=".")
    names(dimnames(land))[1] <- "x.y.iso"
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

  } else {
    stop("Please choose an appropriate available land scenario in avlland_scen argument: curr_irrig (only currently irrigated cropland available for irrigated agriculture), curr_cropland (only current cropland areas available for irrigated agriculture), pot_irrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)")
  }

  # Adjust dimensionality
  tmp  <- land
  land <- new.magpie(getCells(land), selectyears, getNames(land), fill=1)
  land <- tmp * land

  # Areas that are already irrigated (by committed agricultural uses)
  if (!is.null(comagyear)) {
    # subtract area already reserved for irrigation by committed agricultural uses [in mio. ha] (to avoid double accounting)
    irrig_area_com  <- calcOutput("IrrigAreaCommitted", selectyears=selectyears, iniyear=comagyear, aggregate=FALSE)
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
