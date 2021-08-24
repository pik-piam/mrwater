#' @title       calcAreaPotIrrig
#' @description This function calculates land that is potentially available
#'              for irrigated agriculture
#'
#' @param selectyears   years to be returned
#' @param comagyear     if NULL: total potential croparea is used;
#'                      if !NULL: already irrigated area is subtracted;
#'                      year specified here is the year of the initialization
#'                      used for cropland area initialization in calcIrrigatedArea (e.g. NULL, 1995, 2010)
#' @param avlland_scen  Land availability scenario (currCropland, currIrrig, potIrrig)
#'                      combination of land availability scenario and initialization year separated by ":".
#'                      protection scenario separated by "_" (only relevant when potIrrig selected):
#'                      WDPA, BH, FF, CPD, LW, HalfEarth
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("AreaPotIrrig", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput toolSplitSubtype
#' @importFrom magclass collapseNames getCells getYears getNames dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcAreaPotIrrig <- function(selectyears, comagyear, avlland_scen) {

  # retrieve function arguments
  iniyear      <- as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])
  avlland_scen <- as.list(strsplit(avlland_scen, split = ":"))[[1]][1]
  protect_scen <- as.list(strsplit(avlland_scen, split = "_"))[[1]][2]

  if (grepl("potIrrig", avlland_scen)) {
    # read in suitable land [in mio. ha]
    land <- collapseNames(calcOutput("AvlLandSi", aggregate = FALSE, cells = "lpjcell")[, , "si0"])

    # No irrigation in protected areas (if protection scenario is activated) [in mio. ha]
    if (!is.na(protect_scen)) {
      # read in protected area of selected scenario
      tmp <- collapseNames(calcOutput("ProtectArea", aggregate = FALSE)[, , protect_scen])
      #### expand to 67k cells (temporarily until read/calcProtectArea is adjusted) ####
      tmp <- collapseDim(addLocation(tmp), dim = c("region", "cell"))
      protect_area                    <- new.magpie(cells_and_regions = getCells(collapseDim(land, dim = "iso")),
                                                    years = getYears(tmp),
                                                    names = getNames(tmp), fill = 0,
                                                    sets = c("x.y.iso", "t", "data"))
      protect_area[getCells(tmp), , ] <- tmp
      #### expand to 67k cells (temporarily until read/calcProtectArea is adjusted) ####

      # total land area (Note: constant over the years.)
      landarea <- setYears(collapseNames(dimSums(readSource("LUH2v2", subtype = "states",
                                                            convert = "onlycorrect")[, "y1995", ],
                                                 dim = 3)),
                           NULL)
      landarea <- dimSums(landarea, dim = 3)

      # area available for cropland
      avl_cropland <- landarea - protect_area
      # land suitable for (sustainable) (irrigated) agriculture
      land         <- pmin(avl_cropland, land)
    }

  } else if (grepl("curr", avlland_scen)) {

    # Total current cropland per cell:
    if (avlland_scen == "currCropland") {

      land <- dimSums(calcOutput("CropareaAdjusted", years = iniyear,
                                 sectoral = "kcr", cells = "lpjcell",
                                 physical = TRUE, cellular = TRUE,
                                 irrigation = FALSE, aggregate = FALSE),
                      dim = 3)

    }

    # Total irrigated cropland per cell:
    if (avlland_scen == "currIrrig") {
      land <- dimSums(collapseNames(calcOutput("CropareaAdjusted", years = iniyear,
                                               sectoral = "kcr", cells = "lpjcell",
                                               physical = TRUE, cellular = TRUE,
                                               irrigation = TRUE, aggregate = FALSE)[, , "irrigated"]),
                      dim = 3)
    }

  } else {
    stop("Please choose an appropriate available land scenario in avlland_scen argument:
         currIrrig (only currently irrigated cropland available for irrigated agriculture),
         currCropland (only current cropland areas available for irrigated agriculture),
         potIrrig (suitable land is available for irrigated agriculture,
         potentially land protection activated through addition of protection scen in this argument)")
  }

  # Adjust dimensionality
  tmp  <- land
  land <- new.magpie(cells_and_regions = getCells(land),
                     years = selectyears,
                     names = getNames(land), fill = 1)
  land <- tmp * land

  # Areas that are already irrigated (by committed agricultural uses)
  if (!is.null(comagyear)) {
    # subtract area already reserved for irrigation by committed agricultural uses
    # (to avoid double accounting)
    irrig_area_com  <- calcOutput("IrrigAreaCommitted", selectyears = selectyears,
                                  iniyear = comagyear, aggregate = FALSE)
    irrig_area_com  <- collapseNames(dimSums(irrig_area_com, dim = 3))
    land            <- land - irrig_area_com
  }

  # correct negative land availability due to mismatch of available land and protected land or rounding imprecision
  land[land < 0] <- 0

  # Checks
  if (any(is.na(land))) {
    stop("Function AreaPotIrrig produced NA values")
  }

  if (any(round(land) < 0)) {
    stop("Function AreaPotIrrig produced negative values")
  }

  return(list(x            = land,
              weight       = NULL,
              unit         = "Mha",
              description  = "area potentially available for irrigated agriculture",
              isocountries = FALSE))
}
