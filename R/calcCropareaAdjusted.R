#' @title       calcCropareaAdjusted
#' @description This function adjusts the cell names that mrcommons::calcCroparea
#'              returns, so that they have the correct format for the mrwater library
#'              Note: This function is only necessary until the 67k cell names
#'              are fully integrated in mrcommons (specifically calcCroparea and calcLUH2v2)
#'
#' @param sectoral   "area_harvested" returns croparea aggregated to FAO products,
#'                   "ProductionItem" unaggregated ProdSTAT items,
#'                   "FoodBalanceItem" Food Balance Sheet categories,
#'                   "kcr" MAgPIE items, and "lpj" LPJmL items
#' @param physical   if TRUE the sum over all crops agrees with the cropland area per country
#' @param cellular   if TRUE: calculates cellular MAgPIE crop area for all magpie croptypes.
#'                   Crop area from LUH2 crop types (c3ann, c4ann, c3per, c4per, cnfx)
#'                   are mapped to MAgpIE crop types using mappingLUH2cropsToMAgPIEcrops.csv.
#'                   Harvested areas of FAO weight area within a specific LUH crop type
#'                   to divide into MAgPIE crop types.
#' @param cells      Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param irrigation If true: cellular areas are returned separated
#'                   into irrigated and rainfed (see setup in calcLUH2v2)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("CropareaAdjusted", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass getCells
#' @importFrom mrcommons toolGetMappingCoord2Country

calcCropareaAdjusted <- function(sectoral, physical, cellular, cells, irrigation) {

  # read in croparea from original function
  croparea <- calcOutput("Croparea", sectoral = sectoral, physical = physical,
                                  cells = cells, cellular = cellular,
                                  irrigation = irrigation, aggregate = FALSE)

  # tranform cell names such that they match with the rest of mrwater library
  map                          <- toolGetMappingCoord2Country()
  getCells(croparea)           <- paste(map$coords, map$iso, sep = ".")
  names(dimnames(croparea))[1] <- "x.y.iso"

  # Check for NAs
  if (any(is.na(croparea))) {
    stop("Function calcCropareaAdjusted produced NAs")
  }

  return(list(x            = croparea,
              weight       = NULL,
              unit         = "million ha",
              description  = "harvested crop areas from FAOSTAT",
              isocountries = !cellular))
}
