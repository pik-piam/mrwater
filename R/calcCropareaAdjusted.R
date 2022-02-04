#' @title       calcCropareaAdjusted
#' @description This returns croparea as reported by FAO and LUH for the
#'              initialization year and
#'              splits croparea into first and second season croparea
#'
#' @param iniyear initialization year
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
#' @importFrom magclass getCells getNames add_dimension new.magpie
#' @importFrom mrcommons toolGetMappingCoord2Country

calcCropareaAdjusted <- function(iniyear) {

  # read in physical croparea per crop and management type (in Mha)
  phys <- calcOutput("Croparea", years = iniyear, physical = TRUE,
                      sectoral = "kcr", irrigation = TRUE,
                      cells = "lpjcell", cellular = TRUE,
                      aggregate = FALSE)
  # read in harvested croparea per crop and management type (in Mha)
  harv <- calcOutput("Croparea", years = iniyear, physical = FALSE,
                     sectoral = "kcr", irrigation = TRUE,
                     cells = "lpjcell", cellular = TRUE,
                     aggregate = FALSE)

  ### UNTIL calcCroparea is adjusted for 67k cell names ###
  # tranform cell names such that they match with the rest of mrwater library
  map             <- toolGetMappingCoord2Country()
  getCells(phys)  <- paste(map$coords, map$iso, sep = ".")
  getCells(harv)  <- paste(map$coords, map$iso, sep = ".")
  ### UNTIL calcCroparea is adjusted for 67k cell names ###

  # area under multicropping
  multicroppedArea <- pmax(harv - phys, 0)

  # croparea per season
  croparea <- add_dimension(new.magpie(cells_and_regions = getCells(phys),
                                       years = NULL,
                                       names = getNames(phys),
                                       fill = NA),
                            dim = 3.2, add = "season", nm = c("first", "second"))
  croparea[, , "first"]  <- phys
  croparea[, , "second"] <- multicroppedArea
  getSets(croparea)      <- c("x", "y", "iso", "year", "irrigation", "season", "crop")

  # Check for NAs
  if (any(is.na(croparea))) {
    stop("Function calcCropareaAdjusted produced NAs")
  }

  return(list(x            = croparea,
              weight       = NULL,
              unit         = "million ha",
              description  = "cellular croparea per crop, season, management type",
              isocountries = FALSE))
}
