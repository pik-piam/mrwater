#' @title       calcCropareaAdjusted
#' @description This returns croparea as reported by FAO and LUH for the
#'              initialization year and
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
  # current multiple cropping factor from FAO
  # mc   <- calcOutput("Multicropping", extend_future = FALSE,
  #                    factortype = "MC", aggregate = FALSE)[, iniyear, ]
  # countrylist <- intersect(sort(unique(getCells(dimSums(phys, dim = 1.2)))),
  #                          sort(unique(getCells(mc))))
  # mc   <- mc[countrylist, , ]
  #
  # # calculate area multicropped (in Mha)
  # harv <- phys * mc

  ### NOTE: Problem that mutlicropping potential is equally applied to rainfed
  ### and irrigated areas. In reality might specifically apply only to irrigated
  ### areas.

  ### UNTIL calcCroparea is adjusted for 67k cell names ###
  # tranform cell names such that they match with the rest of mrwater library
  map             <- toolGetMappingCoord2Country()
  getCells(phys)  <- paste(map$coords, map$iso, sep = ".")
  # getCells(harv)  <- paste(map$coords, map$iso, sep = ".")
  ### UNTIL calcCroparea is adjusted for 67k cell names ###

  # area under multicropping
 # multicroppedArea <- pmax(harv - phys, 0)

  # croparea per season
  # croparea <- add_dimension(new.magpie(cells_and_regions = getCells(phys),
  #                                      years = NULL,
  #                                      names = getNames(phys),
  #                                      fill = NA),
  #                           dim = 3.2, add = "season", nm = c("first", "second"))
  # croparea[, , "first"]  <- phys
  # croparea[, , "second"] <- multicroppedArea

  ### NOTE: Assumption that everything that goes beyond physical area is assigned
  ### to off-season (second). In reality it is not clear which part of it is
  ### first and second season.
  ### ALTERNATIVE assumption would be to assign full physical area to second
  ### season where multicropping factor is > 1
  ### Both is not correct... Truth somewhere in the middle. Maybe covered in
  ### Sebastian's data set?

  croparea          <- phys
  getSets(croparea) <- c("x", "y", "iso", "year", "irrigation", "crop")

  # Check for NAs
  if (any(is.na(croparea))) {
    stop("Function calcCropareaAdjusted produced NAs")
  }

  return(list(x            = croparea,
              weight       = NULL,
              unit         = "million ha",
              description  = "cellular croparea per crop",
              isocountries = FALSE))
}
