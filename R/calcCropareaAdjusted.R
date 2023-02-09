#' @title       calcCropareaAdjusted
#' @description This returns croparea as reported by FAO and LUH for the
#'              initialization year and
#'
#' @param iniyear initialization year
#' @param dataset LUH or Toolbox.
#'                Note: once migration to Toolbox data is complete, this function
#'                can be replaced with calcCropareaToolbox
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

calcCropareaAdjusted <- function(iniyear, dataset = "Toolbox") {

  # read in physical croparea per crop and management type (in Mha)
  if (dataset == "LUH") {

    phys <- calcOutput("Croparea", years = iniyear, physical = TRUE,
                       sectoral = "kcr", irrigation = TRUE,
                       cells = "lpjcell", cellular = TRUE,
                       aggregate = FALSE)
    map             <- toolGetMappingCoord2Country()
    getCells(phys)  <- paste(map$coords, map$iso, sep = ".")

  } else if (dataset == "Toolbox") {

    phys <- calcOutput("CropareaToolbox", physical = TRUE, sectoral = "kcr",
                         cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                         selectyears = iniyear, aggregate = FALSE)

  } else {
    stop("Please select Croparea data set to be used.")
  }

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
