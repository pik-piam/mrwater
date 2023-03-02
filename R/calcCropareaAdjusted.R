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

  # Check for landmass mismatch: Total physical croparea should be < landmass
  # Total physical croparea
  physTotal <- dimSums(phys, dim = 3)
  # Total land area according to LUH
  landarea <- setYears(collapseNames(dimSums(readSource("LUH2v2", subtype = "states_1995to1996",
                                                        convert = "onlycorrect")[, "y1995", ],
                                             dim = 3)),
                       NULL)

  if (any(landarea - physTotal < 0)) {
    # Note: Due to mismatches in the land masks used in the Toolbox (LandInG)
    #       and LUH, croparea may exceed total landarea.
    #       It is cut off here, but this issue should be addressed at the step
    #       of the generation of the Toolbox data (FELI!)
    vcat(verbosity = 0,
         paste0("In calcCropareaAdjusted: There is a mismatch in the landmask underlying
                the ", dataset, " croparea dataset and LUH.
                Croparea is cut here to fit into the landmass as reported by LUH,
                but a more generic solution should be found to make the data consistent!"))

    ratio <- ifelse(landarea - physTotal < 0,
                      landarea / physTotal,
                    1)
    # Scale down crop-specific croparea by mismatch area
    phys <- phys * ratio

  }

  croparea          <- phys
  getSets(croparea) <- c("x", "y", "iso", "year", "irrigation", "crop")

  # Check for NAs
  if (any(is.na(croparea))) {
    stop("Function calcCropareaAdjusted produced NAs")
  }
  # Check for mismatches
  if (any(landarea - dimSums(croparea, dim = 3) < 0)) {
    stop("Mismatch in croparea and landmass has not been addressed sufficiently.
         Please revisit calcCropareaAdjusted.")
  }

  return(list(x            = croparea,
              weight       = NULL,
              unit         = "million ha",
              description  = "cellular croparea per crop",
              isocountries = FALSE))
}
