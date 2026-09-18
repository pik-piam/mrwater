#' @title       calcRunoffMonthly
#' @description This function calculates monthly runoff from inputs provided by LPJmL
#'              It is used in calcAvlWater and will be deprecated when the mrwater
#'              pipeline is fully integrated in MAgPIE
#'
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop
#' @param climatetype Switch between different climate scenarios or
#'                    historical baseline "GSWP3-W5E5:historical"
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RunoffMonthly", aggregate = FALSE)
#' }
#'
calcRunoffMonthly <- function(lpjml, climatetype) {

  # Monthly runoff (m^3/ha) [smoothed & harmonized]
  x  <- calcOutput("LPJmLTransform", subtype = "pnv:runoff", stage = "raw:cut",
                   lpjmlversion = lpjml, climatetype = climatetype,
                   monthly = TRUE,
                   aggregate = FALSE)

  # LUH landarea (in Mha)
  # Note: Since land area is constant, any year can be chosen
  landArea <- setYears(dimSums(calcOutput("LUH3", yrs = "y1995",
                                          landuseTypes = "LUH3", irrigation = FALSE,
                                          cellular = TRUE,  aggregate = FALSE),
                               dim = 3),
                       NULL)

  ### Transformation to flow on land ###
  # Transformation factor: 1 m^3/ha = 1e-6 mio. m^3/ha
  # Transformation factor: 1 Mha    = 1e+6 ha
  x <- x * landArea
  # new unit
  unit <- "mio. m^3"

  return(list(x            = x,
              weight       = NULL,
              unit         = unit,
              description  = "monthly runoff",
              isocountries = FALSE))
}
