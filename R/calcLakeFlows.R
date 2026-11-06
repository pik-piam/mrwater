#' @title       calcLakeFlows
#' @description This function calculates evaporation from water bodies
#'              or inputs to water bodies
#'              based on LPJmL inputs
#'
#' @param lpjml       LPJmL version used
#' @param climatetype Switch between different climate scenarios
#'                    or historical baseline "GSWP3-W5E5:historical"
#' @param subtype     Switch between water inputs to water bodies ("input_lake")
#'                    and evaporation from water bodies ("evap_lake")
#'
#' @importFrom magclass collapseNames new.magpie getCells mbind setYears
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("LakeFlows", aggregate = FALSE)
#' }
#'
calcLakeFlows <- function(lpjml, climatetype, subtype) {

  # Lake area from LPJmL (in ha) for one year (static over time)
  lakeArea <- calcOutput("LPJmLTransform", subtype = "pnv:lake_area",
                         stage = "raw:Fullhist",
                         lpjmlversion = lpjml, climatetype = climatetype,
                         monthly = FALSE,
                         aggregate = FALSE)
  # To Do (Feli): use LUH lake area (if possible)

  if (subtype == "input_lake") {
    # Precipitation from LPJmL (in m^3/ha) [smoothed & harmonized]
    x <- calcOutput("LPJmLHarmonize", subtype = "pnv:prec",
                    lpjmlversion = lpjml, climatetype = climatetype,
                    monthly = "FALSE:sum",
                    aggregate = FALSE)
    description <- "precipitation on water bodies"
  } else if (subtype == "evap_lake") {
    # Lake evaporation from LPJmL (in m^3/ha) [smoothed & harmonized]
    x <- calcOutput("LPJmLHarmonize", subtype = "pnv:evap_lake",
                    lpjmlversion = lpjml, climatetype = climatetype,
                    monthly = "FALSE:sum",
                    aggregate = FALSE)
    description <- "evaporation from water bodies"
  } else {
    stop("Please select subtype in calcLakeFlows.
          For evaporation from water bodies select `evap_lake`.
          For precipitation on water bodies select `input_lake`")
  }

  # Transform to flow (from m^3/ha to mio. m^3)
  x <- x * lakeArea * 1e-6

  return(list(x            = x,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
