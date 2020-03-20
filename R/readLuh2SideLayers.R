#' @title readLuh2SideLayers
#' @description Read LUH2 land cover data of biodiversity loss
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Michael Windisch
#' @examples
#'
#' \dontrun{
#'   readSource("Luh2SideLayers", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readLuh2SideLayers <- function(){

  x <- read.magpie("luh2_side_layers_0.5.mz")

  return(x)
}
