#' @title readBendingTheCurve
#' @description Read bending the curve data
#' @param subtype Data used in the Bending the Curve initiative. Type "rr_layer" for the range-size rarity layer and "luh2_side_layers" for the LUH2 Side Layers.
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Patrick v. Jeetze, Michael Windisch
#' @examples
#'
#' \dontrun{
#'   readSource("BendingTheCurve", subtype="rr_layer", convert="onlycorrect")
#' }
#'

readBendingTheCurve <- function(subtype){
  if(subtype=="rr_layer"){
    x <- read.magpie("rr_layer_0.5.mz")
    return(x)
  }
  if(subtype=="luh2_side_layers"){
    x <- read.magpie("luh2_side_layers_0.5.mz")
    return(x)
  }
  if(subtype!= "luh2_side_layers" | "rr_layer") {
    stop("Not a Valid Subtype")
  }
}


