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


correctBendingTheCurve <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
