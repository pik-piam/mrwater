#' @title readAff_noboreal
#' @description Read Afforestation No Boreal dataset
#' @param subtype afforestation mask sub type
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("AfforestationMask", subtype="noboreal", convert="onlycorrect")
#' }
#'

readAfforestationMask <- function(subtype){

  if(subtype=="noboreal"){
  x <- read.magpie("aff_noboreal_0.5.mz")
}
  if(subtype=="onlytropical"){
    x <- read.magpie("aff_onlytropical_0.5.mz")
    }

  if(subtype=="unrestricted"){
    x <- read.magpie("aff_unrestricted_0.5.mz")
  }

  return(x)
}
