#' @title calcAfforestationMask
#' @description Afforestation mask for where afforestation possible
#' @param subtype afforestation mask sub type
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("AfforestationMask", subtype="noboreal", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom madrat readSource

calcAfforestationMask <-function(subtype){

  x <- readSource("AfforestationMask",subtype=subtype, convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="binary",
    description="",
    isocountries=FALSE))
}
