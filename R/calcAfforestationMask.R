#' @title calcAfforestationMask
#' @description Afforestation mask for where afforestation possible
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("AfforestationMask", subtype="noboreal", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets findset
#'

calcAfforestationMask <-function(subtype){

  x <- readSource("AfforestationMask",subtype=subtype, convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="binary",
    description="",
    isocountries=FALSE))
}
