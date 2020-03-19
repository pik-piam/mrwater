#' @title calcAff_onlytropical
#' @description no documentation available
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("Aff_onlytropical", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import moinput
#' @importFrom magpiesets findset
#'

calcAff_onlytropical <-function(){

  x <- readSource("Aff_onlytropical", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="Mha",
    description="",
    isocountries=FALSE))
}
