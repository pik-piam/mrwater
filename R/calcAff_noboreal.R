#' @title calcAff_noboreal
#' @description no documentation available
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("Aff_noboreal", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import moinput
#' @importFrom magpiesets findset
#'

calcAff_noboreal <-function(){

  x <- readSource("Aff_noboreal", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="Mha",
    description="",
    isocountries=FALSE))
}
