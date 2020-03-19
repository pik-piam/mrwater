#' @title calcAff_unrestricted
#' @description no documentation available
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("Aff_unrestricted", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import moinput
#' @importFrom magpiesets findset
#'

calcAff_unrestricted <-function(){

  x <- readSource("Aff_unrestricted", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="Mha",
    description="",
    isocountries=FALSE))
}
