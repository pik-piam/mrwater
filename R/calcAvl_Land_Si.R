#' @title calcAvl_Land_Si
#' @description Extracts si0 and nsi0 areas based on Ramankutty dataset
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("Avl_Land_Si", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import moinput
#' @importFrom magpiesets findset
#'

calcAvl_Land_Si <-function(){

  x <- readSource("Avl_Land_Si", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="Mha",
    description="si and nsi0 areas",
    isocountries=FALSE))
}
