#' @title calcAvlLandSi
#' @description Extracts si0 and nsi0 areas based on Ramankutty dataset
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("AvlLandSi", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets findset
#'

calcAvlLandSi <-function(){

  x <- readSource("AvlLandSi", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="Mha",
    description="si and nsi0 areas",
    isocountries=FALSE))
}
