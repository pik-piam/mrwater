#' @title calcProtectArea
#' @description Function extracts conservation protected area
#'
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("ProtectArea", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets findset
#'

calcProtectArea <-function(){

  x <- readSource("ProtectArea", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="Mha",
    description="conservation priority areas",
    isocountries=FALSE))
}
