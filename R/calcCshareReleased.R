#' @title calcCshareReleased
#' @description   "Share of soil carbon that is released on cropland during the harvest. Calculated by Benjamin Bodirsky"
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{calcOutput("CshareReleased", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import moinput
#' @importFrom magpiesets findset
#'

calcCshareReleased <-function(){

  x <- readSource("CshareReleased", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="share",
    description="Share of soil carbon that is released on cropland during the harvest. Calculated by Benjamin Bodirsky",
    isocountries=FALSE))
}
