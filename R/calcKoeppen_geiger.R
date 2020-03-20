#' @title calcKoeppen_geiger
#' @description fraction of a cell belonging to a given climate classification,reference: Rubel, F., and M. Kottek, 2010: Observed and projected climate shifts 1901-2100 depicted by world maps of the Koeppen-Geiger climate classification. Meteorol. Z., 19, 135-141. DOI: 10.1127/0941-2948/2010/0430."
#' note: Missing values are replaced with the classification information which appears most often in the cells closest to the one with the missing information.
#'
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("Koeppen_geiger", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import moinput
#' @importFrom magpiesets findset
#'

calcKoeppen_geiger <-function(){

  x <- readSource("Koeppen_geiger", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="share",
    description="share of koeppen geiger area",
    isocountries=FALSE))
}
