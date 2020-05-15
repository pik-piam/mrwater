#' @title readWATERGAP
#' @description Read in non-agricultural water demand data from WATERGAP model with A2: WATERGAP WATCH project; B1: WATERGAP WATCH project; SSP2: WATERGAP ISIMIP project
#' @param subtype Data source to be read from
#' @return MAgPIE object of non-agricultural water demand at 0.5 cellular level in mio. m^3
#' @author Felicitas Beier, Abhijeet Mishra
#'
#' @examples
#' \dontrun{ readSource("WATERGAP", convert="onlycorrect") }
#'
#' @import madrat
#' @import magclass

readWATERGAP <- function(subtype="WATERGAP2013"){

  x <- read.magpie(paste0(subtype,"/watdem_nonagr_0.5.mz"))

  return(x)
}




