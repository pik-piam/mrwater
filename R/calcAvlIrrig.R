#' @title calcAvl_irrig
#' @description Function extracts Area equipped for irrigation for the period 1997-2002,
#' data revision: 18, citation: Siebert et al.; 2007 ; Global Map of Irrigation Areas version 4.0.1
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("AvlIrrig", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#'

calcAvlIrrig <-function(){

  x <- readSource("AvlIrrig", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="Mha",
    description="Area equipped for irrigation for the period 1997-2002",
    isocountries=FALSE))
}
