#' @title calcCropAreaInitialization
#' @description Function to generate the initialization values for crop-specific physical area with half-degree resolution
#'
#' @return magpie object in cellular resolution
#' @author Edna Molina Bacca
#' @param irrigation This parameter determines if the crops should be differentiated between irrigation methods
#' @param aggregate This parameter determines if the crops should be differentiated between irrigation methods
#' @examples
#' \dontrun{ calcOutput("calcCropAreaInitialization", aggregate = FALSE) }
#' @importFrom madrat toolAggregate

calcCropAreaInitialization<-function(aggregate=FALSE,irrigation=TRUE){


     data<- calcOutput("Croparea", sectoral="kcr", physical=TRUE, cellular=TRUE, irrigation=irrigation, aggregate = aggregate)
     x<-data

  return(list(
    x=x,
    weight=NULL,
    unit="million ha",
    description="Physical crop area from FAOSTAT",
    isocountries=FALSE))
}
