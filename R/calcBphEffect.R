#' @title calcBphEffect
#' @description Biogeophysical temperature change of afforestation (degree C). File is based on observation datasets of Bright et al. 2017 and Duveiller et al. 2018
#'# @param
#' @return magpie object in cellular resolution
#' @author Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("BphEffect", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom madrat readSource

calcBphEffect <-function(){

  x <- readSource("BphEffect", convert="onlycorrect")

  bph_mask_weight <- calcOutput("BphMask", aggregate="cluster", round=6)

  return(list(
    x=x,
    weight=bph_mask_weight,
    unit="degC",
    description="Biogeophysical temp change of afforestation in degC",
    isocountries=FALSE))
}
