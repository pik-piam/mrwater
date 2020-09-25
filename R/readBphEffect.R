#' @title readBphEffect
#' @description Read Biogeophysical temperature change of afforestation (degree C). File is based on observation datasets of Bright et al. 2017 and Duveiller et al. 2018
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Michael Windisch
#' @examples
#'
#' \dontrun{
#'   readSource("BphEffect", convert="onlycorrect")
#' }
#'

readBphEffect <- function(){

  x <- read.magpie("f32_bph_effect_noTCRE_0.5.mz")

  return(x)
}
