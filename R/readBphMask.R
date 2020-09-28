#' @title readBphMask
#' @description Read Mask of Datapoints of biogeophysical temperature change of afforestation (degree C) to be used as weight. File is based on observation datasets of Bright et al. 2017 and Duveiller et al. 2018
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Michael Windisch
#' @examples
#'
#' \dontrun{
#'   readSource("readBphMask", convert="onlycorrect")
#' }
#'

readBphMask <- function(){

  x <- read.magpie("f32_bph_mask_0.5.mz")

  return(x)
}
