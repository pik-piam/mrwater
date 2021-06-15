#' @title       plotMapPotReturnToIrrigation
#' @description plots map of return to irrigation in terms of monetary yield gain in USD/ha
#'              or water value in USD/m^3
#'
#' @param selectyears      year for which map is displayed
#' @param unit             unit of return to irrigation
#'                         (perha for monetary yield gain per cell in USD05/ha
#'                         or perm3 for irrigation water value in USD05/m^3)
#' @param lpjml            LPJmL version required for return to irrigation
#' @param iniyear          Initialization year for return to irrigation
#' @param climatetype      Climate scenario for return to irrigation
#' @param yieldcalib       yieldcalib for return to irrigation (FAO, calibrated,
#'                         smoothed, smoothed_calib)
#' @param multicropping    multicropping for return to irrigation (TRUE or FALSE)
#' @param cropmix          cropmix for calculation of return to irrigation
#'                         (proxycrop(s) or hist_irrig or hist_total)
#' @param land             land area to be displayed
#'                         combination of land availability scenario and initialization year separated by ":"
#'                         land availability scenario: currIrrig, currCropland, potIrrig
#'                         protection scenario separated by "_" (only relevant when potIrrig selected):
#'                         WDPA, BH, FF, CPD, LW, HalfEarth
#'                         (e.g. "potIrrig_HalfEarth:1995", currCropland:2010)
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMapPotReturnToIrrigation() }
#'
#' @importFrom luplot plotmap2
#'
#' @export

plotMapPotReturnToIrrigation <- function(selectyears, unit, iniyear, lpjml, climatetype, yieldcalib, multicropping, cropmix, land) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Map depicting the share of current irrigation that can be fulfilled given surface water availability of the algorithm")
  }

  if (unit=="perha") {
    # Read in monetary potential yield gain per cell (USD05 per ha)
    return <- calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype, selectyears = selectyears,
                             cropmix = cropmix, monetary = TRUE, iniyear = iniyear, yieldcalib = yieldcalib, multicropping = multicropping, aggregate = FALSE)
  } else if (unit=="perm3") {
    # Read in water value per cell (USD05 per m^3)
    return <- calcOutput("IrrigWatValue", lpjml = lpjml, climatetype = climatetype, selectyears = selectyears,
                         cropmix = cropmix, iniyear = iniyear, yieldcalib = yieldcalib, multicropping = multicropping, aggregate = FALSE)
  } else {
    stop("Please select unit of irrigation return to be displayed on the map (perha or perm3)")
  }

  # relevant map area
  area              <- calcOutput("AreaPotIrrig", selectyears = selectyears, comagyear = NULL, avlland_scen = land, aggregate = FALSE)
  return[area == 0] <- NA

  out <- plotmap2(toolLPJcell2MAgPIEcell(return), title = element_blank(), labs = FALSE, sea = FALSE, land_colour = "transparent") +
                  scale_fill_continuous("", low = "#FFFF66", high = "darkgreen", na.value = "grey") +
                  theme(title=element_blank(),
                        panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white"))

  return(out)
}
