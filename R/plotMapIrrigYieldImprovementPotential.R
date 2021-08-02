#' @title       plotMapIrrigYieldImprovementPotential
#' @description plots map of irrigation yield improvement potential in USD/ha
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years for which yield gain potential is calculated
#' @param iniyear           Initialization year
#' @param climatetype       Switch between different climate scenarios or
#'                          historical baseline "GSWP3-W5E5:historical"
#' @param yieldcalib        FAO (LPJmL yields calibrated with current FAO yield) or
#'                          calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                          smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                          smoothed_calib (smoothed LPJmL yield potentials, not harmonized, calibrated for proxycrops)
#' @param unit              Unit of yield improvement potential to be returned:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return
#' @param avlland_scen      Land availability scenario (currCropland, currIrrig, potIrrig)
#'                          combination of land availability scenario and initialization year separated by ":".
#'                          protection scenario separated by "_" (only relevant when potIrrig selected):
#'                          WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix           Cropmix for which irrigation yield improvement is calculated
#'                          can be selection of proxycrop(s) for calculation of average yield gain
#'                          or hist_irrig or hist_total for historical cropmix
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotMapYieldgainPotential()
#' }
#'
#' @importFrom luplot plotmap2
#' @importFrom ggplot2 scale_fill_discrete element_blank theme element_rect
#'
#' @export

plotMapIrrigYieldImprovementPotential <- function(selectyears, iniyear, lpjml, climatetype,
                                      yieldcalib, unit, multicropping, cropmix, avlland_scen) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Yield Gain Potential ")
  }

  x <- calcOutput("IrrigYieldImprovementPotential", selectyears = selectyears,
                  lpjml = lpjml, climatetype = climatetype, iniyear = iniyear,
                  cropmix = cropmix, unit = unit, yieldcalib = yieldcalib,
                  multicropping = multicropping, aggregate = FALSE)
  # sum over all seasons (-> total irrigation yield gain per year)
  x <- dimSums(x, dim = 3)

  # relevant map area
  area         <- calcOutput("AreaPotIrrig", selectyears = selectyears,
                              comagyear = NULL, avlland_scen = avlland_scen, aggregate = FALSE)
  x[area == 0] <- NA

  if (unit == "USD_ha") {
    limits <- c(0, 3000)
  } else if (unit == "tDM") {
    limits <- c(0, 2)
  } else if (unit == "USD_m3") {
    limits <- c(0, 1)
  }

  # plot output
  out <- plotmap2(toolLPJcell2MAgPIEcell(x), legend_range = limits,
                  title = element_blank(), labs = FALSE, sea = FALSE,
                  land_colour = "transparent", legendname = unit) +
          theme(title = element_blank(),
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                strip.background = element_rect(fill = "transparent", colour = NA),
                strip.text = element_text(color = "white"))

  return(out)
}
