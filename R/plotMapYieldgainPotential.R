#' @title       plotMapYieldgainPotential
#' @description plots map of irrigation yield gain potential in USD/ha
#'              for water limited or unlimited irrigatable areas or their
#'              ratio
#'
#' @param output            Output to be plotted: "unlimited", "limited" or "ratio
#' @param scenario          Non-agricultural water use and EFP scenario, separated
#'                          by "." (e.g. "on.ssp2")
#' @param lpjml             LPJmL version used
#' @param selectyears       Years for which yield gain potential is calculated
#' @param iniyear           Initialization year
#' @param climatetype       Switch between different climate scenarios or
#'                          historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod         EFR method used including selected strictness of EFRs
#'                          (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod        Method of calculating the rank:
#'                          "meancellrank": mean over cellrank of proxy crops,
#'                          "meancroprank": rank over mean of proxy crops (normalized),
#'                          "meanpricedcroprank": rank over mean of proxy crops (normalized using price),
#'                          "watervalue": rank over value of irrigation water;
#'                          and fullpotentail TRUE/FALSE separated by ":"
#'                          (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area).
#'                          FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param yieldcalib        FAO (LPJmL yields calibrated with current FAO yield) or
#'                          calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                          smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                          smoothed_calib (smoothed LPJmL yield potentials, not harmonized, calibrated for proxycrops)
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param thresholdtype     Thresholdtype of yield improvement:
#'                          TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (same unit as thresholdtype)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
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

plotMapYieldgainPotential <- function(output, scenario, selectyears, iniyear, lpjml,
                                      climatetype, EFRmethod, accessibilityrule,
                                      yieldcalib, rankmethod, thresholdtype, gainthreshold,
                                      allocationrule, irrigationsystem, multicropping, cropmix, avlland_scen) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Yield Gain Potential ")
  }

  unlim <- calcOutput("YieldgainPotential", scenario = scenario, selectyears = selectyears,
                  iniyear = iniyear, lpjml = lpjml, climatetype = climatetype,
                  EFRmethod = EFRmethod, yieldcalib = yieldcalib, irrigationsystem = irrigationsystem,
                  accessibilityrule = accessibilityrule, rankmethod = rankmethod,
                  gainthreshold = gainthreshold,
                  allocationrule = allocationrule, avlland_scen = avlland_scen,
                  cropmix = cropmix, multicropping = multicropping, unlimited = TRUE, aggregate = FALSE)

  lim <- calcOutput("YieldgainPotential", scenario = scenario, selectyears = selectyears,
                      iniyear = iniyear, lpjml = lpjml, climatetype = climatetype,
                      EFRmethod = EFRmethod, yieldcalib = yieldcalib, irrigationsystem = irrigationsystem,
                      accessibilityrule = accessibilityrule, rankmethod = rankmethod,
                      gainthreshold = gainthreshold,
                      allocationrule = allocationrule, avlland_scen = avlland_scen,
                      cropmix = cropmix, multicropping = multicropping, unlimited = FALSE, aggregate = FALSE)

  if (output == "limited") {

    x      <- lim
    unit   <- "USD"
    limits <- c(0, 100)

  } else if (output == "unlimited") {

    x      <- unlim
    unit   <- "USD"
    limits <- c(0, 100)

  } else if (output == "ratio") {

    x     <- ifelse(unlim > 0, lim / unlim, 0)
    unit  <- "ratio limited / unlimited"
    limits <- c(min(x), max(x))

  } else {

    stop("Please select output to be plotted: limited, unlimited or ratio")

  }

  # relevant map area
  area         <- calcOutput("AreaPotIrrig", selectyears = selectyears,
                              comagyear = NULL, avlland_scen = avlland_scen, aggregate = FALSE)
  x[area == 0] <- NA

  yieldGain   <- calcOutput("IrrigYieldImprovementPotential", selectyears = selectyears,
                           lpjml = lpjml, climatetype = climatetype, cropmix = cropmix,
                           unit = thresholdtype, iniyear = iniyear, yieldcalib = yieldcalib,
                           multicropping = multicropping, aggregate = FALSE)
  x[yieldGain <= gainthreshold] <- NA

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
