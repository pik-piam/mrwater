#' @title       plotMapEconOfIrrig
#' @description plot of irrigatable area depending on costs paid for irrigation
#'
#' @param areacorrect      TRUE: only cropland area > 0.01*cellarea are shown
#' @param reference        LUH or committed current irrigated areas are printed in grey as reference to predicted
#' @param legend_scale     Legend scale to be displayed: Mha (million hectares), Cellshare (share of cellarea), Areashare (share of area selected in avlland_scen)
#' @param scenario         EFP and non-agricultural water use scenarios separate by "." (e.g. "on.ssp2")
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen  Land availability scenario (currCropland, currIrrig, potIrrig)
#'                      combination of land availability scenario and initialization year separated by ":".
#'                      protection scenario separated by "_" (only relevant when potIrrig selected):
#'                      WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotMapEconOfIrrig()
#' }
#'
#' @importFrom luplot plotmap2
#' @importFrom magclass collapseNames
#' @importFrom ggplot2 theme scale_fill_continuous element_blank element_rect
#'
#' @export

plotMapEconOfIrrig <- function(areacorrect, reference, legend_scale, scenario, lpjml, selectyears, climatetype, EFRmethod, accessibilityrule, rankmethod, yieldcalib, allocationrule, thresholdtype, irrigationsystem, avlland_scen, cropmix, com_ag, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  if (!requireNamespace("cowplot", quietly = TRUE)) stop("The package cowplot is required for plotting MapEconOfIrrig!")

  x0                <- dimSums(collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = 0,
                                                        selectyears = selectyears, climatetype = climatetype,
                                                        accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                                        allocationrule = allocationrule, thresholdtype = thresholdtype,
                                                        irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                                                        cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag,
                                                        multicropping = multicropping, aggregate = FALSE)[, , scenario][, , "irrigatable"]), dim = "season")
  x0[x0 == 0]       <- NA
  x500              <- dimSums(collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = 250,
                                                        selectyears = selectyears, climatetype = climatetype,
                                                        accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                                        allocationrule = allocationrule, thresholdtype = thresholdtype,
                                                        irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                                                        cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag,
                                                        multicropping = multicropping, aggregate = FALSE)[, , scenario][, , "irrigatable"]), dim = "season")
  x500[x500 == 0]   <- NA
  x1000             <- dimSums(collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = 1500,
                                                        selectyears = selectyears, climatetype = climatetype,
                                                        accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                                        allocationrule = allocationrule, thresholdtype = thresholdtype,
                                                        irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                                                        cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag,
                                                        multicropping = multicropping, aggregate = FALSE)[, , scenario][, , "irrigatable"]), dim = "season")
  x1000[x1000 == 0] <- NA

  if (legend_scale == "Mha") {
    y           <- 1
    legendtitle <- "Mha"
    legendrange <- c(0, 0.3)
  } else if (legend_scale == "Cellshare") {
    y <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type = "cell")
    y <- (111e3 * 0.5) * (111e3 * 0.5) * cos(y$lat / 180 * pi) / 1000000000 # mio. ha
    y <- as.magpie(y, spatial = 1)
    getCells(y) <- getCells(x0)
    legendtitle <- "Cellshare"
    legendrange <- c(0, 0.1)
  } else if (legend_scale == "Areashare") {
    y <- calcOutput("AreaPotIrrig", selectyears = selectyears, comagyear = NULL, avlland_scen = avlland_scen, aggregate = FALSE)
    legendtitle <- "Areashare"
    legendrange <- c(0, 0.1)
  } else if (legend_scale == "Boolean") {
    x0[x0 > 0] <- 1
    x500[x500 > 0] <- 1
    x1000[x1000 > 0] <- 1
    y <- 1
    legendtitle <- ""
    legendrange <- c(0, 1)
  } else {
    stop("Please select legend_scale to be displayed: Mha, Cellshare or Areashare")
  }

  if (areacorrect) {
    z <- calcOutput("AreaPotIrrig", selectyears = selectyears, comagyear = NULL, avlland_scen = avlland_scen, aggregate = FALSE)
    w <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type = "cell")
    w <- (111e3 * 0.5) * (111e3 * 0.5) * cos(w$lat / 180 * pi) / 1000000000 # mio. ha
    w <- as.magpie(w, spatial = 1)
    getCells(w) <- getCells(z)

    cellshare <- z / w
    cellshare[cellshare < 0.01] <- 0

    x0[cellshare == 0]       <- NA
    x500[cellshare == 0]     <- NA
    x1000[cellshare == 0]    <- NA

  }

  x0    <- x0 / y
  x500  <- x500 / y
  x1000 <- x1000 / y

  p1 <- plotmap2(toolLPJcell2MAgPIEcell(x1000), title = element_blank(), labs = FALSE, sea = FALSE, land_colour = "transparent") +
    scale_fill_continuous(legendtitle, limits = legendrange, low = "thistle2", high = "darkred", guide = "colorbar", na.value = "transparent") +
    theme(title = element_blank(),
      legend.position = c(0.1, 0.3), legend.direction = "vertical",
      panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white"))
  p2 <- plotmap2(toolLPJcell2MAgPIEcell(x500), title = element_blank(), labs = FALSE, sea = FALSE, land_colour = "transparent") +
    scale_fill_continuous("", limits = legendrange, low = "lightblue", high = "darkblue", guide = "colorbar", na.value = "transparent") +
    theme(title = element_blank(),
      legend.position = c(0.08, 0.3), legend.direction = "vertical",
      panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white"))
  p3 <- plotmap2(toolLPJcell2MAgPIEcell(x0), title = element_blank(), labs = FALSE, sea = FALSE, land_colour = "transparent") +
    scale_fill_continuous("", limits = legendrange, low = "lightgreen", high = "darkgreen", guide = "colorbar", na.value = "transparent") +
    theme(title = element_blank(),
      legend.position = c(0.06, 0.3), legend.direction = "vertical",
      panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white"))

  if (reference == "committed") {
    xC              <- dimSums(collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = 0,
                                                selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
                                                EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib,
                                                allocationrule = allocationrule, thresholdtype = thresholdtype,
                                                irrigationsystem = irrigationsystem, avlland_scen = avlland_scen, cropmix = cropmix,
                                                potential_wat = FALSE, com_ag = TRUE, aggregate = FALSE)[, , scenario][, , "irrigatable"]), dim = "season")
    xC[xC > 0]        <- TRUE
    xC[xC == 0]       <- FALSE

    if (areacorrect) {
      xC[cellshare == 0] <- NA
    }

    pC <- plotmap2(toolLPJcell2MAgPIEcell(xC), title = element_blank(), labs = FALSE, sea = FALSE, land_colour = "transparent") +
      scale_fill_continuous(low = "transparent", high = "#00000080", na.value = "transparent") +
      theme(title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white"))

    out <- cowplot::ggdraw() + cowplot::draw_plot(p3) + cowplot::draw_plot(p2) + cowplot::draw_plot(p1) + cowplot::draw_plot(pC)
  } else if (reference == "LUH") {
    xC <- dimSums(calcOutput("Croparea", years = selectyears, sectoral = "kcr", cells = "lpjcell",
      physical = TRUE, cellular = TRUE, irrigation = TRUE, aggregate = FALSE)[, , "irrigated"], dim = 3)
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    map                    <- toolGetMappingCoord2Country()
    getCells(xC)           <- paste(map$coords, map$iso, sep = ".")
    names(dimnames(xC))[1] <- "x.y.iso"
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    xC[xC > 0]        <- TRUE
    xC[xC == 0]       <- FALSE

    if (areacorrect) {
      xC[cellshare == 0] <- NA
    }

    pC <- plotmap2(toolLPJcell2MAgPIEcell(xC), title = element_blank(), labs = FALSE, sea = FALSE, land_colour = "transparent") +
      scale_fill_continuous(low = "transparent", high = "#00000080", na.value = "transparent") +
      theme(title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white"))

    out <- cowplot::ggdraw() + cowplot::draw_plot(p3) + cowplot::draw_plot(p2) + cowplot::draw_plot(p1) + cowplot::draw_plot(pC)
  } else {
    out <- cowplot::ggdraw() + cowplot::draw_plot(p3) + cowplot::draw_plot(p2) + cowplot::draw_plot(p1)
  }

  return(out)
}
