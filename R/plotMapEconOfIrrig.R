#' @title       plotMapEconOfIrrig
#' @description plot of irrigatable area depending on costs paid for irrigation
#'
#' @param reference        TRUE current irrigated areas are printed in grey as reference to predicted
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
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param proxycrop        proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMapEconOfIrrig() }
#'
#' @importFrom luplot plotmap2
#' @importFrom magclass collapseNames
#' @importFrom ggplot2 theme scale_fill_continuous element_blank element_rect
#'
#' @export

plotMapEconOfIrrig <- function(reference, legend_scale, scenario, lpjml, selectyears, climatetype, EFRmethod, accessibilityrule, rankmethod, yieldcalib, allocationrule, thresholdtype, irrigationsystem, avlland_scen, proxycrop, com_ag) {

  if (length(selectyears)>1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  if (!requireNamespace("cowplot", quietly = TRUE)) stop("The package ncdf4 is required for writing NCDF4 files!")

  x0              <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=0, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, yieldcalib=yieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=TRUE, com_ag=com_ag, aggregate=FALSE)[,,paste(scenario, "irrigatable", sep=".")])
  x0[x0==0]       <- NA
  x500            <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=500, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, yieldcalib=yieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=TRUE, com_ag=com_ag, aggregate=FALSE)[,,paste(scenario, "irrigatable", sep=".")])
  x500[x500==0]   <- NA
  x1000           <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=1000, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, yieldcalib=yieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=TRUE, com_ag=com_ag, aggregate=FALSE)[,,paste(scenario, "irrigatable", sep=".")])
  x1000[x1000==0] <- NA

  if (legend_scale=="Mha") {
    y           <- 1
    legendtitle <- "Mha"
    legendrange <- c(0, 0.3)
  } else if (legend_scale=="Cellshare") {
    y <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell")
    y <- (111e3*0.5)*(111e3*0.5)*cos(y$lat/180*pi)/1000000000 # mio. ha
    y <- as.magpie(y, spatial=1)
    getCells(y) <- getCells(x0)
    legendtitle <- "Cellshare"
    legendrange <- c(0, 0.1)
  } else if (legend_scale=="Areashare") {
    y <- calcOutput("AreaPotIrrig", selectyears=selectyears, comagyear=NULL, avlland_scen=avlland_scen, aggregate=FALSE)
    legendtitle <- "Areashare"
    legendrange <- c(0, 0.1)
  } else {
    stop("Please select legend_scale to be displayed: Mha, Cellshare or Areashare")
  }

  x0    <- x0/y
  x500  <- x500/y
  x1000 <- x1000/y

  p1 <- plotmap2(x1000[magclassdata$cellbelongings$LPJ_input.Index,,], title = element_blank(), labs= FALSE, sea=FALSE, land_colour = "transparent") +
    scale_fill_continuous(legendtitle, limits=legendrange, low="thistle2", high="darkred", guide="colorbar", na.value="transparent") +
    theme(title=element_blank(),
          legend.position = c(0.1,0.3), legend.direction = "vertical",
          panel.background = element_rect(fill="transparent", colour=NA),  plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="transparent", colour=NA), strip.text = element_text(color="white"))
  p2 <- plotmap2(x500[magclassdata$cellbelongings$LPJ_input.Index,,], title = element_blank(), labs= FALSE, sea=FALSE, land_colour = "transparent") +
    scale_fill_continuous("", limits=legendrange, low="lightblue", high="darkblue", guide="colorbar", na.value="transparent") +
    theme(title=element_blank(),
          legend.position = c(0.08,0.3), legend.direction = "vertical",
          panel.background = element_rect(fill="transparent", colour=NA),  plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="transparent", colour=NA), strip.text = element_text(color="white"))
  p3 <- plotmap2(x0[magclassdata$cellbelongings$LPJ_input.Index,,], title = element_blank(), labs= FALSE, sea=FALSE, land_colour = "transparent") +
    scale_fill_continuous("", limits=legendrange, low="lightgreen", high="darkgreen", guide="colorbar", na.value="transparent") +
    theme(title=element_blank(),
          legend.position = c(0.06,0.3), legend.direction = "vertical",
          panel.background = element_rect(fill="transparent", colour=NA),  plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="transparent", colour=NA), strip.text = element_text(color="white"))

  if (reference) {
    xC              <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=0, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, yieldcalib=yieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=FALSE, com_ag=com_ag, aggregate=FALSE)[,,paste(scenario, "irrigatable", sep=".")])
    xC[xC>0]        <- TRUE
    xC[xC==0]       <- FALSE

    pC <- plotmap2(xC[magclassdata$cellbelongings$LPJ_input.Index,,], title = element_blank(), labs= FALSE, sea=FALSE, land_colour = "transparent") +
      scale_fill_continuous(low="transparent", high="#00000080", na.value="transparent") +
      theme(title=element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill="transparent", colour=NA),  plot.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            strip.background = element_rect(fill="transparent", colour=NA), strip.text = element_text(color="white"))

    out <- cowplot::ggdraw() + cowplot::draw_plot(p3) + cowplot::draw_plot(p2) + cowplot::draw_plot(p1) + cowplot::draw_plot(pC)
  } else {
    out <- cowplot::ggdraw() + cowplot::draw_plot(p3) + cowplot::draw_plot(p2) + cowplot::draw_plot(p1)
  }

  return(out)
}
