#' @title       plotEconOfIrrig
#' @description plot of irrigatable area depending on costs paid for irrigation
#'
#' @param x_axis_range     range of x-axis (gainthreshold) to be depicted on the curve
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param FAOyieldcalib    TRUE (LPJmL yields scaled with current FAO yield) or FALSE (LPJmL yield potentials)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param proxycrop        proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotEconOfIrrig(x_axis_range=seq(0, 10000, by=100), scenario="ssp2") }
#'
#' @importFrom ggplot2 ggplot geom_line geom_point aes_string ggtitle xlab ylab theme_bw
#'
#' @export

plotEconOfIrrig <- function(x_axis_range, scenario, lpjml, selectyears, climatetype, EFRmethod, accessibilityrule, rankmethod, FAOyieldcalib, allocationrule, thresholdtype, irrigationsystem, avlland_scen, proxycrop, potential_wat=TRUE, com_ag) {

  df  <- reportEconOfIrrig(GT_range=x_axis_range, scenario=scenario, lpjml=lpjml, selectyears=selectyears, climatetype=climatetype,
                           EFRmethod=EFRmethod, accessibilityrule=accessibilityrule, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib,
                           allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen,
                           proxycrop=proxycrop, potential_wat=TRUE, com_ag=com_ag)


  out <- ggplot(data=df, aes_string(x="GT")) +
                geom_line(aes_string(y=paste("IrrigArea", "on", scenario, sep=".")),  color="darkblue")                    + geom_point(aes_string(y=paste("IrrigArea", "on", scenario, sep="."))) +
                geom_line(aes_string(y=paste("IrrigArea", "off", scenario, sep=".")), color="darkred", linetype="twodash") + geom_point(aes_string(y=paste("IrrigArea", "off", scenario, sep="."))) +
                theme_bw() +
                ggtitle(paste0("Irrigatable Area for FAOyieldcalib = ", FAOyieldcalib, " on ", avlland_scen)) + xlab("Irrigation Costs (USD/ha)") + ylab("Irrigatable Area (Mha)")

  return(out)
}
