#' @title       plotMapEconOfIrrig_old
#' @description plot of irrigatable area depending on costs paid for irrigation
#'
#' @param scenario         EFP and non-agricultural water use scenarios separate by "." (e.g. "on.ssp2")
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
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMapEconOfIrrig_old() }
#'
#' @importFrom luplot plotmap2
#' @importFrom magclass collapseNames
#'
#' @export

plotMapEconOfIrrig_old <- function(scenario, lpjml, selectyears, climatetype, EFRmethod, accessibilityrule, rankmethod, FAOyieldcalib, allocationrule, thresholdtype, irrigationsystem, avlland_scen, proxycrop, com_ag) {

  if (length(selectyears)>1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  avl_land <- calcOutput("AreaPotIrrig", selectyears=selectyears, comagyear=NULL, avlland_scen=avlland_scen, aggregate=FALSE)

  xC                 <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=0, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=FALSE, com_ag=com_ag, aggregate=FALSE)[,,"irrigatable"]) / avl_land
  xC[avl_land==0]    <- 0
  x0                 <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=0, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=TRUE, com_ag=com_ag, aggregate=FALSE)[,,"irrigatable"]) / avl_land
  x0[avl_land==0]    <- 0
  x100               <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=100, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=TRUE, com_ag=com_ag, aggregate=FALSE)[,,"irrigatable"]) / avl_land
  x100[avl_land==0]  <- 0
  x500               <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=500, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=TRUE, com_ag=com_ag, aggregate=FALSE)[,,"irrigatable"]) / avl_land
  x500[avl_land==0]  <- 0
  x1000              <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=1000, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=TRUE, com_ag=com_ag, aggregate=FALSE)[,,"irrigatable"]) / avl_land
  x1000[avl_land==0] <- 0

  x <- xC + x0 + x100 + x500 + x1000
  x <- x[,,scenario]

  out <- plotmap2(toolLPJarrayToMAgPIEmap(x), legend_discrete = TRUE, legend_breaks=c(0,0.99,1.99,2.99,3.99,4.99,5.99), highcol="darkblue", lowcol="grey", midcol="darkgreen")

  return(out)
}
