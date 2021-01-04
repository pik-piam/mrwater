#' @title plotmapIrrigatableArea
#' @description This function plots the irrigatable area on a global map
#'
#' @param climatetype switch between different climate scenarios for yields
#' @param time            time smoothing: average, spline (default) or raw
#' @param averaging_range just specify for time=="average": number of time steps to average
#' @param dof             just specify for time=="spline": degrees of freedom
#' @param harmonize_baseline FALSE (default) no harmonization, harmonization: if a baseline is specified here data is harmonized to that baseline (from ref_year onwards)
#' @param ref_year           just specify for harmonize_baseline != FALSE : Reference year
#' @param selectyears Years to be returned
#' @param cells       cells used for calculation ("lpjcell" or "magpiecell")
#' @param output             Type of area to be returned: irrigatable_area (default, considers both water and land availability), irrig_area_ww or irrig_area_wc (area that can be irrigated given water available for withdrawal or consumption), avl_land
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param allocationshare Share of water to be allocated to cell (only needs to be selected in case of allocationrule=="equality")
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param irrigini         When "initialization" selected for irrigation system: choose initialization data set for irrigation system initialization ("Jaegermeyr_lpjcell", "LPJmL_lpjcell")
#' @param iniyear          Initialization year of irrigation system
#' @param proxycrops  proxycrops used for cell ranking
#' @param landtype     current cropland area (currentcropland) or potential cropland area (potentialcropland)
#' @param EFP         switch for environmental flow policy ("on" or "off")
#' @param scen        non-agricultural water demand scenario ("ssp1", "ssp2", "ssp3")
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @importFrom luplot plotmap2
#'
#' @examples
#' \dontrun{ plotmapIrrigatableArea() }

plotmapIrrigatableArea <- function(selectyears=1995, cells="lpjcell", output="irrigatable_area",
                                 climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015",
                                 allocationrule="optimization", allocationshare=NULL, gainthreshold=1, irrigationsystem="initialization", irrigini="Jaegermeyr_lpjcell", iniyear=1995,
                                 landtype="potentialcropland", proxycrops="maiz", EFP="on", scen="ssp2"){

  # read in area to be plotted
  x <- calcOutput("IrrigatableArea", aggregate=FALSE, selectyears=selectyears, cells=cells, output=output,
                      climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year,
                      allocationrule=allocationrule, allocationshare=allocationshare, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, irrigini=irrigini, iniyear=iniyear,
                      landtype=landtype, proxycrops=proxycrops)
  x <- collapseNames(x[,,paste(EFP,scen,sep=".")])

  # Cropland area
  c_area <- calcOutput("IrrigatableArea", output="avl_land", aggregate=FALSE, selectyears=selectyears, cells=cells,
                       climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year,
                       allocationrule=allocationrule, allocationshare=allocationshare, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, irrigini=irrigini, iniyear=iniyear,
                       landtype=landtype, proxycrops=proxycrops)
  c_area <- collapseNames(c_area[,,paste(EFP,scen,sep=".")])

  # Correct number of cells
  if (length(getCells(x))==67420){
    x <- x[magclassdata$cellbelongings$LPJ_input.Index,,]
    x <- toolCell2isoCell(x)
    c_area <- c_area[magclassdata$cellbelongings$LPJ_input.Index,,]
    c_area <- toolCell2isoCell(c_area)
  } else if (length(getCells(x))==59199){
    x      <- x
    c_area <- c_area
  } else {
    stop("Please provide object in cellular resolution for map")
  }

  # Area share
  area_shr           <- new.magpie(getCells(x), getYears(x), getNames(x), fill=0)
  area_shr[c_area>0] <- x[c_area>0] / c_area[c_area>0]

  # Plot
  out <- plotmap2(log(area_shr))

  return(out)
}
