#' @title       calcIrrigatableArea
#' @description calculates area that can potentially be irrigated given available water and land
#'
#' @param selectyears  years for which irrigatable area is calculated
#' @param cells        cells to be returned by the function (lpjcell or magpiecell)
#' @param landtype     current cropland area (currentcropland) or potential cropland area (potentialcropland) or nonprotected (restriction of withdrawals in protected areas)
#' @param protectscen  protection scenario for protected areas ("WDPA", "HalfEarth", )
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param time               Time smoothing: average, spline or raw (default)
#' @param averaging_range    only specify if time=="average": number of time steps to average
#' @param dof                only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param allocationrule     Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param allocationshare    Share of water to be allocated to cell (only needs to be selected in case of allocationrule=="equality")
#' @param gainthreshold      Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem   Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniarea          if TRUE: already irrigated area is subtracted, if FALSE: total potential land area is used from potentially available irrigation land
#' @param iniyear          year of initialization for cropland area initialization and irrigation systems
#' @param protect_scen     land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection
#' @param proxycrop   proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#' @param output             Type of area to be returned: irrigatable_area (default, considers both water and land availability), irrig_area_ww or irrig_area_wc (area that can be irrigated given water available for withdrawal or consumption), avl_land, protect_area (restricts water withdrawal in protected areas)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigatableArea", aggregate=FALSE) }
#'
#' @import magclass
#' @import magpiesets

calcIrrigatableArea <- function(selectyears=1995, cells="lpjcell", output="irrigatable_area", iniarea, protect_scen,
                                climatetype="GSWP3-W5E5:historical", time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015",
                                allocationrule="optimization", allocationshare=NULL, gainthreshold=1, irrigationsystem="initialization", iniyear=1995,
                                landtype="potentialcropland", protectscen="WDPA", proxycrop="maiz"){

  ### Irrigatable area = MIN (area that can be irrigated given water resources; available suitable land area)
  ## Area that can be irrigated given water available for withdrawals (in ha)
  # read in water available for withdrawal (in mio. m^3)
  avl_wat_ww <- calcOutput("WaterAllocation", output="withdrawal", finalcells=cells,
                           selectyears=seq(1995,2100,by=5), climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year,
                           allocationrule=allocationrule, allocationshare=allocationshare, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, aggregate=FALSE)
  avl_wat_ww <- avl_wat_ww[,selectyears,]
  # transform from mio. m^3 to m^3
  avl_wat_ww <- avl_wat_ww*1e6
  # read in water withdrawal required for irrigation of proxy crop(s) (in m^3 per ha)
  wat_req_ww <- calcOutput("ActualIrrigWatRequirements", selectyears=selectyears, climatetype=climatetype, iniyear=iniyear, aggregate=FALSE)[,,"withdrawal"]
  wat_req_ww <- wat_req_ww[,,proxycrop]
  #### normalization / (weighted) average of proxycrop (??????)
  wat_req_ww <- dimSums(wat_req_ww,dim=3)/length(getNames(wat_req_ww))

  # calculate area that could be irrigated given water available for withdrawal (ha)
  irrig_area_ww               <- new.magpie(getCells(avl_wat_ww), getYears(avl_wat_ww), getNames(avl_wat_ww), fill=0)
  irrig_area_ww[wat_req_ww>0] <- avl_wat_ww[wat_req_ww>0]/wat_req_ww[wat_req_ww>0]
  irrig_area_ww               <- collapseNames(irrig_area_ww)

  ## Area that can be irrigated given water available for consumption (in ha)
  # read in water available for consumption (in mio. m^3)
  avl_wat_wc <- calcOutput("WaterAllocation", output="consumption", finalcells=cells,
                           selectyears=seq(1995,2100,by=5), climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year,
                           allocationrule=allocationrule, allocationshare=allocationshare, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, aggregate=FALSE)
  avl_wat_wc <- avl_wat_wc[,selectyears,]
  # transform from mio. m^3 to m^3
  avl_wat_wc <- avl_wat_wc*1e6
  # read in water withdrawal required for irrigation of proxy crop(s) (in m^3 per ha)
  wat_req_wc <- calcOutput("ActualIrrigWatRequirements", selectyears=selectyears, climatetype=climatetype, iniyear=iniyear, aggregate=FALSE)[,,"consumption"]
  wat_req_wc <- wat_req_wc[,,proxycrop]
  #### normalization / (weighted) average of proxycrop (??????)
  wat_req_wc <- dimSums(wat_req_wc,dim=3)/length(getNames(wat_req_wc))

  # calculate area that could be irrigated given water available for withdrawal (ha)
  irrig_area_wc               <- new.magpie(getCells(avl_wat_wc), getYears(avl_wat_wc), getNames(avl_wat_wc), fill=0)
  irrig_area_wc[wat_req_wc>0] <- avl_wat_wc[wat_req_wc>0]/wat_req_ww[wat_req_wc>0]
  irrig_area_wc               <- collapseNames(irrig_area_wc)

  ## Area available and suitable for cropland
  avl_land <- calcOutput("AreaPotIrrig", selectyears=selectyears, iniareayear=iniyear, protect_scen=protect_scen, aggregate=FALSE)


  # irrigatable area (area that can be irrigated): enough local water available & enough local land available
  irrigatable_area <- pmin(avl_land, irrig_area_wc, irrig_area_ww)

  if (output=="irrigatable_area") {
    out <- irrigatable_area
  } else if (output=="irrig_area_ww") {
    out <- irrig_area_ww
  } else if (output=="irrig_area_wc") {
    out <- irrig_area_wc
  } else if (output=="avl_land") {
    out <- avl_land
  } else {
    stop("Please select which area type should be returned: irrigatable_area includes both the water and the land constraint.")
  }

  # convert to mio. ha
  out <- out*1e-6

  # Corrections
  # years
  if(selectyears!="all"){
    years <- sort(findset(selectyears,noset="original"))
    out   <- out[,years,]
  }

  # check for NAs and negative values
  if(any(is.na(out))){
    stop("produced NA irrigation water requirements")
  }
  if(any(out<0)){
    stop("produced negative irrigation water requirements")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. ha",
    description="Area that can be irrigated given land and water constraint",
    isocountries=FALSE))
}
