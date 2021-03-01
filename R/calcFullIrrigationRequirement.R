#' @title       calcFullIrrigationRequirement
#' @description This function calculates the water requirements for full irrigation per cell per crop given potentially available land
#'
#' @param selectyears years to be returned
#' @param climatetype switch between different climate scenarios (default: "CRU_4")
#' @param time            time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param iniarea          if TRUE (default): already irrigated area is subtracted, if FALSE: total potential land area is used
#' @param iniyear          year of initialization for cropland area
#' @param irrigationsystem irrigation system used: system share as in initialization year (default) or drip, surface, sprinkler for full irrigation by selected system
#' @param protect_scen     land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection
#' @param proxycrop        historical crop mix pattern ("historical") or list of proxycrop(s)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("FullIrrigationRequirement", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass collapseNames getCells getSets getYears getNames new.magpie dimSums
#' @importFrom mrcommons toolCell2isoCell

calcFullIrrigationRequirement <- function(climatetype="HadGEM2_ES:rcp2p6:co2", harmonize_baseline=FALSE, time="spline", dof=4, averaging_range=NULL, ref_year=NULL, selectyears=seq(1995,2095,by=5), iniyear=1995, iniarea=TRUE, irrigationsystem="initialization", protect_scen, proxycrop) {

  # read in irrigation water requirements for each irrigation system [in m^3 per hectare per year] (smoothed & harmonized)
  irrig_wat <- calcOutput("IrrigWatRequirements", aggregate=FALSE, selectyears=selectyears, climatetype=climatetype, time=time, dof=dof, averaging_range=averaging_range, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
  # pasture is not irrigated in MAgPIE
  irrig_wat <- irrig_wat[,,"pasture",invert=T]

  # land area that can potentially be used for irrigated agriculture given assumptions set in the arguments [in Mha]
  land <- calcOutput("AreaPotIrrig", selectyears=selectyears, iniareayear=iniyear, protect_scen=protect_scen, aggregate=FALSE)

  # share of corp area by crop type
  if (proxycrop=="historical") {
    # historical crop mix
    # read in total (irrigated + rainfed) croparea
    croparea <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=FALSE, aggregate=FALSE)
    rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater")) #### Note: only until calcCroparea is adjusted to 67420 cells
    getCells(croparea) <- rs$coordinates                                                  #### Note: only until calcCroparea is adjusted to 67420 cells
    # historical share of crop types in total cropland per cell
    croparea_shr <- croparea / dimSums(croparea, dim=3)
    # correct NAs: where no land available -> crop share 0
    croparea_shr[dimSums(croparea, dim=3)==0] <- 0
  } else {
    # equal crop area share for each proxycrop assumed
    croparea_shr              <- new.magpie(cells_and_regions = getCells(land), years = NULL, names = proxycrop)
    croparea_shr[,,proxycrop] <- 1 / length(proxycrop)
  }

  # land area per crop
  land <- land * croparea_shr

  # water requirements for full irrigation in cell per crop accounting for cropshare (in mio. m^3)
  # Note on unit transformation:
  # land (mio ha -> ha): multiply with 1e6,
  # irrigation water requirements (m^3 per ha -> mio. m^3 per ha): divide by 1e6
  # --> cancels out -> water requirements for full irrigation (mio. m^3)
  irrig_wat <- irrig_wat[,,getNames(croparea_shr)]
  irrig_wat <- irrig_wat * land

  # sum over crops
  irrig_wat <- dimSums(irrig_wat, dim="crop")

  # calculate irrigation water requirements per crop [in mio. m^3 per year] given irrigation system share in use
  if (irrigationsystem=="initialization") {
    # read in irrigation system area initialization [share of AEI by system] and expand to all years
    tmp                   <- calcOutput("IrrigationSystem", source="Jaegermeyr_lpjcell", aggregate=FALSE)
    irrigation_system     <- new.magpie(getCells(irrig_wat), getYears(irrig_wat), getNames(tmp))
    irrigation_system[,,] <- tmp

    # every crop irrigated by same share of initialization irrigation system
    irrig_wat <- dimSums(irrig_wat * irrigation_system, dim="system")

  } else {
    # whole area irrigated by one system as selected in argument "irrigationsystem"
    irrig_wat <- collapseNames(irrig_wat[,,irrigationsystem])
  }

  # Checks
  if (any(is.na(irrig_wat))) {
    stop("produced NA full irrigation requirements")
  }

  return(list(
    x=irrig_wat,
    weight=NULL,
    unit="mio. m^3",
    description="full irrigation requirements per cell per crop per irrigation system",
    isocountries=FALSE))
}
