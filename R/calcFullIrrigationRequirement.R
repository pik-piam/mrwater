#' @title       calcFullIrrigationRequirement
#' @description This function calculates the water requirements for full irrigation per cell per crop given potentially available land
#'
#' @param selectyears years to be returned
#' @param version     switch between LPJmL4 and LPJmL5
#' @param climatetype switch between different climate scenarios (default: "CRU_4")
#' @param time            time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param iniarea if TRUE (default): already irrigated area is subracted, if FALSE: total potential land area is used
#' @param iniyear  Year of initialization for cropland area
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("FullIrrigationRequirement", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass collapseNames getCells getSets new.magpie
#' @importFrom mrcommons toolCell2isoCell
#' @import mrmagpie

calcFullIrrigationRequirement <- function(version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", harmonize_baseline=FALSE, time="spline", dof=4, averaging_range=NULL, ref_year=NULL, selectyears=seq(1995,2095,by=5), iniyear=1995, iniarea=TRUE) {

  # read in irrigation water requirements [in m^3 per hectar per year] (smoothed & harmonized)
  irrig_wat <- calcOutput("IrrigWatRequirements", aggregate=FALSE, selectyears=selectyears, version=version, climatetype=climatetype, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof, averaging_range=averaging_range)
  # pasture is not irrigated in MAgPIE
  irrig_wat <- irrig_wat[,,"pasture",invert=T]
  irrig_wat <- toolCell2isoCell(irrig_wat)

  # read in land available for agricultural use (in mio. ha)
  land <- collapseNames(calcOutput("AvlLandSi", aggregate=FALSE, cells="lpjcell")[,,"si0"])

  if (iniarea) {
    # subtract area already reserved for irrigation by committed agricultural uses (in mio. ha)
    crops_grown    <- calcOutput("IrrigatedArea", selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)
    crops_grown    <- collapseNames(dimSums(crops_grown,dim=3))
    land           <- land - crops_grown
  }
  # negative values may occur because AvlLandSi is based on Ramankutty data and Croparea based on LUH -> set to 0
  land[land<0] <- 0

  # water requirements for full irrigation in cell per crop (in mio. m^3)
  # Note on unit transformation:
  # land (mio ha -> ha): multiply with 1e6,
  # irrigation water requirements (m^3 per ha -> mio. m^3 per ha): devide by 1e6
  # --> cancels out -> water requirements for full irrigation (mio. m^3)
  tmp <- irrig_wat * land

  # dimension names
  getSets(out)[c(1,2)] <- c("iso","cell")
  getSets(out)[c(4,5)] <- c("crop","system")

  # Checks
  if(any(is.na(out))){
    stop("produced NA full irrigation requirements")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="full irrigation requirements per cell per crop per irrigation system",
    isocountries=FALSE))
}
