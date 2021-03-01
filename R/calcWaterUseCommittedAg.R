#' @title       calcWaterUseCommittedAg
#' @description This function calculates committed agricultural water uses that are used in the river routing algorithm for distributing available water across the basin
#'
#' @param selectyears Years to be returned
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param time            Time smoothing: average or spline (default)
#' @param averaging_range Only specify if time=="average": number of time steps to average
#' @param dof             Only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization of input data to this function, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline of input data to this function (just specify when harmonize_baseline=TRUE)
#' @param iniyear  Year of initialization for cropland area
#'
#' @importFrom magclass collapseNames dimSums getNames mbind
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("WaterUseCommittedAg", aggregate = FALSE) }
#'

calcWaterUseCommittedAg <- function(climatetype="HadGEM2_ES:rcp2p6:co2", selectyears=seq(1995,2095,by=5),
                                    time="spline", dof=4, averaging_range=NULL, harmonize_baseline="CRU_4", ref_year="y2015", iniyear=1995) {

  ##############################
  ######## Read in data ########
  ##############################
  ## Irrigation system area initialization
  irrigation_system <- calcOutput("IrrigationSystem", source="Jaegermeyr_lpjcell", aggregate=FALSE)

  ## Read in Irrigation Water (in m^3 per hectar per year) [smoothed and harmonized]
  irrig_water <- calcOutput("IrrigWatRequirements", aggregate=FALSE, selectyears=selectyears, climatetype=climatetype, time=time, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
  # Pasture is not irrigated in MAgPIE
  irrig_water  <- irrig_water[,,"pasture",invert=T]

  # Withdrawal
  irrig_withdrawal  <- collapseNames(irrig_water[,,"withdrawal"])
  # Consumption
  irrig_consumption  <- collapseNames(irrig_water[,,"consumption"])

  ## Read in cropland area (by crop) from crop area initialization (in mio. ha)
  crops_grown       <- calcOutput("IrrigatedArea", selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)

  ##############################
  ######## Calculations ########
  ##############################
  ## Committed agricultural uses (in mio. m^3 per year) [in initialization year]
  # withdrawal
  CAW <- (irrigation_system[,,] * irrig_withdrawal[,,]) * crops_grown
  CAW <- dimSums(CAW,dim=3.1)
  getNames(CAW) <- paste(rep("withdrawal",3), getNames(CAW), sep=".")
  # consumption
  CAU <- (irrigation_system[,,] * irrig_consumption[,,]) * crops_grown
  CAU <- dimSums(CAU,dim=3.1)
  getNames(CAU) <- paste(rep("consumption",3), getNames(CAU), sep=".")
  # combine
  CAD <- mbind(CAW, CAU)
  names(dimnames(CAD))[1] <- "iso.cell"
  names(dimnames(CAD))[3] <- "wateruse.crop"

  return(list(
    x=CAD,
    weight=NULL,
    unit="mio. m^3 per year",
    description="committed agricultural water demands",
    isocountries=FALSE))
}
