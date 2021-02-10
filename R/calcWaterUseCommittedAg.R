#' @title calcWaterUseCommittedAg
#' @description This function calculates committed agricultural water uses that are used in the river routing algorithm for distributing available water across the basin
#'
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param selectyears Years to be returned
#' @param cells       Cells to be returned by the function (lpjcell or magpiecell)
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param time            Time smoothing: average or spline (default)
#' @param averaging_range Only specify if time=="average": number of time steps to average
#' @param dof             Only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization of input data to this function, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline of input data to this function (just specify when harmonize_baseline=TRUE)
#' @param iniyear  Year of initialization for cropland area
#' @param irrigini Initialization data set for irrigation system initialization ("Jaegermeyr_lpjcell", "LPJmL_lpjcell")
#'
#' @import magclass
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("WaterUseCommittedAg", aggregate = FALSE) }
#'

calcWaterUseCommittedAg <- function(version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", selectyears=seq(1995,2095,by=5), cells="lpjcell",
                                    time="spline", dof=4, averaging_range=NULL, harmonize_baseline="CRU_4", ref_year="y2015", iniyear=1995, irrigini="Jaegermeyr_lpjcell"){

  ##############################
  ######## Read in data ########
  ##############################
  ## Irrigation system area initialization
  irrigation_system <- calcOutput("IrrigationSystem", source=irrigini, aggregate=FALSE)

  ## Read in Irrigation Water Withdrawals (in m^3 per hectar per year) [smoothed and harmonized]
  irrig_withdrawal  <- calcOutput("IrrigWatRequirements", version="LPJmL5", cells="lpjcell", selectyears=selectyears, climatetype=climatetype, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof, irrig_requirement="withdrawal", aggregate=FALSE)
  # Pasture is not irrigated in MAgPIE
  irrig_withdrawal  <- irrig_withdrawal[,,"pasture",invert=T]

  ## Read in Irrigation Water Consumption (in m^3 per hectar per year) [smoothed and harmonized]
  irrig_consumption <- calcOutput("IrrigWatRequirements", version="LPJmL5", cells="lpjcell", selectyears=selectyears, climatetype=climatetype, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof, irrig_requirement="consumption", aggregate=FALSE)
  # Pasture is not irrigated in MAgPIE
  irrig_consumption <- irrig_consumption[,,"pasture",invert=T]

  ## Read in cropland area (by crop) from crop area initialization (in mio. ha)
  crops_grown    <- calcOutput("IrrigatedArea", selectyears=selectyears, iniyear=iniyear, cells="lpjcell", aggregate=FALSE)

  ##############################
  ######## Calculations ########
  ##############################
  ## Committed agricultural uses (in mio. m^3 per year) [in initialization year]
  # withdrawal
  CAW <- (irrigation_system[,,]*irrig_withdrawal[,,]) * crops_grown
  CAW <- dimSums(CAW,dim=3.1)
  getNames(CAW) <- paste(rep("withdrawal",3),getNames(CAW),sep=".")
  # consumption
  CAU <- (irrigation_system[,,]*irrig_consumption[,,]) * crops_grown
  CAU <- dimSums(CAU,dim=3.1)
  getNames(CAU) <- paste(rep("consumption",3),getNames(CAU),sep=".")
  # combine
  CAD <- mbind(CAW, CAU)
  names(dimnames(CAD))[1] <- "iso.cell"
  names(dimnames(CAD))[3] <- "wateruse.crop"

  ### Correct number of cells
  if (cells=="lpjcell"){
    out <- CAD
  } else if (cells=="magpiecell"){
    out <- CAD[magclassdata$cellbelongings$LPJ_input.Index,,]
    out <- toolCell2isoCell(out)
  } else {
    stop("Cell argument not supported. Select lpjcell for 67420 cells or magpiecell for 59199 cells")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3 per year",
    description="committed agricultural water demands",
    isocountries=FALSE))
}
