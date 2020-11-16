#' @title calcActualIrrigWatRequirements
#' @description This function calculates actual irrigation water requirements per cell given a certain irrigation system
#'
#' @param selectyears Years to be returned
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param cells       Switch between "lpjcell" (67420) and "magpiecell" (59199)
#' @param crops       Selects "magpie" (default) or "lpjml" crops
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param irrig_requirement  Consumptive (consumption) or non-consumptive (withdrawals) irrigation water requirements
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("ActualIrrigWatRequirements", aggregate=FALSE) }
#'
#' @import magpiesets
#' @import magclass
#' @import madrat

### NOTE: not yet implemented

calcActualIrrigWatRequirements <- function(selectyears="all", cells="lpjcell", crops="magpie",
                                     version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="raw", averaging_range=NULL, dof=NULL,
                                     harmonize_baseline=FALSE, ref_year=NULL, irrig_requirement="withdrawal"){

  # irrigation water requirement per crop per system (in m^3 per ha per yr)
  irrig_wat_requirement <- calcOutput("IrrigWatRequirements", selectyears=selectyears, cells=cells, crops=crops, version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof,
                                      harmonize_baseline=harmonize_baseline, ref_year=ref_year, irrig_requirement=irrig_requirement, aggregate=FALSE)
  names(dimnames(irrig_wat_requirement))[3] <- "iso.cell"
  names(dimnames(irrig_wat_requirement))[3] <- "crop.system"

  # irrigation system share (share of irrigated area)
  #irrig_system_share    <- calcOutput("IrrigationSystem",source="Jaegermeyr_magpiecell",aggregate=FALSE)

  # irrigated area

  #
  #x <- irrig_wat_requirement * irrig_system_share
  x <- irrig_wat_requirement


  if(selectyears!="all"){
    years <- sort(findset(selectyears,noset="original"))
    x     <- x[,years,]
  }

  # Check for NAs and negative values
  if(any(is.na(out))){
    stop("produced NA irrigation water requirements")
  }
  if(any(out<0)){
    stop("produced negative irrigation water requirements")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="m^3 per ha per yr",
    description="Irrigation water requirements for irrigation for different crop types under selected irrigation system scenario",
    isocountries=FALSE))
}
