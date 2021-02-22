#' @title       calcActualIrrigWatRequirements
#' @description This function calculates actual irrigation water requirements per cell given a certain irrigation system
#'
#' @param selectyears Years to be returned
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param crops       Selects "magpie" (default) or "lpjml" crops
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param iniyear            Initialization year (for weight by cropland)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @seealso
#' \code{\link{calcIrrigationSystem}}, \code{\link{calcIrrigWatRequirements}}
#'
#' @examples
#' \dontrun{ calcOutput("ActualIrrigWatRequirements", aggregate=FALSE) }
#'
#' @importFrom magclass dimSums collapseNames
#' @importFrom madrat calcOutput
#' @importFrom magpiesets findset

calcActualIrrigWatRequirements <- function(selectyears="all", iniyear=1995, crops="magpie",
                                     version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="raw", averaging_range=NULL, dof=NULL, harmonize_baseline=FALSE, ref_year=NULL) {

  # irrigation water requirement per crop per system (in m^3 per ha per yr)
  irrig_wat_requirement        <- calcOutput("IrrigWatRequirements", aggregate=FALSE, crops=crops, selectyears=selectyears, version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
  irrig_wat_requirement        <- irrig_wat_requirement[,,"pasture",invert=T]
  names(dimnames(irrig_wat_requirement))[1] <- "iso.cell"
  names(dimnames(irrig_wat_requirement))[3] <- "crop.system"

  # irrigation system share (share of irrigated area)
  irrig_system_share           <- calcOutput("IrrigationSystem", source="Jaegermeyr_lpjcell", aggregate=FALSE)

  # composite mean
  mean_irrig_wat_requirement   <- dimSums(irrig_system_share * irrig_wat_requirement,dim=3.1) / dimSums(irrig_system_share, dim=3)

  # Correction of years
  if (selectyears!="all") {
    years                      <- sort(findset(selectyears,noset="original"))
    mean_irrig_wat_requirement <- mean_irrig_wat_requirement[,years,]
  }

  # Check for NAs and negative values
  if (any(is.na(mean_irrig_wat_requirement))) {
    stop("produced NA irrigation water requirements")
  }
  if (any(mean_irrig_wat_requirement<0)) {
    stop("produced negative irrigation water requirements")
  }

  # irrigated cropland area as weight
  irrig_area <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)
  irrig_area <- collapseNames(irrig_area[,,"irrigated"])+1e-9

  return(list(
    x=mean_irrig_wat_requirement,
    weight=irrig_area,
    unit="m^3 per ha per yr",
    description="Irrigation water requirements for irrigation for different crop types under selected irrigation system share per cell",
    isocountries=FALSE))
}
