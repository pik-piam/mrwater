#' @title       calcWaterUseCommittedAg
#' @description This function calculates committed agricultural water uses that are used in the river routing algorithm for distributing available water across the basin
#'
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop
#' @param selectyears Years to be returned
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear     Year of initialization for cropland area
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

calcWaterUseCommittedAg <- function(lpjml, climatetype, selectyears, iniyear) {

  ##############################
  ######## Read in data ########
  ##############################
  ## Irrigation system area initialization
  irrigation_system <- calcOutput("IrrigationSystem", source="Jaegermeyr", aggregate=FALSE)

  ## Read in irrigation water requirements per crop (in m^3 per hectare per year) [smoothed and harmonized]
  irrig_water <- calcOutput("IrrigWatRequirements", aggregate=FALSE, lpjml=lpjml, selectyears=selectyears, climatetype=climatetype)
  # Pasture is not irrigated in MAgPIE
  irrig_water <- irrig_water[,,"pasture",invert=T]

  # Withdrawal requirements per crop
  irrig_withdrawal   <- collapseNames(irrig_water[,,"withdrawal"])
  # Consumption requirements per crop
  irrig_consumption  <- collapseNames(irrig_water[,,"consumption"])

  ## Read in cropland area (by crop) from crop area initialization (in mio. ha)
  crops_grown        <- calcOutput("IrrigAreaCommitted", selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)

  ##############################
  ######## Calculations ########
  ##############################
  ## Committed agricultural uses (in mio. m^3 per year)
  # withdrawal
  CAW           <- crops_grown * dimSums(irrigation_system * irrig_withdrawal, dim="system")
  getNames(CAW) <- paste(rep("withdrawal",3), getNames(CAW), sep=".")
  # consumption
  CAU           <- crops_grown * dimSums(irrigation_system * irrig_consumption, dim="system")
  getNames(CAU) <- paste(rep("consumption",3), getNames(CAU), sep=".")
  # combine
  CAD                     <- mbind(CAW, CAU)
  names(dimnames(CAD))[3] <- "wateruse.crop"

  return(list(
    x=CAD,
    weight=NULL,
    unit="mio. m^3 per year",
    description="committed agricultural water demands per crop",
    isocountries=FALSE))
}
