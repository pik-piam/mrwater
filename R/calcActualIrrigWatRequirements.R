#' @title       calcActualIrrigWatRequirements
#' @description This function calculates actual irrigation water requirements per cell given a certain irrigation system
#'
#' @param selectyears Years to be returned
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
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

calcActualIrrigWatRequirements <- function(selectyears="all", climatetype="GSWP3-W5E5:historical", iniyear=1995) {

  # irrigation water requirement per crop per system (in m^3 per ha per yr)
  irrig_wat_requirement        <- calcOutput("IrrigWatRequirements", aggregate=FALSE, selectyears=selectyears, climatetype=climatetype)
  irrig_wat_requirement        <- irrig_wat_requirement[,,"pasture",invert=T]
  getSets(irrig_wat_requirement, fulldim=F)[3] <- "crop.system"

  # irrigation system share (share of irrigated area)
  irrig_system_share           <- calcOutput("IrrigationSystem", source="Jaegermeyr", aggregate=FALSE)

  # composite mean
  mean_irrig_wat_requirement   <- dimSums(irrig_system_share * irrig_wat_requirement, dim=3.1) / dimSums(irrig_system_share, dim=3)

  # Correction of years
  if (selectyears!="all") {
    years                      <- sort(findset(selectyears, noset="original"))
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
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  map                            <- toolGetMappingCoord2Country()
  getCells(irrig_area)           <- paste(map$coords, map$iso, sep=".")
  names(dimnames(irrig_area))[1] <- "x.y.iso"
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  # Small additive term where irrigated area is 0
  irrig_area <- collapseNames(irrig_area[,,"irrigated"]) + 1e-9

  return(list(
    x=mean_irrig_wat_requirement,
    weight=irrig_area,
    unit="m^3 per ha per yr",
    description="Irrigation water requirements for irrigation for different crop types under selected irrigation system share per cell",
    isocountries=FALSE))
}
