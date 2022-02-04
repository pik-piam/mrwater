#' @title       calcWaterUseCommittedAg
#' @description This function calculates committed agricultural water uses that
#'              are used in the river routing algorithm for distributing available water across the basin
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
#' \dontrun{
#' calcOutput("WaterUseCommittedAg", aggregate = FALSE)
#' }
#'
calcWaterUseCommittedAg <- function(lpjml, climatetype, selectyears, iniyear) {

  ##############################
  ######## Read in data ########
  ##############################
  ## Irrigation system area initialization
  irrigationSystem <- calcOutput("IrrigationSystem", datasource = "Jaegermeyr", aggregate = FALSE)

  ## Read in irrigation water requirements per crop (in m^3 per hectare per year) [smoothed and harmonized]
  irrigWater <- calcOutput("IrrigWatRequirements", aggregate = FALSE, lpjml = lpjml,
                           selectyears = selectyears, climatetype = climatetype)
  # Pasture is not irrigated in MAgPIE
  irrigWater <- irrigWater[, , "pasture", invert = TRUE]

  # Withdrawal requirements per crop
  irrigReqW  <- collapseNames(irrigWater[, , "withdrawal"])
  # Consumption requirements per crop
  irrigReqC  <- collapseNames(irrigWater[, , "consumption"])

  ## Read in cropland area (by crop) from crop area initialization (in mio. ha)
  grownCrops <- calcOutput("IrrigAreaCommitted", selectyears = selectyears,
                            iniyear = iniyear, aggregate = FALSE)

  ##############################
  ######## Calculations ########
  ##############################
  ## Committed agricultural uses (in mio. m^3 per year)
  # withdrawal
  comAgW           <- grownCrops * dimSums(irrigationSystem * irrigReqW, dim = "system")
  getNames(comAgW) <- paste(rep("withdrawal", 3), getNames(comAgW), sep = ".")
  # consumption
  comAgC           <- grownCrops * dimSums(irrigationSystem * irrigReqC, dim = "system")
  getNames(comAgC) <- paste(rep("consumption", 3), getNames(comAgC), sep = ".")
  # combine
  comAgUse                     <- mbind(comAgW, comAgC)
  names(dimnames(comAgUse))[3] <- "wateruse.crop"

  return(list(x            = comAgUse,
              weight       = NULL,
              unit         = "mio. m^3 per year",
              description  = "committed agricultural water demands per crop",
              isocountries = FALSE))
}
