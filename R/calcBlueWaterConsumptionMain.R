#' @title       calcBlueWaterConsumptionMain
#' @description This function calculates consumptive blue water consumption
#'              for the main growing period based on
#'              LPJmL transpiration, evaporation and interception for different
#'              irrigation systems (sprinkler, surface, drip)
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Climate model (e.g., "MRI-ESM2-0:ssp370")
#'                      or historical baseline (e.g., "GSWP3-W5E5:historical")
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("BlueWaterConsumptionMain", aggregate = FALSE)
#' }
#'
#' @importFrom magclass getItems collapseNames getSets add_dimension
#' @importFrom madrat calcOutput getFromComment

calcBlueWaterConsumptionMain <- function(selectyears, lpjml, climatetype) {

  ####################
  ### Read in data ###
  ####################
  .subtype <- function(x, runfolder) {
    out <- paste(runfolder, paste0("cft_", x), sep = ":")
    return(out)
  }
  # transpiration (in m^3/ha)
  transp <- calcOutput("LPJmLHarmonize", subtype = .subtype(x = "transp", runfolder = "cropsIR"),
                       lpjmlversion = lpjml, climatetype = climatetype,
                       monthly = FALSE,
                       aggregate = FALSE)[, selectyears, ]
  # evaporation (in m^3/ha)
  evap   <- calcOutput("LPJmLHarmonize", subtype = .subtype(x = "evap", runfolder = "cropsIR"),
                       lpjmlversion = lpjml, climatetype = climatetype,
                       monthly = FALSE,
                       aggregate = FALSE)[, selectyears, ]
  # interception (in m^3/ha)
  interc <- calcOutput("LPJmLHarmonize", subtype = .subtype(x = "interc", runfolder = "cropsIR"),
                       lpjmlversion = lpjml, climatetype = climatetype,
                       monthly = FALSE,
                       aggregate = FALSE)[, selectyears, ]

  # store unit for meta data
  unit <- getFromComment(transp, "unit")

  ####################
  ### Calculations ###
  ####################
  # Calculate additional evaporation, transpiration and interception due to irrigation
  # compared to rainfed counterfactual (in same growing season)
  transp <- collapseNames(transp[, , "irrigated"]) - collapseNames(transp[, , "rainfed"])
  evap <- collapseNames(evap[, , "irrigated"]) - collapseNames(evap[, , "rainfed"])
  interc <- collapseNames(interc[, , "irrigated"]) - collapseNames(interc[, , "rainfed"])

  # Calculate blue water consumption per system
  # Prepare object: blue water consumption of main growing period for three irrigation systems
  bwc1st <- new.magpie(cells_and_regions = getItems(transp, dim = 1),
                       years = getItems(transp, dim = 2),
                       names = getItems(transp, dim = 3))
  bwc1st <- add_dimension(bwc1st, dim = 3.1, add = "system",
                          nm = c("drip", "sprinkler", "surface"))
  getSets(bwc1st) <- c("x", "y", "iso", "year", "system", "crop")
  # Sprinkler system
  bwc1st[, , "sprinkler"] <- transp + evap + interc
  # Drip system
  # parameter as of LPJmL (drip_evap_reduction = 0.6):
  # 60% of blue water evaporation are saved in drip system
  bwc1st[, , "drip"] <- transp + 0.4 * evap
  # Surface system
  # no interception losses
  bwc1st[, , "surface"] <- transp + evap

  ##############
  ### Checks ###
  ##############
  if (any(is.na(bwc1st))) {
    stop("calcBlueWaterConsumptionMain produced NA irrigation water requirements")
  }
  # Remove negative consumptive irrigation water requirements in main season
  bwc1st[bwc1st < 0] <- 0

  return(list(x = bwc1st,
              weight = NULL,
              unit = unit,
              description = paste0("blue water consumption in the main irrigated growing period ",
                                   "for three different irrigation systems"),
              isocountries = FALSE))
}
