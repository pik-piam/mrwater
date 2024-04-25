#' @title       calcIrrigWatRequirements
#' @description This function calculates irrigation water requirements based on
#'              LPJmL blue water consumption of crops and
#'              considering irrigation efficiencies
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Climate model or historical baseline "GSWP3-W5E5:historical"
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from LandInG
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigWatRequirements", aggregate = FALSE)
#' }
#'
#' @importFrom magclass getItems new.magpie add_dimension
#' @importFrom madrat calcOutput toolAggregate toolGetMapping
#' @importFrom mstools toolCell2isoCell
#' @importFrom stringr str_split
#' @importFrom withr local_options

calcIrrigWatRequirements <- function(selectyears, lpjml, climatetype,
                                     multicropping) {
  # Set size limit
  local_options(magclass_sizeLimit = 1e+12)

  # Extract multiple cropping suitability mask
  areaMask  <- paste(str_split(multicropping, ":")[[1]][2],
                     str_split(multicropping, ":")[[1]][3],
                     sep = ":")
  mcBoolean <- as.logical(str_split(multicropping, ":")[[1]][1])

  # Read in blue water consumption (in m^3 per ha per yr):
  if (mcBoolean) {
    # For multiple cropping case: whole year where suitable
    bwc <- calcOutput("BlueWaterConsumption", output = "crops:year",
                      areaMask = areaMask,
                      lpjml = lpjml, climatetype = climatetype,
                      selectyears = selectyears, aggregate = FALSE)

  } else {
    # For single cropping case: main season blue water consumption
    # (Note: areaMask argument not relevant here, but needs to be set)
    # To Do: as soon as code review complete, set default in calcBlueWaterConsumption
    bwc <- calcOutput("BlueWaterConsumption", output = "crops:main",
                      areaMask = "potential:endogenous",
                      lpjml = lpjml, climatetype = climatetype,
                      selectyears = selectyears, aggregate = FALSE)
  }

  years       <- getItems(bwc, dim = "year")
  cropnames   <- getItems(bwc, dim = "crop")
  systemnames <- c("drip", "sprinkler", "surface")

  ### Field efficiencies from Jägermeyr et al. (global values) [placeholder!]
  #### Use field efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####
  ### Alternatively: use regional efficiencies from Sauer et al. (2010), Table 5,
  fieldEff <- add_dimension(new.magpie(cells_and_regions =  getCells(bwc),
                                       years = years,
                                       names = cropnames,
                                       sets = c("x.y.iso", "year", "crop")),
                            dim = 3.1, add = "system", nm = systemnames)
  fieldEff[, , "drip"]      <- 0.88 # Sauer: 0.8-0.93
  fieldEff[, , "sprinkler"] <- 0.78 # Sauer: 0.6-0.86
  fieldEff[, , "surface"]   <- 0.52 # Sauer: 0.25-0.5
  #### Use field efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####

  ### Conveyance efficiency proxy [placeholder]
  #### Use conveyance efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####
  convEff <- add_dimension(new.magpie(cells_and_regions =  getCells(bwc),
                                      years = years,
                                      names = cropnames,
                                      sets = c("x.y.iso", "year", "crop")),
                           dim = 3.1, add = "system", nm = systemnames)
  convEff[, , "drip"]      <- 0.95 # Note: same as in LPJmL (see Schaphoff 2018 p. 1395)
  convEff[, , "sprinkler"] <- 0.95 # Note: same as in LPJmL (see Schaphoff 2018 p. 1395)
  convEff[, , "surface"]   <- 0.7
  #### Use conveyance efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####

  ##############################
  ######## Calculations ########
  ##############################

  # Calculate project efficiency from given field and conveyance efficiencies
  projectEff <- fieldEff * convEff

  # Water withdrawal = crop water consumption + field losses + conveyance losses
  watWW <- bwc / projectEff

  # Conveyance loss (from river to field)
  convLoss <- watWW * (1 - convEff)

  # consumptive irrigation water = consumptive plant transpiration + evaporative conveyance loss
  # (Note: According to Rost et al. (2008) 50% of conveyance loss are evaporative)
  # ["Half of conveyance losses are assumed to occur due to evaporation from open
  # water bodies and the remainder is added to the return flow as drainage." (Schaphoff 2018)]
  watWC <- bwc + 0.5 * convLoss

  # Output: irrigation water requirements (consumption and withdrawals)
  irrigReq <- new.magpie(cells_and_regions = getCells(watWC),
                         years = years,
                         names = getItems(watWC, dim = 3),
                         sets = c("x.y.iso", "year", "crop.system"))
  irrigReq <- add_dimension(irrigReq, dim = 3.4, add = "irrig_type",
                            nm = c("consumption", "withdrawal"))
  irrigReq[, , "consumption"] <- watWC
  irrigReq[, , "withdrawal"]  <- watWW

  # Check for NAs and negative values
  if (any(is.na(irrigReq))) {
    stop("produced NA irrigation water requirements")
  }
  if (any(irrigReq < 0)) {
    stop("produced negative irrigation water requirements")
  }

  return(list(x            = irrigReq,
              weight       = NULL,
              unit         = "m^3 per ha per yr",
              description  = paste0("Irrigation water requirements for irrigation for ",
                                    "different crop types ",
                                    "under different irrigation systems"),
              isocountries = FALSE))
}
