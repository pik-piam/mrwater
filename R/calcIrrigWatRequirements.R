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
#'                      ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                      separated by ":"
#'                      (e.g. TRUE:endogenous; FALSE:NULL)
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
#' @importFrom mrcommons toolCell2isoCell
#' @importFrom stringr str_split

calcIrrigWatRequirements <- function(selectyears, lpjml, climatetype,
                                     multicropping) {

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit = 1e+12)
  on.exit(options(magclass_sizeLimit = sizelimit))

  # Extract multiple cropping suitability mask
  suitability   <- str_split(multicropping, ":")[[1]][2]
  multicropping <- as.logical(str_split(multicropping, ":")[[1]][1])

  ##############################
  ######## Read in data ########
  ##############################
  ### Mappings
  lpj2mag <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral",
                            where = "mappingfolder")

  if (multicropping) {

    # Read in whole-year blue water consumption for irrigated crops (in m^3 per ha per yr):
    bwc <- calcOutput("BlueWaterConsumption", output = "crops:year",
                      suitability = suitability,
                      lpjml = lpjml, climatetype = climatetype,
                      selectyears = selectyears, aggregate = FALSE)

  } else {

    # Read in main-season blue water consumption for irrigated crops (in m^3 per ha per yr):
    bwc <- calcOutput("BlueWaterConsumption", output = "crops:main",
                      suitability = suitability,
                      lpjml = lpjml, climatetype = climatetype,
                      selectyears = selectyears, aggregate = FALSE)

  }

  years                   <- getItems(bwc, dim = "year")
  cropnames               <- getItems(bwc, dim = "crop")
  systemnames             <- c("drip", "sprinkler", "surface")

  ### Field efficiencies from Jägermeyr et al. (global values) [placeholder!]
  #### Use field efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####
  ### Alternatively: use regional efficiencies from Sauer et al. (2010), Table 5,
  fieldEff                <- add_dimension(new.magpie(cells_and_regions =  getCells(bwc),
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
  convEff                <- add_dimension(new.magpie(cells_and_regions =  getCells(bwc),
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
  watWW      <- bwc / projectEff

  # Conveyance loss (from river to field)
  convLoss   <- watWW * (1 - convEff)

  # consumptive irrigation water = consumptive plant transpiration + evaporative conveyance loss
  # (Note: According to Rost et al. (2008) 50% of conveyance loss are evaporative)
  # ["Half of conveyance losses are assumed to occur due to evaporation from open
  # water bodies and the remainder is added to the return flow as drainage." (Schaphoff 2018)]
  watWC      <- bwc + 0.5 * convLoss

  # Output: irrigation water requirements (consumption and withdrawals)
  irrigReq   <- new.magpie(cells_and_regions = getCells(watWC),
                           years = years,
                           names = getItems(watWC, dim = 3),
                           sets = c("x.y.iso", "year", "crop.system"))
  irrigReq <- add_dimension(irrigReq, dim = 3.4, add = "irrig_type",
                            nm = c("consumption", "withdrawal"))
  irrigReq[, , "consumption"] <- watWC
  irrigReq[, , "withdrawal"]  <- watWW

  # Aggregate to MAgPIE crops
  irrigReq  <- toolAggregate(irrigReq, lpj2mag, from = "LPJmL", to = "MAgPIE",
                             dim = "crop", partrel = TRUE)

  # Pasture is not irrigated in MAgPIE
  irrigReq <- irrigReq[, , "pasture", invert = TRUE]

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
              description  = "Irrigation water requirements for irrigation for
                              different crop types
                              under different irrigation systems",
              isocountries = FALSE))
}
