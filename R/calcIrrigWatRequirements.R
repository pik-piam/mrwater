#' @title       calcIrrigWatRequirements
#' @description This function calculates irrigation water requirements based on
#'              LPJmL blue water consumption of plants and considering irrigation efficiencies
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Climate model or historical baseline "GSWP3-W5E5:historical"
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigWatRequirements", aggregate = FALSE)
#' }
#'
#' @importFrom magclass collapseNames collapseDim getYears getCells getNames new.magpie add_dimension
#' @importFrom madrat calcOutput toolAggregate toolGetMapping
#' @importFrom mrcommons toolCell2isoCell

calcIrrigWatRequirements <- function(selectyears, lpjml, climatetype, multicropping) {

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit = 1e+12)
  on.exit(options(magclass_sizeLimit = sizelimit))

  ##############################
  ######## Read in data ########
  ##############################
  ### Mappings
  lpj2mag <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")

  ### Read in blue water consumption for irrigated crops (in m^3 per ha per yr):
  bwc <- collapseNames(setYears(calcOutput("LPJmL_new", subtype = "cwater_b",
                                            version = lpjml["crop"], climatetype = climatetype, stage = "smoothed",
                                            aggregate = FALSE, years = selectyears),
                                selectyears))
  names(dimnames(bwc))[3] <- "crop"
  years                   <- getYears(bwc)
  cropnames               <- getNames(bwc)
  systemnames             <- c("drip", "sprinkler", "surface")

  # Seasonality dimension
  bwc   <- add_dimension(bwc, dim = 3.2, add = "season", nm = c("first", "second"))

  if (multicropping) {

    # Reduce to areas where multicropping is relevant based on Multiple Cropping Zones
    mc <- calcOutput("MultipleCroppingZones", layers = 2, aggregate = FALSE)
    mc <- collapseNames(mc[, , "irrigated"])

    # Irrigation water requirements of main-season ("first") and off-season ("second"):
    ratio <- calcOutput("MultipleCroppingWatRatio", selectyears = selectyears,
                        lpjml = lpjml, climatetype = climatetype, aggregate = FALSE)
    bwc[, , "second"] <- bwc[, , "second"] * ratio * mc

  } else {

    bwc[, , "second"] <- 0

  }

  ### Field efficiencies from Jägermeyr et al. (global values) [placeholder!]
  #### Use field efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####
  ### Alternatively: use regional efficiencies from Sauer et al. (2010), Table 5,
  fieldEff                <- new.magpie(cells_and_regions =  getCells(bwc),
                                        years = years,
                                        names = sort(paste(systemnames, rep(cropnames, 3), sep = ".")),
                                        sets = c("x.y.iso", "year", "system.crop"))
  fieldEff[, , "drip"]      <- 0.88 # Sauer: 0.8-0.93
  fieldEff[, , "sprinkler"] <- 0.78 # Sauer: 0.6-0.86
  fieldEff[, , "surface"]   <- 0.52 # Sauer: 0.25-0.5
  #### Use field efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####

  ### Conveyance efficiency proxy [placeholder]
  #### Use conveyance efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####
  convEff                <- new.magpie(cells_and_regions = getCells(bwc),
                                       years = years,
                                       names = sort(paste(systemnames, rep(cropnames, 3), sep = ".")),
                                       sets = c("x.y.iso", "year", "system.crop"))
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
                           years = getYears(watWC),
                           names = getNames(watWC),
                           sets = c("x.y.iso", "year", "crop.season.system"))
  irrigReq <- add_dimension(irrigReq, dim = 3.4, add = "irrig_type", nm = c("consumption", "withdrawal"))
  irrigReq[, , "consumption"] <- watWC
  irrigReq[, , "withdrawal"]  <- watWW

  # Aggregate to MAgPIE crops
  irrigReq  <- toolAggregate(irrigReq, lpj2mag, from = "LPJmL", to = "MAgPIE",
                             dim = "crop", partrel = TRUE)

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
                              different crop types in different seasons
                              under different irrigation systems",
              isocountries = FALSE))
}
