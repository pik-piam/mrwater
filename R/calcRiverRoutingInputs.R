#' @title       calcRiverRoutingInputs
#' @description This function collects inputs necessary for
#'              the different river routings
#'              depending on the chosen settings
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years to be returned
#'                          (Note: does not affect years of harmonization or smoothing)
#' @param iteration         River routing iteration
#'                          (non_agriculture, committed_agriculture,
#'                          potential_irrigation).
#' @param climatetype       Switch between different climate scenarios
#'                          or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear           Initialization year of irrigation system
#' @param efrMethod         EFR method used including selected strictness of EFRs
#'                          (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Water accessibility rule.
#'                          Available methods: Quantile method (Q) or Coefficient of Variation (CV);
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                          "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                          separated by ":"
#'                          (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#' @param comAg             if TRUE: the currently already irrigated areas
#'                               in initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential).
#'                          Only relevant for iteration = potential
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          USD_ha (USD per hectare) for relative area return, or
#'                          USD_m3 (USD per cubic meter) for relative volumetric return;
#'                          USD for absolute return (total profit);
#'                          USD_m3ha (USD per hectare per cubic meter)
#'                          for relative return according to area and volume.
#'                          Price aggregation:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices
#'                          and boolean indicating fullpotential (TRUE, i.e. cell
#'                          receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                          receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param gainthreshold     Threshold of yield improvement potential
#'                          (same unit as in rankmethod argument)
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param irrigationsystem  Irrigation system to be used for river basin discharge
#'                          allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getItems new.magpie
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverRoutingInputs", aggregate = FALSE)
#' }
#' @export

calcRiverRoutingInputs <- function(lpjml, climatetype,
                                   selectyears, iniyear,
                                   iteration, transDist, efrMethod, accessibilityrule,
                                   multicropping, comAg, rankmethod, gainthreshold,
                                   cropmix, yieldcalib, irrigationsystem, landScen) {

  # Object dimensions
  watNonAg  <- calcOutput("WaterUseNonAg",
                          selectyears = selectyears, cells = "lpjcell",
                          datasource = "WATERGAP_ISIMIP", usetype = "total",
                          seasonality = "total", harmonType = "average",
                          lpjml = NULL, climatetype = NULL, aggregate = FALSE)
  nonAgWW   <- collapseNames(watNonAg[, , "withdrawal"])
  nonAgWC   <- collapseNames(watNonAg[, , "consumption"])
  scenarios <- c(paste("on", getItems(nonAgWW, dim = 3), sep = "."),
                 paste("off", getItems(nonAgWW, dim = 3), sep = "."))
  cells     <- getItems(watNonAg, dim = 1)
  # Transform object dimensions
  .transformObject <- function(x,
                               cells, years, scenarios) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = cells,
                          years = years,
                          names = scenarios,
                          fill = 0,
                          sets = c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out     <- object0 + x
    return(out)
  }

  ## Routing-iteration-specific inputs
  if (iteration == "non_agriculture") {
    ## Previous Uses
    # Minimum flow requirements determined by natural flow river routing:
    # (full) Environmental Flow Requirements (in mio. m^3 / yr) [long-term average]
    prevReservedWW <- .transformObject(x = 0,
                                       cells = cells, years = selectyears, scenarios = scenarios)
    prevReservedWW[, , "on"] <- calcOutput("EnvmtlFlowRequirements",
                                          lpjml = lpjml, climatetype = climatetype,
                                          selectyears = selectyears, efrMethod = efrMethod,
                                          aggregate = FALSE)[, , "EFR"]
    # No previous consumption & no previous human uses yet
    prevReservedWC <- .transformObject(x = 0, cells = cells,
                                       years = selectyears, scenarios = scenarios)

    ## Current Uses
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currRequestWWlocal <- .transformObject(x = nonAgWW,
                                          cells = cells, years = selectyears, scenarios = scenarios)
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currRequestWClocal <- .transformObject(x = nonAgWC,
                                          cells = cells, years = selectyears, scenarios = scenarios)

    ## Discharge from previous routing (natural discharge)
    discharge <- collapseNames(calcOutput("RiverNaturalFlows",
                                          selectyears = selectyears,
                                          lpjml = lpjml, climatetype = climatetype,
                                          aggregate = FALSE)[, , "discharge_nat"])
    discharge <- .transformObject(x = discharge,
                                  cells = cells, years = selectyears, scenarios = scenarios)

  } else if (iteration == "committed_agriculture") {
    ## Previous Uses
    # Non-agricultural withdrawals and consumption
    previousHumanUse <- calcOutput("RiverHumanUseAccounting",
                                    iteration = "non_agriculture",
                                    lpjml = lpjml, climatetype = climatetype,
                                    efrMethod = efrMethod, multicropping = multicropping,
                                    selectyears = selectyears, iniyear = iniyear,
                                    transDist = transDist, comAg = NULL,
                                    accessibilityrule = NULL,
                                    rankmethod = NULL, gainthreshold = NULL,
                                    cropmix = NULL, yieldcalib = NULL,
                                    irrigationsystem = NULL, landScen = NULL,
                                    aggregate = FALSE)

    # Minimum flow requirements determined by previous river routing:
    # Environmental Flow Requirements + Reserved for Non-Agricultural Uses (in mio. m^3 / yr)
    prevReservedWW <- as.array(collapseNames(previousHumanUse[, , "reservedWW"]))
    # Previous human uses (determined in non-agricultural uses river routing) (in mio. m^3 / yr):
    prevReservedWC <- as.array(collapseNames(previousHumanUse[, , "reservedWC"]))

    ## Current Uses
    # Committed agricultural uses per crop (in mio. m^3 / yr)
    watComAg <- calcOutput("WaterUseCommittedAg",
                            lpjml = lpjml, climatetype = climatetype,
                            selectyears = selectyears, iniyear = iniyear,
                            multicropping = multicropping, aggregate = FALSE)
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currRequestWWlocal <- .transformObject(x = collapseNames(dimSums(watComAg[, , "withdrawal"],
                                                                 dim = "crop")),
                                          cells = cells, years = selectyears, scenarios = scenarios)
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currRequestWClocal <- .transformObject(x = collapseNames(dimSums(watComAg[, , "consumption"],
                                                                 dim = "crop")),
                                          cells = cells, years = selectyears, scenarios = scenarios)

    ## Discharge from previous routing
    discharge <- collapseNames(previousHumanUse[, , "discharge"])

} else if (iteration == "potential_irrigation") {

    if (comAg == TRUE) {
      # accounting in potentials
      comagyear <- iniyear
      # previous use
      humanuse <- "committed_agriculture"
    } else if (comAg == FALSE) {
      # committed agriculture not accounted in potentials (full potential)
      comagyear <- NULL
      # previous use
      humanuse <- "non_agriculture"
    }

    ## Yield gain potential through irrigation of proxy crops
    unit <- paste(unlist(str_split(rankmethod, ":"))[1],
                  unlist(str_split(rankmethod, ":"))[2],
                  sep = ":")
    irrigGain <- calcOutput("IrrigYieldImprovementPotential",
                            selectyears = selectyears, iniyear = iniyear,
                            comagyear = comagyear, transDist = transDist, efrMethod = efrMethod,
                            lpjml = lpjml, climatetype = climatetype, cropmix = cropmix,
                            unit = unit, yieldcalib = yieldcalib,
                            irrigationsystem = irrigationsystem, landScen = landScen,
                            multicropping = multicropping, aggregate = FALSE)
    irrigGain[irrigGain <= gainthreshold] <- 0
    irrigGain[irrigGain > gainthreshold]  <- 1
    irrigGain <- .transformObject(x = irrigGain,
                                  cells = cells, years = selectyears, scenarios = scenarios)

    ## Previous Uses
    prevRouting <- calcOutput("RiverHumanUseAccounting",
                              iteration = humanuse,
                              lpjml = lpjml, climatetype = climatetype,
                              efrMethod = efrMethod, multicropping = multicropping,
                              selectyears = selectyears, iniyear = iniyear,
                              transDist = transDist, comAg = NULL,
                              accessibilityrule = NULL,
                              rankmethod = NULL, gainthreshold = NULL,
                              cropmix = NULL, yieldcalib = NULL,
                              irrigationsystem = NULL, landScen = NULL,
                              aggregate = FALSE)
    # Reserved minimum flow: Inaccessible discharge +
    #                        Environmental Flow Requirements (adjusted for
    #                        part that is fulfilled by inaccessible water) +
    #                        Reserved for Non-Agricultural withdrawal +
    #                        [Reserved Committed Agricultural withdrawal, if activated] (in mio. m^3 / yr)
    prevReservedWW <- calcOutput("RiverWatReserved",
                                  transDist = transDist, accessibilityrule = accessibilityrule,
                                  selectyears = selectyears, iniyear = iniyear,
                                  lpjml = lpjml, climatetype = climatetype,
                                  comAg = comAg, multicropping = multicropping,
                                  efrMethod = efrMethod, aggregate = FALSE)
    # Non-agricultural and committed agricultural consumption
    prevReservedWC <- collapseNames(prevRouting[, , "reservedWC"])

    ## Current Uses
    # Required water for full irrigation per cell (in mio. m^3)
    reqWatFullirrig <- calcOutput("FullIrrigationRequirement",
                                  selectyears = selectyears, iniyear = iniyear,
                                  comagyear = comagyear, efrMethod = efrMethod, transDist = transDist,
                                  lpjml = lpjml, climatetype = climatetype,
                                  irrigationsystem = irrigationsystem, landScen = landScen,
                                  cropmix = cropmix, yieldcalib = yieldcalib,
                                  multicropping = multicropping, aggregate = FALSE)
    currRequestWWlocal <- pmax(collapseNames(reqWatFullirrig[, , "withdrawal"]), 0)
    currRequestWClocal <- pmax(collapseNames(reqWatFullirrig[, , "consumption"]), 0)
    # Only request water where gain from irrigation exceeds chosen threshold
    currRequestWWlocal <- currRequestWWlocal * irrigGain
    currRequestWClocal <- currRequestWClocal * irrigGain

    ## Discharge determined by previous river routings (in mio. m^3 / yr)
    discharge <- collapseNames(prevRouting[, , "discharge"])

  } else {
    stop("Please specify iteration for which river routing inputs shall be calculated:
         non_agriculture, committed_agriculture or potential")
  }

  ###############
  ### Outputs ###
  ###############
  ### Final magpie object structure to be filled
  out <- .transformObject(x = new.magpie(cells_and_regions = getItems(watNonAg, dim = 1),
                                        years = getItems(watNonAg, dim = 2),
                                        names = c("discharge",
                                                  "prevReservedWW", "prevReservedWC",
                                                  "currRequestWWlocal", "currRequestWClocal"),
                                        sets = c("x.y.iso", "year", "data"),
                                        fill = NA),
                          cells = cells, years = selectyears, scenarios = scenarios)
  out[, , "discharge"]          <- discharge
  out[, , "prevReservedWW"]     <- prevReservedWW
  out[, , "prevReservedWC"]     <- prevReservedWC
  out[, , "currRequestWWlocal"] <- currRequestWWlocal
  out[, , "currRequestWClocal"] <- currRequestWClocal

  ##############
  ### Checks ###
  ##############
  if (any(is.na(out))) {
    stop("calcRiverRoutinInputs produced NAs!")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("River routing inputs for ",
                                    iteration, " routing"),
              isocountries = FALSE))
}
