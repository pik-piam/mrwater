#' @title       calcPotWater
#' @description This function returns the potential water quantity
#'              (separted into withdrawal and consumption)
#'              available for different uses
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years for which irrigatable area is calculated
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          USD_ha (USD per hectare) for relative area return, or
#'                          USD_m3 (USD per cubic meter) for relative volumetric return;
#'                          USD for absolute return (total profit);
#'                          Price aggregation:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices
#'                          and boolean indicating fullpotential (TRUE, i.e. cell
#'                          receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                          receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (in USD per hectare)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear           Initialization year of irrigation system
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param comAg             If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param fossilGW          If TRUE: non-renewable groundwater can be used.
#'                          If FALSE: non-renewable groundwater cannot be used.
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
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells mbind add_dimension new.magpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("PotWater", aggregate = FALSE)
#' }

calcPotWater <- function(lpjml, selectyears, climatetype, efrMethod,
                         accessibilityrule, rankmethod, yieldcalib, allocationrule,
                         gainthreshold, irrigationsystem, iniyear,
                         landScen, cropmix, comAg, fossilGW,
                         multicropping, transDist) {

  if (!is.numeric(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }

  # Check whether cropmix argument is set correctly
  if (comAg) {
    cmix <- "hist_rainf"
  } else {
    cmix <- cropmix
  }

  # Water potentially available for additional potential irrigation
  # from renewable water sources (additionally to already reserved water
  # for committed agricultural uses, i.e.
  # if comAg: this water fraction is already reserved)
  # unit: mio. m^3
  watAvlAg  <- collapseNames(calcOutput("RiverDischargeAllocation",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, transDist = transDist,
                                        accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                        allocationrule = allocationrule,
                                        gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                                        iniyear = iniyear, landScen = landScen,
                                        cropmix = cmix, comAg = comAg,
                                        multicropping = multicropping, aggregate = FALSE))
  watAvlAgWW <- collapseNames(watAvlAg[, , "currWWtotal"])
  watAvlAgWC <- collapseNames(watAvlAg[, , "currWCtotal"])

  # Object dimensionality
  .transformObject <- function(x, gridcells, years, names) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = gridcells,
                          years = years,
                          names = names,
                          fill = 0,
                          sets = c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out <- object0 + x
    return(out)
  }

  watNonAgWW <- watNonAgWC <- watComAgWW <- watComAgWC <- new.magpie(cells_and_regions = getCells(watAvlAgWW),
                                                                     years = getYears(watAvlAgWW),
                                                                     names = getNames(watAvlAgWW),
                                                                     fill = 0)

  # Water use for non-agricultural purposes
  # unit: mio. m^3
  watNonAg <- calcOutput("RiverHumanUseAccounting",
                         iteration = "non_agriculture",
                         lpjml = lpjml, climatetype = climatetype,
                         transDist = transDist, comAg = NULL,
                         efrMethod = efrMethod, multicropping = multicropping,
                         selectyears = selectyears, iniyear = iniyear,
                         accessibilityrule = NULL,
                         rankmethod = NULL, gainthreshold = NULL,
                         cropmix = NULL, yieldcalib = NULL,
                         irrigationsystem = NULL, landScen = NULL,
                         aggregate = FALSE)
  watNonAgWW <- collapseNames(watNonAg[, , "currHumanWWtotal"])
  watNonAgWC <- collapseNames(watNonAg[, , "currHumanWCtotal"])

  # Fossil groundwater use
  if (fossilGW) {

    # fossil groundwater used in non-agricultural sector
    gw <- calcOutput("NonrenGroundwatUse", output = "nonAg",
                     multicropping = multicropping,
                     lpjml = lpjml, climatetype = climatetype,
                     selectyears = selectyears, iniyear = iniyear,
                     aggregate = FALSE)
    gwWW <- .transformObject(x = collapseNames(gw[, , "withdrawal"]),
                             gridcells = getItems(watAvlAgWW, dim = 1),
                             years = getItems(watAvlAgWW, dim = 2),
                             names = getItems(watAvlAgWW, dim = 3))
    gwWC <- .transformObject(x = collapseNames(gw[, , "consumption"]),
                             gridcells = getItems(watAvlAgWW, dim = 1),
                             years = getItems(watAvlAgWW, dim = 2),
                             names = getItems(watAvlAgWW, dim = 3))
    # add fossil groundwater to renewable groundwater used in non-agricultural sector
    watNonAgWW <- watNonAgWW + gwWW
    watNonAgWC <- watNonAgWC + gwWC

    # Fossil groundwater available in agricultural sector
    # Note: if no areas are committed (previously reserved),
    #       all of the fossil groundwater is available for
    #       additional irrigation (but capped at maximum area in calcPotIrrigAreas)
    gw <- calcOutput("NonrenGroundwatUse", output = "comAg",
                     multicropping = multicropping,
                     lpjml = lpjml, climatetype = climatetype,
                     selectyears = selectyears, iniyear = iniyear,
                     aggregate = FALSE)
    gwWW <- .transformObject(x = collapseNames(gw[, , "withdrawal"]),
                             gridcells = getItems(watAvlAgWW, dim = 1),
                             years = getItems(watAvlAgWW, dim = 2),
                             names = getItems(watAvlAgWW, dim = 3))
    gwWC <- .transformObject(x = collapseNames(gw[, , "consumption"]),
                             gridcells = getItems(watAvlAgWW, dim = 1),
                             years = getItems(watAvlAgWW, dim = 2),
                             names = getItems(watAvlAgWW, dim = 3))

  } else {
    gwWW <- gwWC <- .transformObject(x = 0,
                                     gridcells = getItems(watAvlAgWW, dim = 1),
                                     years = getItems(watAvlAgWW, dim = 2),
                                     names = getItems(watAvlAgWW, dim = 3))
  }

  if (comAg) {

    # (Renewable) water already committed to irrigation in algorithm
    currHuman <- calcOutput("RiverHumanUseAccounting",
                            iteration = "committed_agriculture",
                            lpjml = lpjml, climatetype = climatetype,
                            transDist = transDist, comAg = NULL,
                            efrMethod = efrMethod, multicropping = multicropping,
                            selectyears = selectyears, iniyear = iniyear,
                            accessibilityrule = NULL,
                            rankmethod = NULL, gainthreshold = NULL,
                            cropmix = NULL, yieldcalib = NULL,
                            irrigationsystem = NULL, landScen = NULL,
                            aggregate = FALSE)

    if (as.logical(stringr::str_split(multicropping, ":")[[1]][1])) {
      # water required to expand multiple cropping in already irrigated areas and can
      # be fulfilled by renewable water resources
      currHumanAdd <- calcOutput("RiverHumanUseAccounting",
                                 iteration = "committed_agriculture_fullMulticropping",
                                 lpjml = lpjml, climatetype = climatetype,
                                 transDist = transDist, comAg = NULL,
                                 efrMethod = efrMethod, multicropping = multicropping,
                                 selectyears = selectyears, iniyear = iniyear,
                                 accessibilityrule = NULL,
                                 rankmethod = NULL, gainthreshold = NULL,
                                 cropmix = NULL, yieldcalib = NULL,
                                 irrigationsystem = NULL, landScen = NULL,
                                 aggregate = FALSE)
      currHuman <- currHuman + currHumanAdd
    }

  } else {

    # No water is previously committed
    currHuman       <- watNonAg
    currHuman[, , "currHumanWWtotal"] <- currHuman[, , "currHumanWCtotal"] <- 0

  }

  watComAgWW <- collapseNames(currHuman[, , "currHumanWWtotal"])
  watComAgWC <- collapseNames(currHuman[, , "currHumanWCtotal"])

  #### Function outputs ###
  # Agricultural includes additional potential from renewable resources,
  # committed from renewable resources
  # and fossil groundwater
  watAgWW <- watAvlAgWW + watComAgWW + gwWW
  watAgWC <- watAvlAgWC + watComAgWC + gwWC
  # Total includes non-agricultural and agricultural
  # from both renewable (and fossil groundwater, if activated)
  watTotWW <- watNonAgWW + watAgWW
  watTotWC <- watNonAgWC + watAgWC

  watAgWW  <- add_dimension(watAgWW, dim = 3.4, add = "type", nm = "wat_ag_ww")
  watAgWC  <- add_dimension(watAgWC, dim = 3.4, add = "type", nm = "wat_ag_wc")
  watTotWW <- add_dimension(watTotWW, dim = 3.4, add = "type", nm = "wat_tot_ww")
  watTotWC <- add_dimension(watTotWC, dim = 3.4, add = "type", nm = "wat_tot_wc")

  out          <- mbind(watAgWW, watAgWC, watTotWW, watTotWC)
  getSets(out) <- c("x", "y", "iso", "year", "EFP", "scen", "type")

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("potential water availability for agricultural usage ",
                                    "or total human water usage"),
              isocountries = FALSE))
}
