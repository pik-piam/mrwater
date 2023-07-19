#' @title       calcRiverHumanUseAccounting
#' @description This function calculates human uses and reserved water along
#'              the river
#'
#' @param lpjml         LPJmL version used
#' @param selectyears   Years to be returned
#'                      (Note: does not affect years of harmonization or smoothing)
#' @param iteration     Water use to be allocated in this river routing iteration
#'                      (non_agriculture, committed_agriculture, potential_irrigation,
#'                      special case (for current irrigated area analysis): "committed_agriculture_fullPotential").
#' @param climatetype   Switch between different climate scenarios
#'                      or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear       Initialization year of irrigation system
#' @param efrMethod     EFR method used including selected strictness of EFRs
#'                      (Smakhtin:good, VMF:fair)
#' @param multicropping  Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLanduseToolbox
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#' @param comAg         if TRUE: the currently already irrigated areas
#'                               in initialization year are reserved for irrigation,
#'                      if FALSE: no irrigation areas reserved (irrigation potential).
#'                      Only relevant for iteration = potential
#' @param accessibilityrule (For case of iteration = "potential_irrigation" only:)
#'                          Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
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
#' @param gainthreshold     (For case of iteration = "potential_irrigation" only:)
#'                          Threshold of yield improvement potential
#'                          (same unit as in rankmethod)
#' @param cropmix           (For case of iteration = "potential_irrigation" only:)
#'                          Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param irrigationsystem  (For case of iteration = "potential_irrigation" only:)
#'                          Irrigation system to be used for river basin discharge
#'                          allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param yieldcalib        (For case of iteration = "potential_irrigation" only:)
#'                          If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param landScen          (For case of iteration = "potential_irrigation" only:)
#'                          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverHumanUseAccounting", aggregate = FALSE)
#' }
#'
calcRiverHumanUseAccounting <- function(iteration,
                                        lpjml, climatetype,
                                        selectyears, iniyear,
                                        efrMethod, multicropping,
                                        transDist, comAg,
                                        accessibilityrule,
                                        rankmethod, gainthreshold,
                                        cropmix, yieldcalib,
                                        irrigationsystem, landScen) {
  ##############################
  ###### Read in Inputs ########
  ##############################

  ### Read in river structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds",
                            package = "mrwater"))
  # Read in neighbor cells and transform to list
  neighborCells <- readSource("NeighborCells", convert = FALSE)
  neighborCells <- attr(neighborCells, "data")
  # Calculate river structure including neighbor cells
  rs <- toolSelectNeighborCell(transDist = transDist, rs = rs,
                               neighborCells = neighborCells)

  ### Inputs from previous river routings
  inputData <- calcOutput("RiverRoutingInputs",
                          iteration = iteration,
                          lpjml = lpjml, climatetype = climatetype,
                          transDist = transDist, comAg = comAg,
                          selectyears = selectyears, iniyear = iniyear,
                          efrMethod = efrMethod, multicropping = multicropping,
                          accessibilityrule = accessibilityrule,
                          rankmethod = rankmethod, gainthreshold = gainthreshold,
                          cropmix = cropmix, yieldcalib = yieldcalib,
                          irrigationsystem = irrigationsystem, landScen = landScen,
                          aggregate = FALSE)
  dischargeMAG <- collapseNames(inputData[, , "discharge"])

  # Object dimensions:
  gridcells <- getItems(dischargeMAG, dim = 1)
  dimnames  <- getItems(dischargeMAG, dim = 3)
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

  ### Water inputs
  # Natural flows
  natFlows <- calcOutput("RiverNaturalFlows",
                          selectyears = selectyears,
                          lpjml = lpjml, climatetype = climatetype,
                          aggregate = FALSE)
  # Natural discharge (for checks)
  natDischarge <- .transformObject(x = collapseNames(natFlows[, , "discharge_nat"]),
                                   gridcells = gridcells,
                                   years = selectyears, names = dimnames)
  # Lake evaporation as calculated by natural flow river routing
  lakeEvap <- collapseNames(natFlows[, , "lake_evap_nat"])
  # Runoff (on land and water)
  yearlyRunoff <- collapseNames(calcOutput("YearlyRunoff",
                                          selectyears = selectyears,
                                          lpjml = lpjml, climatetype = climatetype,
                                          aggregate = FALSE))
  # Reduce to one object (for performance reasons)
  runoffWOEvap <- yearlyRunoff - lakeEvap

  # Initialize river routing variables and dimensions
  fracFulfilled <- missingWW <- missingWC <- as.array(.transformObject(x = 0,
                                                                       gridcells = gridcells,
                                                                       years = selectyears,
                                                                       names = dimnames))
  # Extract scenarios and years
  years     <- getItems(dischargeMAG, dim = 2)
  scenarios <- getItems(dischargeMAG, dim = 3)
  # Adjust object dimension and type
  runoffWOEvap   <- as.array(.transformObject(x = runoffWOEvap,
                                              gridcells = gridcells,
                                              years = selectyears, names = dimnames))
  discharge      <- as.array(dischargeMAG)
  prevReservedWW <- as.array(collapseNames(inputData[, , "prevReservedWW"]))
  prevReservedWC <- as.array(collapseNames(inputData[, , "prevReservedWC"]))
  currRequestWWlocal <- currRequestWWtotal <- as.array(collapseNames(inputData[, , "currRequestWWlocal"]))
  currRequestWClocal <- currRequestWCtotal <- as.array(collapseNames(inputData[, , "currRequestWClocal"]))

  ##################################################
  ###### Upstream-Downstream River Routing  ########
  ##################################################
  for (y in years) {
    for (scen in scenarios) {
      # Select scenario and reduce object size (for performance)
      tmpRequestWWlocal <- currRequestWWlocal[, y, scen]
      tmpRequestWClocal <- currRequestWClocal[, y, scen]
      tmpDischarge      <- discharge[, y, scen]

      # Cells to be calculated
      cellsCalc <- which(tmpRequestWWlocal > 0)
      cellsCalc <- unique(c(cellsCalc, unlist(rs$downstreamcells[cellsCalc])))
      cellsCalc <- cellsCalc[order(rs$calcorder[cellsCalc], decreasing = FALSE)]

      for (c in cellsCalc) {

        # Does the respective cell request water withdrawal?
        # Or: Is available water is smaller than previously reserved withdrawal?
        #     Then: update of discharge required.
        if ((tmpRequestWWlocal[c] > 0) ||
            ((tmpDischarge[c] + prevReservedWC[c, y, scen]) < prevReservedWW[c, y, scen])) {

          cellsRequest <- cellsDischarge <- c
          if (length(rs$upstreamcells[[c]]) > 0) {
            cellsRequest <- c(cellsRequest, unlist(rs$upstreamcells[[c]]))
          }
          if (length(rs$downstreamcells[[c]]) > 0) {
            cellsDischarge <- c(cellsDischarge, unlist(rs$downstreamcells[[c]]))
          }

          tmp <- toolRiverUpDownBalanceSINGLE(inLIST = list(prevWC = prevReservedWC[c, y, scen],
                                                            prevWW = prevReservedWW[c, y, scen],
                                                            currWW = tmpRequestWWlocal[c]),
                                              inoutLIST = list(q = tmpDischarge[cellsDischarge],
                                                               currWC = tmpRequestWClocal[cellsRequest]))

          # Updated flows
          tmpDischarge[cellsDischarge]    <- tmp$q
          tmpRequestWClocal[cellsRequest] <- tmp$currWC
        }
      }
      # Save result for respective scenario
      currRequestWClocal[, y, scen] <- tmpRequestWClocal
    }
  }

  # Update current withdrawal
  fracFulfilled <- ifelse(currRequestWCtotal > 0,
                            currRequestWClocal / currRequestWCtotal,
                          0)
  currRequestWWlocal <- fracFulfilled * currRequestWWtotal

  # Update minimum water required in cell (for further river processing steps):
  prevReservedWW <- prevReservedWW + currRequestWWlocal
  prevReservedWC <- prevReservedWC + currRequestWClocal

  # Update discharge given reserved water (consumptive)
  for (y in years) {
    for (scen in scenarios) {
      tmp <- toolRiverDischargeUpdate(rs = rs,
                                      runoffWOEvap = runoffWOEvap[, y, scen],
                                      watCons = prevReservedWC[, y, scen])
      discharge[, y, scen] <- tmp
    }
  }

  # Locally missing water that might be fulfilled by surrounding cells
  missingWW <- currRequestWWtotal - currRequestWWlocal
  missingWC <- currRequestWCtotal - currRequestWClocal

  # If local water is not sufficient:
  # may be fulfilled by surrounding cell water provision
  if (transDist != 0) {
    tmp <- toolNeighborUpDownProvision(rs = rs, transDist = transDist,
                                       years = years, scenarios = scenarios,
                       listNeighborIN = list(discharge = discharge,
                                             prevReservedWC = prevReservedWC,
                                             prevReservedWW = prevReservedWW,
                                             missingWC = missingWC,
                                             missingWW = missingWW,
                                             runoffWOEvap = runoffWOEvap))
    # Update discharge
    discharge <- tmp$discharge

    # Update minimum water required in cell (for further river processing steps):
    prevReservedWW <- prevReservedWW + tmp$toNeighborWW
    prevReservedWC <- prevReservedWC + tmp$toNeighborWC

    # Update currently fulfilled water use
    currRequestWWtotal <- currRequestWWlocal + tmp$fromNeighborWW
    currRequestWCtotal <- currRequestWClocal + tmp$fromNeighborWC

    ### Checks ###
    # All reserved water needs to end up somewhere
    if (any(abs(round(apply(tmp$toNeighborWW, MARGIN = 3, FUN = sum) -
                        apply(tmp$fromNeighborWW, MARGIN = 3, FUN = sum),
                      digits = 6)) > 1e-6)) {
      stop(paste0("After Neighbor Water Provision in
                   calcRiverHumanUseAccounting with iteration = ",
                   iteration, " some water was not properly allocated."))
    }
    if (any(abs(round(apply(tmp$toNeighborWC, MARGIN = 3, FUN = sum) -
                        apply(tmp$fromNeighborWC, MARGIN = 3, FUN = sum),
                      digits = 6)) > 1e-6)) {
      stop(paste0("After Neighbor Water Provision in
                   calcRiverHumanUseAccounting with iteration = ",
                   iteration, " some water was not properly allocated."))
    }
  } else {
    # total and local currently requested water fulfilled is the same in this case
    currRequestWWtotal <- currRequestWWlocal
    currRequestWCtotal <- currRequestWClocal
  }

  ###############
  ### Outputs ###
  ###############
  ### Final magpie object structure to be filled
  out <- .transformObject(x = new.magpie(cells_and_regions = getItems(natDischarge, dim = 1),
                                         years = getItems(natDischarge, dim = 2),
                                         names = c("discharge",
                          # water reserved in current cell (for either local or neighboring cell)
                          # to be considered in following river water use accountings
                                                   "reservedWW",
                                                   "reservedWC",
                          # water available for specified use from local sources and surrounding cells:
                                                   "currHumanWWtotal", "currHumanWCtotal",
                          # water available for specified use from local sources
                                                   "currHumanWWlocal", "currHumanWClocal"),
                                          sets = c("x.y.iso", "year", "data"),
                                          fill = NA),
                          gridcells = gridcells,
                          years = selectyears, names = dimnames)
  # river discharge flows
  out[, , "discharge"] <- as.magpie(discharge, spatial = 1, temporal = 2)
  # water reserved in current cell (for either local or neighboring cell)
  # to be considered in following river water use accountings
  out[, , "reservedWW"] <- as.magpie(prevReservedWW, spatial = 1, temporal = 2)
  out[, , "reservedWC"] <- as.magpie(prevReservedWC, spatial = 1, temporal = 2)
  # water use that is fulfilled locally
  out[, , "currHumanWClocal"] <- as.magpie(currRequestWClocal, spatial = 1, temporal = 2)
  out[, , "currHumanWWlocal"] <- as.magpie(currRequestWWlocal, spatial = 1, temporal = 2)
  # local water use that is fulfilled by local water resources or by surrounding water available
  out[, , "currHumanWCtotal"] <- as.magpie(currRequestWCtotal, spatial = 1, temporal = 2)
  out[, , "currHumanWWtotal"] <- as.magpie(currRequestWWtotal, spatial = 1, temporal = 2)

  ##############
  ### Checks ###
  ##############
  if (any(is.na(out))) {
    stop("calcRiverHumanUseAccounting produced NAs!")
  }
  if (any(round(out, digits = 6) < 0)) {
    stop("calcRiverHumanUseAccounting produced negative values")
  }
  # Check if too much water has been allocated
  # (currRequestWClocal should always be smaller than currRequestWCtotal)
  if (any((out[, , "currHumanWClocal"] - out[, , "currHumanWCtotal"]) > 1e-6)) {
    stop("Too much water has been allocated
          in calcRiverHumanUseAccounting.")
  }
  # Check whether more than available discharge has been reserved
  if (any(round(out[, , "discharge"] + out[, , "reservedWC"] - out[, , "reservedWW"],
                digits = 6) < 0)) {
    stop("Too much water has been reserved
          in calcRiverHumanUseAccounting.")
  }
  # Check whether water has been lost
  basinDischarge <- .transformObject(x = 0,
                                     gridcells = gridcells,
                                     years = selectyears,
                                     names = dimnames)
  basinDischarge[unique(rs$endcell), , ] <- out[unique(rs$endcell), , "discharge"]
  totalWat <- dimSums(basinDischarge, dim = 1) +
                dimSums(out[, , "currHumanWCtotal"],
                        dim = c("x", "y", "iso", "data")) +
                  dimSums(inputData[, , "prevReservedWC"], dim = 1)
  # Total water (summed basin discharge + consumed)
  # must be identical across scenarios
  for (y in selectyears) {
    if (!all(abs(totalWat[, y, ] - mean(totalWat[, y, ])) < 1e-06)) {
      stop("In calcRiverHumanUseAccounting:
            Scenarios differ. That should not be the case.
            Total water volume should always be the same")
    }
  }
  # Total water (summed basin discharge + consumed)
  # must be same as natural summed basin discharge
  if (any(abs(round(dimSums(natDischarge[unique(rs$endcell), , ],
                        dim = 1) - totalWat,
                digits = 6)) > 1e-6)) {
    stop("In calcRiverHumanUseAccounting:
          Water has been lost during the Neighbor Water Provision Algorithm")
  }

  # Description
  description <- paste0("Upstream-downstream river routing outputs taking
                         human uses (", iteration, ") into account")

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
