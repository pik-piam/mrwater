#' @title       calcRiverDischargeAllocation_NEW2
#' @description This function distributes surplus basin discharge after the
#'              previous river routings following certain management assumptions
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years to be returned
#'                          (Note: does not affect years of harmonization or smoothing)
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
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
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          ("optimization" or "upstreamfirst")
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand
#' @param gainthreshold     Threshold of yield improvement potential
#'                          (same unit as in rankmethod)
#' @param irrigationsystem  Irrigation system to be used for river basin discharge
#'                          allocation algorithm ("surface", "sprinkler", "drip", "initialization")
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
#' @param comAg             if TRUE: the currently already irrigated areas
#'                                   in initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          ("endogenous": suitability for multiple cropping determined
#'                                         by rules based on grass and crop productivity
#'                          "exogenous": suitability for multiple cropping given by
#'                                       GAEZ data set),
#'                          separated by ":"
#'                          (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames as.magpie getCells setCells mbind setYears
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverDischargeAllocation_NEW2", aggregate = FALSE)
#' }
#'
calcRiverDischargeAllocation_NEW2 <- function(lpjml, climatetype,
                                             selectyears, efrMethod,
                                             accessibilityrule, transDist,
                                             rankmethod, yieldcalib,
                                             allocationrule,
                                             gainthreshold, irrigationsystem, iniyear, landScen,
                                             cropmix, comAg, multicropping) {
  # Retrieve arguments
  if (!is.numeric(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }
  if (is.numeric(selectyears)) {
    selectyears <- paste0("y", selectyears)
  }

  # Natural discharge (for checks)
  natDischarge <- calcOutput("RiverNaturalFlows",
                             selectyears = selectyears,
                             lpjml = lpjml, climatetype = climatetype,
                             aggregate = FALSE)
  # Previous discharge
  inputData <- calcOutput("RiverRoutingInputs",
                          iteration = "potential_irrigation",
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

  # Object to be returned
  out <- new.magpie(cells_and_regions = getCells(dischargeMAG),
                    years = getYears(dischargeMAG),
                    names = c("discharge",
                              "currWWlocal", "currWClocal",
                              "currWWtotal", "currWCtotal"),
                    sets = c("x.y.iso", "year", "data"))
  out <- .transformObject(x = out, gridcells = gridcells,
                          years = selectyears, names = dimnames)

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # Read in river structure
  rs0 <- readRDS(system.file("extdata/riverstructure_stn_coord.rds",
                package = "mrwater"))
  # Read in neighbor cells and transform to list
  neighborCells <- readSource("NeighborCells", convert = FALSE)
  neighborCells <- attr(neighborCells, "data")
  # Calculate river structure including neighbor cells
  rs0 <- toolSelectNeighborCell(transDist = transDist, rs = rs0,
                               neighborCells = neighborCells)
  # Add country information
  map          <- toolGetMappingCoord2Country(extended = FALSE, pretty = FALSE)
  rs0$isoCoord <- paste(rs0$coordinates, map$iso, sep = ".")
  # Reduce list size
  rs <- list()
  rs$cells           <- rs0$cells
  rs$isoCoord        <- rs0$isoCoord
  rs$downstreamcells <- rs0$downstreamcells
  rs$neighborcell    <- rs0$neighborcell

  if (allocationrule == "optimization") {

    if (comAg == TRUE) {
      # accounting in potentials
      comagyear <- iniyear
    } else if (comAg == FALSE) {
      # committed agriculture not accounted in potentials (full potential)
      comagyear <- NULL
    }
    # Global cell rank based on yield gain potential by irrigation
    # of chosen crop mix
    glocellrank <- setYears(calcOutput("IrrigCellranking",
                                        cellrankyear = selectyears,
                                        lpjml = lpjml, climatetype = climatetype, method = rankmethod,
                                        cropmix = cropmix, iniyear = iniyear, yieldcalib = yieldcalib,
                                        comagyear = comagyear, irrigationsystem = irrigationsystem,
                                        landScen = landScen, multicropping = multicropping,
                                       aggregate = FALSE),
                            selectyears)

    ### Inputs from Previous River Routing Iterations ###
    scenarios      <- getItems(dischargeMAG, dim = 3)
    discharge      <- as.array(dischargeMAG)
    prevReservedWW <- as.array(collapseNames(inputData[, , "prevReservedWW"]))
    currReqWW      <- as.array(collapseNames(inputData[, , "currRequestWWlocal"]))
    currReqWC      <- as.array(collapseNames(inputData[, , "currRequestWClocal"]))
    glocellrank    <- as.array(glocellrank)

    # Initialize objects to be filled in Allocation Algorithm with 0
    fromNeighborWC <- fromNeighborWW <- as.array(.transformObject(x = 0, gridcells = gridcells,
                                                                  years = selectyears, names = dimnames))
    currWWlocal <- currWClocal <- as.array(.transformObject(x = 0, gridcells = gridcells,
                                                            years = selectyears, names = dimnames))

    ################################################
    ####### River basin discharge Allocation #######
    #######        (ranked cell order)       #######
    ################################################
    # Share of full irrigation water requirements to be allocated for each round of the allocation algorithm
    allocationshare <- 1 / (length(glocellrank[, 1, 1]) / 67420)
    currReqWW <- currReqWW * allocationshare
    currReqWC <- currReqWC * allocationshare

    # numeric cell number
    rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

    # In case of optimization, glocellrank differs in each year:
    for (y in selectyears) {
      for (scen in scenarios) {
        # Loop in ranked cell order
        for (o in (1:max(glocellrank[, y, ], na.rm = TRUE))) {
          # Extract the cell number
          glocellrankName <- names(which(glocellrank[, y, ] == o))
          c <- rs$cells[rs$isoCoord == ifelse(grepl("B_", glocellrankName),
                                               gsub("B_", "", glocellrankName),
                                              ifelse(grepl("A_", glocellrankName),
                                                      gsub("A_", "", glocellrankName),
                                                     glocellrankName))]

          # Only run for cells where water required
          if (currReqWW[c, y, scen] > 1e-4) {
            # Select relevant cells (for performance reasons)
            if (transDist > 0) {
              nCells <- rs$neighborcell[[c]]
            } else {
              nCells <- NULL
            }
            if (length(rs$downstreamcells[nCells]) > 0) {
              nCells <- c(nCells, unlist(rs$downstreamcells[nCells]))
            }
            downCells <- NULL
            if (length(rs$downstreamcells[[c]]) > 0) {
              downCells <- c(downCells, unlist(rs$downstreamcells[[c]]))

            }
            selectCells <- unique(c(c, downCells, nCells))
            # Function inputs
            inLIST    <- list(currReqWW = currReqWW[c, y, scen],
                              currReqWC = currReqWC[c, y, scen])
            inoutLIST <- list(discharge = discharge[selectCells, y, scen],
                              prevReservedWW = prevReservedWW[selectCells, y, scen])

            tmp <- toolRiverDischargeAllocationSINGLE(c = c, rs = rs,
                                                      downCells = downCells,
                                                      transDist = transDist,
                                                      iteration = "main",
                                                      inoutLIST = inoutLIST,
                                                      inLIST = inLIST)

            discharge[selectCells, y, scen]      <- tmp$discharge
            prevReservedWW[selectCells, y, scen] <- tmp$prevReservedWW
            fromNeighborWC[c, y, scen] <- fromNeighborWC[c, y, scen] + tmp$fromNeighborWC
            fromNeighborWW[c, y, scen] <- fromNeighborWW[c, y, scen] + tmp$fromNeighborWW
            currWWlocal[c, y, scen]    <- currWWlocal[c, y, scen] + tmp$currWWlocal
            currWClocal[c, y, scen]    <- currWClocal[c, y, scen] + tmp$currWClocal
          }
        }
      }
    }

  # update water available for use in cell c including provision by surrounding cells
  # (in case of transDist == 0: fromNeighbor would be 0)
  currWWtotal <- currWWlocal + fromNeighborWW
  currWCtotal <- currWClocal + fromNeighborWC

  # Return output in one object
  out[, , "currWWlocal"] <- as.magpie(currWWlocal, spatial = 1, temporal = 2)
  out[, , "currWClocal"] <- as.magpie(currWClocal, spatial = 1, temporal = 2)
  out[, , "currWWtotal"] <- as.magpie(currWWtotal, spatial = 1, temporal = 2)
  out[, , "currWCtotal"] <- as.magpie(currWCtotal, spatial = 1, temporal = 2)
  out[, , "discharge"]   <- as.magpie(discharge, spatial = 1, temporal = 2)
  ### Note: Need to adjust follow-up functions accordingly
  ###       (before: select output == "discharge", "potIrrigWat",
  ##         now: subset (discharge, currHuman_ww_local ....))
  #    # Main output for MAgPIE: water available for agricultural withdrawal --> currWWtotal

  } else if (allocationrule == "upstreamfirst") {
    # The upstream-downstream surplus discharge allocation
    # follows the same rules ad the upstream-downstream
    # previous human use accounting
    tmp <- calcOutput("RiverHumanUseAccounting",
                      iteration = "potential_irrigation",
                      lpjml = lpjml, climatetype = climatetype,
                      efrMethod = efrMethod, multicropping = multicropping,
                      selectyears = selectyears, iniyear = iniyear,
                      transDist = transDist, comAg = comAg,
                      accessibilityrule = accessibilityrule,
                      rankmethod = rankmethod, gainthreshold = gainthreshold,
                      cropmix = cropmix, yieldcalib = yieldcalib,
                      irrigationsystem = irrigationsystem, landScen = landScen,
                      aggregate = FALSE)

    # Return outputs
    out[, , "currWWlocal"] <- collapseNames(tmp[, , "currHumanWWlocal"])
    out[, , "currWClocal"] <- collapseNames(tmp[, , "currHumanWClocal"])
    out[, , "currWWtotal"] <- collapseNames(tmp[, , "currHumanWWtotal"])
    out[, , "currWCtotal"] <- collapseNames(tmp[, , "currHumanWCtotal"])
    out[, , "discharge"]   <- collapseNames(tmp[, , "discharge"])
  }

  ##############
  ### Checks ###
  ##############
  # Check for NAs and negative values
  if (any(is.na(out))) {
    stop("calcRiverDischargeAllocation produced NAs")
  }
  if (any(round(out) < 0)) {
    stop("calcRiverDischargeAllocation produced negative values")
  }
  # No water should be lost
  basinDischarge <- .transformObject(x = 0,  gridcells = gridcells,
                                     years = selectyears, names = dimnames)
  basinDischarge[unique(rs0$endcell), , ] <- out[unique(rs0$endcell), , "discharge"]
  totalWat <- dimSums(basinDischarge,
                      dim = 1) + dimSums(out[, , "currWCtotal"],
                                        dim = c("x", "y", "iso", "data")) +
                                  dimSums(inputData[, , "prevReservedWC"],
                                          dim = c("x", "y", "iso", "data"))
  # Total water (summed basin discharge + consumed)
  # must be identical across scenarios
  if (!all(abs(totalWat - mean(totalWat)) < 1e-06)) {
    stop("In calcRiverDischargeAllocation:
          Scenarios differ. That should not be the case.
          Total water volume should always be the same")
  }
  # Total water (summed basin discharge + consumed)
  # must be same as natural summed basin discharge
  natDischarge <- .transformObject(x = collapseNames(natDischarge[, , "discharge_nat"]),
                                   gridcells = gridcells,
                                   years = selectyears, names = dimnames)
  if (any(round(dimSums(natDischarge[unique(rs0$endcell), , ],
                        dim = 1) - totalWat[, , 1],
                digits = 6) != 0)) {
    stop("In calcRiverDischargeAllocation:
          Water has been lost during the Neighbor Water Provision Algorithm")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("River routing outputs
                                     after Surplus Discharge Allocation"),
              isocountries = FALSE))
}
