#' @title       calcRiverHumanUseAccounting
#' @description This function calculates human uses and reserved water along
#'              the river
#'
#' @param lpjml         LPJmL version used
#' @param selectyears   Years to be returned
#'                      (Note: does not affect years of harmonization or smoothing)
#' @param humanuse      Human use type to which river routing shall be applied
#'                      (non_agriculture or committed_agriculture).
#' @param climatetype   Switch between different climate scenarios
#'                      or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear       Initialization year of irrigation system
#' @param efrMethod     EFR method used including selected strictness of EFRs
#'                      (Smakhtin:good, VMF:fair)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                      separated by ":"
#'                      (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
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

calcRiverHumanUseAccounting <- function(humanuse, lpjml, climatetype, selectyears,
                                        iniyear, efrMethod, multicropping, transDist) {

  ### Read in river structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds",
                            package = "mrwater"))
  # Read in neighbor cells and transform to list
  neighborCells <- readSource("NeighborCells", convert = FALSE)
  neighborCells <- attr(neighborCells, "data")
  # Calculate river structure including neighbor cells
  rs <- toolSelectNeighborCell(transDist = transDist, rs = rs,
                               neighborCells = neighborCells)

  ## Human uses
  # Non-Agricultural Water Withdrawals and Consumption (in mio. m^3 / yr) [smoothed]
  watNonAg <- calcOutput("WaterUseNonAg",
                          selectyears = selectyears, cells = "lpjcell",
                          datasource = "WATERGAP_ISIMIP", usetype = "total",
                          seasonality = "total", harmonType = "average",
                          lpjml = NULL, climatetype = NULL, aggregate = FALSE)

  nonAgWWmag <- collapseNames(watNonAg[, , "withdrawal"])
  nonAgWCmag <- collapseNames(watNonAg[, , "consumption"])

  # Committed agricultural uses per crop (in mio. m^3 / yr)
  watComAg <- calcOutput("WaterUseCommittedAg",
                         lpjml = lpjml, climatetype = climatetype,
                         selectyears = selectyears, iniyear = iniyear,
                         multicropping = multicropping, aggregate = FALSE)
  # Total committed agricultural withdrawals (in mio. m^3 / yr)
  comAgWW <- collapseNames(dimSums(watComAg[, , "withdrawal"],
                                   dim = "crop"))
  # Total committed agricultural consumption (in mio. m^3 / yr)
  comAgWC <- collapseNames(dimSums(watComAg[, , "consumption"],
                                   dim = "crop"))

  ## Water inputs
  # Runoff (on land and water)
  magYearlyRunoff <- collapseNames(calcOutput("YearlyRunoff", selectyears = selectyears,
                                              lpjml = lpjml, climatetype = climatetype,
                                              aggregate = FALSE))

  # Lake evaporation as calculated by natural flow river routing
  lakeEvap          <- collapseNames(calcOutput("RiverNaturalFlows", selectyears = selectyears,
                                              lpjml = lpjml, climatetype = climatetype,
                                              aggregate = FALSE)[, , "lake_evap_nat"])

  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(magYearlyRunoff),
                          years = getYears(magYearlyRunoff),
                          names = c(paste("on", getNames(nonAgWWmag), sep = "."),
                                    paste("off", getNames(nonAgWWmag), sep = ".")),
                          fill = 0, sets = c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out     <- object0 + x
    return(out)
  }

  # initialize river routing variables
  inflow    <- as.array(.transformObject(0))
  discharge <- as.array(.transformObject(0))
  missingWW <- missingWC <- as.array(.transformObject(0))

  # bring all inputs to correct object size and transform to array for faster calculation
  yearlyRunoff <- as.array(.transformObject(magYearlyRunoff))
  lakeEvap     <- as.array(.transformObject(lakeEvap))
  comAgWC      <- as.array(.transformObject(comAgWC))
  comAgWW      <- as.array(.transformObject(comAgWW))
  nonAgWC      <- as.array(.transformObject(nonAgWCmag))
  nonAgWW      <- as.array(.transformObject(nonAgWWmag))

  ### Final magpie object structure to be filled
  out <- new.magpie(cells_and_regions = getCells(magYearlyRunoff),
                    years = getYears(magYearlyRunoff),
                    names = c("discharge",
                              # water reserved in current cell (for either local or neighboring cell)
                              # to be considered in following river water use accountings
                              "reservedWW", # (note: was previously required_wat_min, needs to be adjusted)
                              # water available for specified use from local sources and surrounding cells:
                              "currHumanWWtotal", "currHumanWCtotal",
                              # water available for specified use from local sources
                              "currHumanWWlocal", "currHumanWClocal"),
                    sets = c("x.y.iso", "year", "data"),
                    fill = NA)

  ########################################################
  ### River Routing accounting for Human Uses and EFRs ###
  ########################################################
  ## Inputs from previous river routings
  if (humanuse == "non_agriculture") {

    # Minimum flow requirements determined by natural flow river routing:
    # (full) Environmental Flow Requirements (in mio. m^3 / yr) [long-term average]
    prevReservedWW           <- new.magpie(cells_and_regions = getCells(magYearlyRunoff),
                                        years = getYears(magYearlyRunoff),
                                        names = c(paste("on", getNames(nonAgWWmag), sep = "."),
                                                  paste("off", getNames(nonAgWWmag), sep = ".")),
                                        fill = 0)
    prevReservedWW[, , "on"] <- calcOutput("EnvmtlFlowRequirements", lpjml = lpjml, selectyears = selectyears,
                                         climatetype = climatetype, efrMethod = efrMethod,
                                         aggregate = FALSE)[, , "EFR"]
    # Bring to correct object size
    prevReservedWW <- as.array(.transformObject(prevReservedWW))

    ## Current human uses
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currRequestWWlocal <- currRequestWWtotal <- nonAgWW
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currRequestWClocal <- currRequestWCtotal <- nonAgWC

    # There are no previous human uses yet to be considered (empty arrays)
    prevReservedWC <- as.array(.transformObject(0))

  } else if (humanuse == "committed_agriculture") {

    previousHumanUse <- calcOutput("RiverHumanUses", humanuse = "non_agriculture",
                                    lpjml = lpjml, climatetype = climatetype,
                                    efrMethod = efrMethod, multicropping = multicropping,
                                    selectyears = selectyears, iniyear = iniyear,
                                    aggregate = FALSE)

    # Minimum flow requirements determined by previous river routing:
    # Environmental Flow Requirements + Reserved for Non-Agricultural Uses (in mio. m^3 / yr)
    prevReservedWW <- as.array(collapseNames(previousHumanUse[, , "required_wat_min"]))
    ## Previous human uses (determined in non-agricultural uses river routing) (in mio. m^3 / yr):
    prevReservedWC <- as.array(collapseNames(previousHumanUse[, , "currHuman_wc"]))
    ### ToDo: Needs to be updated (account for reserved for neighbors...)

    ## Current human uses
    # Committed Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currRequestWWlocal <- currRequestWWtotal <- comAgWW
    # Committed Water Consumption (in mio. m^3 / yr) [smoothed]
    currRequestWClocal <- currRequestWCtotal <- comAgWC

  } else {
    stop("Please specify for which type of human uses river routing shall be calculated:
         non_agriculture or committed_agriculture")
  }

  ####################################################
  ###### River Routing considering Human Uses ########
  ####################################################

  for (o in 1:max(rs$calcorder)) {
    # Note: the calcorder ensures that the upstreamcells are calculated first
    cells <- which(rs$calcorder == o)

    for (c in cells) {
      tmp <- toolRiverUpDownBalance(c = c, rs = rs,
                                    transDist = transDist,
                                    inLIST = list(yearlyRunoff = yearlyRunoff[c, , , drop = FALSE],
                                                  lakeEvap = lakeEvap[c, , , drop = FALSE],
                                                  prevReservedWW = prevReservedWW,
                                                  prevReservedWC = prevReservedWC[c, , , drop = FALSE]),
                                    inoutLIST = list(discharge = discharge,
                                                  inflow = inflow,
                                                  currRequestWClocal = currRequestWClocal,
                                                  currRequestWWlocal = currRequestWWlocal))
      # Updated flows
      discharge <- tmp$discharge
      inflow    <- tmp$inflow
      currRequestWClocal <- tmp$currRequestWClocal
      currRequestWWlocal <- tmp$currRequestWWlocal

      # Locally missing water that might be fulfilled by surrounding cells
      missingWW[c, , ] <- currRequestWWtotal[c, , , drop = FALSE] -
                            currRequestWWlocal[c, , , drop = FALSE]
      missingWC[c, , ] <- currRequestWCtotal[c, , , drop = FALSE] -
                            currRequestWClocal[c, , , drop = FALSE]
    }
  }

  # Update minimum water required in cell (for further river processing steps):
  prevReservedWW[, , ] <- prevReservedWW[, , , drop = FALSE] +
                            currRequestWWlocal[, , , drop = FALSE]

  # What should be reported by calcHumanUseAccounting?
  # 1. discharge
  # 2. reserved flows (reservedWW (-> replace required_wat_min), reservedWC)
  # 3. currHumanWWlocal, currHumanWClocal (locally reserved water for specific use (non-ag or com-ag))
  # 4. currHumanWWtotal, currHumanWCtotal (total water that can be used for specific use (non-ag or com-ag) when considering neighbor water provision)
  # 5. toNeighborWW, toNeighborWC (previously called: resNeighborWW, resNeighborWC) (water that is reserved in current cell for use in another cell)
  # 6. fromNeighborWW, fromNeighborWC (should be same as currHumanWWtotal - currHumanWWlocal)

  # If local water is not sufficient:
  # may be fulfilled by surrounding cell water provision
  if (transDist != 0) {
    tmp <- toolNeighborUpDownProvision(rs = rs, transDist = transDist,
                       listNeighborIN = list(yearlyRunoff = yearlyRunoff, 
                                              lakeEvap = lakeEvap,
                                              prevReservedWC = prevReservedWC,
                                              prevReservedWW = prevReservedWW,
                                              missingWC = missingWC,
                                              missingWW = missingWW))
  
    # Update minimum water required in cell (for further river processing steps):
    prevReservedWW[, , ] <- prevReservedWW[, , , drop = FALSE] +
                              tmp$toNeighborWW[, , , drop = FALSE]

    # Update currently fulfilled water use
    currRequestWWtotal <- currRequestWWlocal + tmp$fromNeighborWW
    currRequestWCtotal <- currRequestWClocal + tmp$fromNeighborWC

    ### Checks ###
    # All reserved water needs to end up somewhere
    toNeighborWW <- as.magpie(tmp$toNeighborWW, spatial = 1, temporal = 2)
    toNeighborWC <- as.magpie(tmp$toNeighborWC, spatial = 1, temporal = 2)
    fromNeighborWW <- as.magpie(tmp$fromNeighborWW, spatial = 1, temporal = 2)
    fromNeighborWC <- as.magpie(tmp$fromNeighborWC, spatial = 1, temporal = 2)

    if (round(dimSums(toNeighborWW, dim = 1) - dimSums(fromNeighborWW, dim = 1), digits = 4) != 0) {
      if (any(dimSums(toNeighborWW, dim = 1) - dimSums(fromNeighborWW, dim = 1) > 0)) {
        warning(paste0("More water was provided to requesting main cells than 
                      was reserved in neighboring cells
                      in Neighbor Water Provision of calcRiverHumanUseAccounting"))
      }
      if (any(dimSums(toNeighborWW, dim = 1) - dimSums(fromNeighborWW, dim = 1) < 0)) {
        warning(paste0("More water was reserved in neighboring cells than 
                        was provided to requesting main cells
                        in Neighbor Water Provision of calcRiverHumanUseAccounting"))
      }
      stop(paste0("After Neighbor Water Provision in calcRiverHumanUseAccounting with humanuse = ",
                  humanuse, " some water was not properly allocated.
                  See warnings() for further information."))
    }
    if (round(dimSums(toNeighborWC, dim = 1) - dimSums(fromNeighborWC, dim = 1), digits = 4) != 0) {
      if (any(dimSums(toNeighborWC, dim = 1) - dimSums(fromNeighborWC, dim = 1) > 0)) {
        warning(paste0("More water was provided to requesting main cells than
                      was reserved in neighboring cells
                      in Neighbor Water Provision of calcRiverHumanUseAccounting"))
      }
      if (any(dimSums(toNeighborWC, dim = 1) - dimSums(fromNeighborWC, dim = 1) < 0)) {
        warning(paste0("More water was reserved in neighboring cells than
                      was provided to requesting main cells
                      in Neighbor Water Provision of calcRiverHumanUseAccounting"))
      }
      stop(paste0("After Neighbor Water Provision in calcRiverHumanUseAccounting with humanuse = ",
                  humanuse, " some water was not properly allocated.
                  See warnings() for further information."))
    }
  } else {
  # total and local currently requested water fulfilled is the same in this case
  currRequestWWtotal <- currRequestWWlocal
  currRequestWCtotal <- currRequestWClocal
  }

  ###############
  ### Outputs ###
  ###############
  # river discharge flows
  out[, , "discharge"] <- as.magpie(discharge, spatial = 1, temporal = 2)
  # water reserved in current cell (for either local or neighboring cell)
  # to be considered in following river water use accountings
  out[, , "reservedWW"] <- as.magpie(prevReservedWW, spatial = 1, temporal = 2)
  # water use that is fulfilled locally
  out[, , "currHumanWClocal"] <- as.magpie(currRequestWClocal, spatial = 1, temporal = 2)
  out[, , "currHumanWWlocal"] <- as.magpie(currRequestWWlocal, spatial = 1, temporal = 2)
  # local water use that is fulfilled by local water resources or by surrounding water available
  out[, , "currHumanWCtotal"] <- as.magpie(currRequestWCtotal, spatial = 1, temporal = 2)
  out[, , "currHumanWWtotal"] <- as.magpie(currRequestWWtotal, spatial = 1, temporal = 2)

  # ToDo: change names
  # before: names = c("required_wat_min", "discharge", "currHuman_ww", "currHuman_wc"),

  ##############
  ### Checks ###
  ##############
  if (any(is.na(out))) {
    stop("calcRiverHumanUseAccounting produced NAs!")
  }
  if (any(round(out) < 0)) {
    stop("calcRiverHumanUseAccounting produced negative values")
  }
  # No water should be lost
  natDischarge <- collapseNames(calcOutput("RiverNaturalFlows",
                                            selectyears = selectyears,
                                            lpjml = lpjml, climatetype = climatetype,
                                            aggregate = FALSE)[, , "discharge_nat"])
  basinDischarge <- natDischarge
  basinDischarge[, , ] <- 0
  basinDischarge[unique(rs$endcell), , ] <- out[unique(rs$endcell), , "discharge"]
  totalWat <- dimSums(basinDischarge, dim = 1) + dimSums(out[, , c("currHumanWCtotal")],
                                                          dim = c("x", "y", "iso", "data"))
  # Total water (summed basin discharge + consumed)
  # must be identical across scenarios
  if (!all(abs(totalWat - mean(totalWat)) < 1e-06)) {
    stop("In calcRiverHumanUseAccounting:
          Scenarios differ. That should not be the case.
          Total water volume should always be the same")
  }
  # Total water (summed basin discharge + consumed)
  # must be same as natural summed basin discharge
  if (any(round(dimSums(natDischarge[unique(rs$endcell), , ],
                        dim = 1) - totalWat[, , 1],
                digits = 6) != 0)) {
    stop("In calcRiverHumanUseAccounting:
          Water has been lost during the Neighbor Water Provision Algorithm")
  }

  # Description
  description <- paste0("Upstream-downstream river routing outputs taking 
                         human uses (", humanuse, ") into account")

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
