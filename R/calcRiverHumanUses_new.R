#' @title       calcRiverHumanUses_new
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
#' @param transDist     Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand
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
#' calcOutput("RiverHumanUses_new", aggregate = FALSE)
#' }
#'
calcRiverHumanUses_new <- function(humanuse, lpjml, climatetype, selectyears,
                               iniyear, efrMethod, multicropping, transDist) {

  # Read in river structure
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
  watNonAg <- calcOutput("WaterUseNonAg", selectyears = 2010, cells = "lpjcell",
                          datasource = "WATERGAP_ISIMIP", usetype = "total",
                          seasonality = "total", harmonType = "average",
                          lpjml = NULL, climatetype = NULL, aggregate = FALSE)

  inputNonAgWW  <- collapseNames(watNonAg[, , "withdrawal"])
  inputNonAgWC  <- collapseNames(watNonAg[, , "consumption"])

  # Committed agricultural uses per crop (in mio. m^3 / yr)
  watComAg <- calcOutput("WaterUseCommittedAg",
                         lpjml = lpjml, climatetype = climatetype,
                         selectyears = selectyears, iniyear = iniyear,
                         multicropping = multicropping, aggregate = FALSE)
  # Total committed agricultural withdrawals (in mio. m^3 / yr)
  watComAgWW <- collapseNames(dimSums(watComAg[, , "withdrawal"],  dim = 3))
  # Total committed agricultural consumption (in mio. m^3 / yr)
  watComAgWC <- collapseNames(dimSums(watComAg[, , "consumption"], dim = 3))

  ## Water inputs
  # Runoff (on land and water)
  inputYearlyRunoff <- collapseNames(calcOutput("YearlyRunoff", selectyears = selectyears,
                                              lpjml = lpjml, climatetype = climatetype,
                                              aggregate = FALSE))

  # Lake evaporation as calculated by natural flow river routing
  naturalFlows <- calcOutput("RiverNaturalFlows",
                              selectyears = selectyears,
                              lpjml = lpjml, climatetype = climatetype,
                              aggregate = FALSE)
  lakeEvap     <- collapseNames(naturalFlows[, , "lake_evap_nat"])

  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(inputYearlyRunoff),
                          years = getYears(inputYearlyRunoff),
                          names = c(paste("on", getNames(inputNonAgWW), sep = "."),
                                    paste("off", getNames(inputNonAgWW), sep = ".")),
                          fill = 0, sets = c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out     <- object0 + x
    return(out)
  }

  # bring all inputs to correct object size and transform to array for faster calculation
  lakeEvap     <- as.array(.transformObject(lakeEvap))
  watComAgWC   <- as.array(.transformObject(watComAgWC))
  watComAgWW   <- as.array(.transformObject(watComAgWW))
  nonAgWC      <- as.array(.transformObject(inputNonAgWC))
  nonAgWW      <- as.array(.transformObject(inputNonAgWW))
  yearlyRunoff <- as.array(.transformObject(inputYearlyRunoff))

  # helper variables for river routing
  inflow     <- as.array(.transformObject(0))
  avlWat     <- as.array(.transformObject(0))
  upstreamWC <- as.array(.transformObject(0))

  # output variable that will be filled during river routing
  discharge           <- as.array(.transformObject(0))
  resNeighborWC       <- resNeighborWW <- as.array(.transformObject(0))
  neighborProvisionWC <- neighborProvisionWW <- as.array(.transformObject(0))

  ########################################################
  ### River Routing accounting for Human Uses and EFRs ###
  ########################################################
  ## Inputs from previous river routings
  if (humanuse == "non_agriculture") {

    # Minimum flow requirements determined by natural flow river routing:
    # (full) Environmental Flow Requirements (in mio. m^3 / yr) [long-term average]
    watReserved           <- new.magpie(cells_and_regions = getCells(yearlyRunoff),
                                        years = getYears(yearlyRunoff),
                                        names = c("on", "off"),
                                        fill = 0)
    watReserved[, , "on"] <- calcOutput("EnvmtlFlowRequirements", lpjml = lpjml, selectyears = selectyears,
                                         climatetype = climatetype, efrMethod = efrMethod,
                                         aggregate = FALSE)[, , "EFR"]
    # Bring to correct object size
    watReserved <- as.array(.transformObject(watReserved))

    ## Current human uses
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currHumanWWlocal <- currHumanWWtotal <- nonAgWW
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currHumanWClocal <- currHumanWCtotal <- nonAgWC

    # There are no previous human uses yet to be considered (empty arrays)
    prevHumanWC <- as.array(.transformObject(0))

  } else if (humanuse == "committed_agriculture") {

    previousHumanUse <- calcOutput("RiverHumanUses_new", humanuse = "non_agriculture",
                                    lpjml = lpjml, climatetype = climatetype,
                                    efrMethod = efrMethod, multicropping = multicropping,
                                    selectyears = selectyears, iniyear = iniyear,
                                    transDist = transDist,
                                    aggregate = FALSE)

    # Minimum flow requirements determined by previous river routing:
    # Environmental Flow Requirements + Reserved for Non-Agricultural Uses (in mio. m^3 / yr)
    watReserved <- as.array(collapseNames(previousHumanUse[, , "required_wat_min"]))
    ## Previous human uses (determined in non-agricultural uses river routing) (in mio. m^3 / yr):
    prevHumanWC <- as.array(collapseNames(previousHumanUse[, , "currHuman_wc_local"]))

    ## Current human uses
    # Committed Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currHumanWWlocal <- currHumanWWtotal <- watComAgWW
    # Committed Water Consumption (in mio. m^3 / yr) [smoothed]
    currHumanWClocal <- currHumanWCtotal <- watComAgWC

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

      # Available water in cell
      avlWat[c, , ] <- inflow[c, , , drop = FALSE] +
                         yearlyRunoff[c, , , drop = FALSE] -
                           lakeEvap[c, , , drop = FALSE]

      ### Is there sufficient water available to fulfill previously determined requirements? ###
      sufficientWat <- (avlWat[c, , , drop = FALSE] >= watReserved[c, , , drop = FALSE])

      #### (1) Available Water in cell is sufficient to fulfill previously determined requirements ####
      ####      -> further withdrawals possible                                                    ####

      # current withdrawals requested?
      isWWreq  <- (currHumanWWlocal[c, , , drop = FALSE] > 0)
      # combined conditions
      isSuffWW <- sufficientWat & isWWreq

      # (I) Water withdrawal constraint: All withdrawals that can be fulfilled considering
      #                                  local previously determined water requirements are served
      fracWWconstraint <- pmin((avlWat[c, , , drop = FALSE] -
                                 watReserved[c, , , drop = FALSE])[isSuffWW] /
                                   currHumanWWlocal[c, , , drop = FALSE][isSuffWW],
                                 1)
      # Current water uses (withdrawals and consumption) given withdrawal constraint
      currHumanWClocal[c, , ][isSuffWW] <- fracWWconstraint * (currHumanWClocal[c, , ,
                                                                      drop = FALSE])[isSuffWW]
      currHumanWWlocal[c, , ][isSuffWW] <- fracWWconstraint * (currHumanWWlocal[c, , ,
                                                                      drop = FALSE])[isSuffWW]

      # Discharge in current cell for case where sufficient water available for requirements
      # (Subtract local water consumption in current cell (and previous if applicable)
      discharge[c, , ][sufficientWat] <- (avlWat[c, , , drop = FALSE] -
                                             currHumanWClocal[c, , , drop = FALSE] -
                                                prevHumanWC[c, , , drop = FALSE])[sufficientWat]

      #### (2) Available Water in cell is not sufficient to fulfill previously determined requirements ####
      ####      -> no more water can be withdrawn locally (A + B)                                      ####
      ####      &  if possible: upstream consumption is reduced to release missing water    (A)        ####
      ####         if not: nothing can be consumed upstream (upstream consumption set to 0) (B)        ####

      # No water withdrawals locally if available water is not sufficient to fulfill
      # previously determined requirements
      currHumanWClocal[c, , ][!sufficientWat] <- 0
      currHumanWWlocal[c, , ][!sufficientWat] <- 0

      # Update upstream cells' current consumption:
      if (length(rs$upstreamcells[[c]]) > 0) {

        # vector of upstreamcells of c
        up <- unlist(rs$upstreamcells[[c]])
        # vector of c in length of upstreamcells of c
        lc <- rep(c, length(rs$upstreamcells[[c]]))
        # vector of 1 in length of upstreamcells of c
        cc <- rep(1, length(rs$upstreamcells[[c]]))

        # Determine upstream current water consumption:
        upstreamWC[c, , ]  <- colSums(currHumanWClocal[up, , , drop = FALSE], dims = 1)

        ### (A or B) Is there current upstream water consumption that can be reduced  ###
        ###          to release water required by previous (prioritary) uses?         ###
        sufficientUP <- (upstreamWC[c, , , drop = FALSE] > (watReserved[c, , , drop = FALSE] -
                                                              avlWat[c, , , drop = FALSE]))
        # Combinations of conditions
        fromUPcc <- (!sufficientWat[cc, , , drop = FALSE] & sufficientUP[cc, , , drop = FALSE])
        fromUPc  <- (!sufficientWat & sufficientUP)
        insuffWATcc <- (!sufficientWat[cc, , , drop = FALSE] & !sufficientUP[cc, , , drop = FALSE])
        insuffWATc  <- (!sufficientWat & !sufficientUP)

        ## (A) upstreamWC high enough to release required water: ##
        ## -> reduce upstream consumption respectively           ##
        # (II) Water consumption constraint: If water is required by priority
        #                                    use in downstream cell, cannot have
        #                                    been consumed for current use upstream
        # fraction that stays in upstream cell(s) = 1 - fraction of water that
        #                                               needs to be released by each upstream cell
        fracWCconstraint <- (1 - (watReserved[lc, , , drop = FALSE] -
                                      avlWat[lc, , , drop = FALSE])[fromUPcc] /
                                        upstreamWC[lc, , , drop = FALSE][fromUPcc])

        # Current human uses (consumption and withdrawal) in upstreamcells is reduced by respective amount
        currHumanWClocal[up, , ][fromUPcc] <- fracWCconstraint * (currHumanWClocal[up, , , drop = FALSE])[fromUPcc]
        currHumanWWlocal[up, , ][fromUPcc] <- fracWCconstraint * (currHumanWWlocal[up, , , drop = FALSE])[fromUPcc]

        # Discharge in current cell when water not sufficient to fulfill requirements,
        # but missing water water requirements can be fulfilled by upstream cells (A)
        discharge[c, , ][fromUPc]  <-  (watReserved[c, , , drop = FALSE] -
                                          prevHumanWC[c, , , drop = FALSE])[fromUPc]

        ## (B) upstreamWC not high enough to release required water:
        ## -> no water can be used upstream
        # Current human uses (consumption and withdrawal) in upstreamcells are set to 0
        currHumanWClocal[up, , ][insuffWATcc] <- 0
        currHumanWWlocal[up, , ][insuffWATcc] <- 0

        # Discharge in current cell when water not sufficient to fulfill requirements
        # and missing water water requirements cannot be fulfilled by upstream cells
        # (since there is no upstream consumption, this water is additionally available in the current cell)
        discharge[c, , ][insuffWATc] <- (avlWat[c, , , drop = FALSE] +
                                           upstreamWC[c, , , drop = FALSE] -
                                             prevHumanWC[c, , , drop = FALSE])[insuffWATc]

      } else {

        # Discharge when water is not sufficient to fulfill previously (priority)
        # requirements and there are no upstream cells
        discharge[c, , ][!sufficientWat]  <- (avlWat[c, , , drop = FALSE] -
                                                 prevHumanWC[c, , , drop = FALSE])[!sufficientWat]

      }

      # Inflow to nextcell (if there is a downstreamcell)
      if (rs$nextcell[c] > 0) {
        inflow[rs$nextcell[c], , ] <- inflow[rs$nextcell[c], , , drop = FALSE] +
                                         discharge[c, , , drop = FALSE]
      }

      # Locally missing water that might be fulfilled by surrounding cells
      missingWW <- currHumanWWtotal[c, , , drop = FALSE] - currHumanWWlocal[c, , , drop = FALSE]
      missingWC <- currHumanWCtotal[c, , , drop = FALSE] - currHumanWClocal[c, , , drop = FALSE]

      if (any(missingWW > 0) || any(missingWC > 0)) {

        inoutLIST <- list(discharge = discharge,
                          watReserved = watReserved,
                          resNeighborWC = resNeighborWC,
                          resNeighborWW = resNeighborWW,
                          neighborProvisionWW = neighborProvisionWW,
                          neighborProvisionWC = neighborProvisionWC)

        tmp <- toolNeighborWaterProvision(transDist = transDist, c = c, rs = rs,
                                          missingWW = missingWW, missingWC = missingWC,
                                          inoutLIST = inoutLIST)
        discharge   <- tmp$discharge
        watReserved <- tmp$watReserved
        resNeighborWC <- tmp$resNeighborWC
        resNeighborWW <- tmp$resNeighborWW
        neighborProvisionWW <- tmp$neighborProvisionWW
        neighborProvisionWC <- tmp$neighborProvisionWC
      }
    }
  }

  # Update minimum water required in cell (for further river processing steps):
  watReserved[, , ] <- watReserved[, , , drop = FALSE] +
                         currHumanWWlocal[, , , drop = FALSE]

  # Report water that is available for human consumption/withdrawal
  # when considering neighbor water provision
  currHumanWWtotal <- currHumanWWlocal + neighborProvisionWW
  currHumanWCtotal <- currHumanWClocal + neighborProvisionWC

  ########################
  ### Output Variables ###
  ########################
  out <- new.magpie(cells_and_regions = getCells(watReserved),
                    years = getYears(watReserved),
                    names = c("required_wat_min", "discharge",
                              "resNeighbor_ww", "resNeighbor_wc",
                              "currHuman_ww_local", "currHuman_wc_local",
                              "currHuman_ww_total", "currHuman_wc_total"),
                    sets = c("x.y.iso", "year", "data"))
  out <- .transformObject(out)
  out[, , "required_wat_min"]   <- as.magpie(watReserved, spatial = 1, temporal = 2)
  out[, , "currHuman_ww_local"] <- as.magpie(currHumanWWlocal, spatial = 1, temporal = 2)
  out[, , "currHuman_wc_local"] <- as.magpie(currHumanWClocal, spatial = 1, temporal = 2)
  out[, , "currHuman_ww_total"] <- as.magpie(currHumanWWtotal, spatial = 1, temporal = 2)
  out[, , "currHuman_wc_total"] <- as.magpie(currHumanWCtotal, spatial = 1, temporal = 2)
  out[, , "resNeighbor_ww"]     <- as.magpie(resNeighborWW, spatial = 1, temporal = 2)
  out[, , "resNeighbor_wc"]     <- as.magpie(resNeighborWC, spatial = 1, temporal = 2)
  out[, , "discharge"]          <- as.magpie(discharge, spatial = 1, temporal = 2)
  description <- paste0("river routing outputs taking human uses (", humanuse, ") into account")

  #############
  ### Tests ###
  #############
  # Check for NAs and negative values
  if (any(is.na(out))) {
    stop("calcRiverHumanUses produced NAs")
  }
  if (any(round(out, digits = 4) < 0)) {
    stop("calcRiverHumanUses produced negative values")
  }
  # Should be the same:
  a <- dimSums(out[, , c("currHuman_wc_local", "resNeighbor_wc")], dim = c("x", "y", "iso", "data"))
  b <- dimSums(out[, , c("currHuman_wc_total")], dim = c("x", "y", "iso", "data"))
  if (any(summary(a - b) != 0)) {
      if (any(summary(a - b) > 0)) {
        stop(paste0("Something is wrong with neighbor water provision algorithm
        in calcRiverHumanUses:
        some of neighbor provision is not reported to total!
        The mismatch ranges between ", summary(a - b)))
      }
      if (any(summary(a - b) < 0)) {
        stop(paste0("Something is wrong with neighbor water provision algorithm
        in calcRiverHumanUses:
        more water is reported to total than is provided by neighbor cells!
        The mismatch ranges between ", summary(a - b)))
      }
  }
  # No water should be lost
  natDischarge       <- collapseNames(out[, , "discharge"])
  natDischarge[, , ] <- collapseNames(naturalFlows[, , "discharge_nat"])

  basinDischarge       <- natDischarge
  basinDischarge[, , ] <- 0
  basinDischarge[unique(rs$endcell), , ] <- out[unique(rs$endcell), , "discharge"]
  totalWat <- dimSums(basinDischarge, dim = 1) + b

  # Total water (summed basin discharge + consumed)
  # must be identical across scenarios
  if (all(abs(totalWat - mean(totalWat)) == 0)) {
    stop("Scenarios differ. That should not be the case.
          Total water volume should always be the same")
  }
  # Total water (summed basin discharge + consumed)
  # must be same as natural summed basin discharge
  if (any(round(dimSums(natDischarge[unique(rs$endcell), , ],
                        dim = 1) - totalWat[, , 1],
                digits = 6) != 0)) {
    stop("In calcRiverHumanUses:
          Water has been lost during the Neighbor Water Provision Algorithm")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
