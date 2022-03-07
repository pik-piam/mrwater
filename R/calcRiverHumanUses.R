#' @title       calcRiverHumanUses
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
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
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
#' calcOutput("RiverHumanUses", aggregate = FALSE)
#' }
#'
calcRiverHumanUses <- function(humanuse, lpjml, climatetype, selectyears,
                               iniyear, efrMethod, multicropping) {

  ### Read in river structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))

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
  inputYearlyRunoff <- collapseNames(calcOutput("YearlyRunoff",      selectyears = selectyears,
                                              lpjml = lpjml, climatetype = climatetype, aggregate = FALSE))

  # Lake evaporation as calculated by natural flow river routing
  lakeEvap          <- collapseNames(calcOutput("RiverNaturalFlows", selectyears = selectyears,
                                              lpjml = lpjml, climatetype = climatetype,
                                              aggregate = FALSE)[, , "lake_evap_nat"])

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
  lakeEvap      <- as.array(.transformObject(lakeEvap))
  watComAgWC    <- as.array(.transformObject(watComAgWC))
  watComAgWW    <- as.array(.transformObject(watComAgWW))
  nonAgWC       <- as.array(.transformObject(inputNonAgWC))
  nonAgWW       <- as.array(.transformObject(inputNonAgWW))
  yearlyRunoff  <- as.array(.transformObject(inputYearlyRunoff))

  # helper variables for river routing
  inflow        <- as.array(.transformObject(0))
  avl_wat_act   <- as.array(.transformObject(0))
  upstreamWC    <- as.array(.transformObject(0))

  # output variable that will be filled during river routing
  discharge     <- as.array(.transformObject(0))


  ########################################################
  ### River Routing accounting for Human Uses and EFRs ###
  ########################################################
  ## Inputs from previous river routings
  if (humanuse == "non_agriculture") {

    # Minimum flow requirements determined by natural flow river routing:
    # (full) Environmental Flow Requirements (in mio. m^3 / yr) [long-term average]
    watReserved         <- new.magpie(cells_and_regions = getCells(yearlyRunoff),
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
    currHumanWW <- nonAgWW
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currHumanWC <- nonAgWC

    # There are no previous human uses yet to be considered (empty arrays)
    prevHumanWC <- as.array(.transformObject(0))

  } else if (humanuse == "committed_agriculture") {

    prevHuman_routing <- calcOutput("RiverHumanUses", humanuse = "non_agriculture",
                                    lpjml = lpjml, climatetype = climatetype,
                                    efrMethod = efrMethod, multicropping = multicropping,
                                    selectyears = selectyears, iniyear = iniyear,
                                    aggregate = FALSE)

    # Minimum flow requirements determined by previous river routing:
    # Environmental Flow Requirements + Reserved for Non-Agricultural Uses (in mio. m^3 / yr)
    watReserved <- as.array(collapseNames(prevHuman_routing[, , "required_wat_min"]))
    ## Previous human uses (determined in non-agricultural uses river routing) (in mio. m^3 / yr):
    prevHumanWC <- as.array(collapseNames(prevHuman_routing[, , "currHuman_wc"]))

    ## Current human uses
    # Committed Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currHumanWW <- watComAgWW
    # Committed Water Consumption (in mio. m^3 / yr) [smoothed]
    currHumanWC <- watComAgWC

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
      avl_wat_act[c, , ] <- inflow[c, , , drop = F] + yearlyRunoff[c, , , drop = F] - lakeEvap[c, , , drop = F]

      ### Is there sufficient water available to fulfill previously determined requirements? ###
      sufficient_water   <- (avl_wat_act[c, , , drop = F] >= watReserved[c, , , drop = F])

      #### (1) Available Water in cell is sufficient to fulfill previously determined requirements     ####
      ####     (avl_wat_act[c,,] >= watReserved[c,,])                                          ####
      ####      -> further withdrawals possible                                                        ####

      # current withdrawals requested?
      withdrawals        <- (currHumanWW[c, , , drop = F] > 0)

      # (I) Water withdrawal constraint: All withdrawals that can be fulfilled considering
      #                                  local previously determined water requirements are served
      frac_ww_constraint <- pmin((avl_wat_act[c, , , drop = F] -
                                    watReserved[c, , , drop = F])[sufficient_water[, , , drop = F] & withdrawals[, , , drop = F]] /
                                   currHumanWW[c, , , drop = F][sufficient_water[, , , drop = F] & withdrawals[, , , drop = F]],
                                 1)
      # Current water uses (withdrawals and consumption) given withdrawal constraint
      currHumanWC[c, , ][sufficient_water[, , , drop = F] & withdrawals[, , , drop = F]] <- frac_ww_constraint * (currHumanWC[c, , , drop = F])[sufficient_water[, , , drop = F] & withdrawals[, , , drop = F]]
      currHumanWW[c, , ][sufficient_water[, , , drop = F] & withdrawals[, , , drop = F]] <- frac_ww_constraint * (currHumanWW[c, , , drop = F])[sufficient_water[, , , drop = F] & withdrawals[, , , drop = F]]

      # Discharge in current cell for case where sufficient water available for requirements
      # (Subtract local water consumption in current cell (and previous if applicable)
      discharge[c, , ][sufficient_water[, , , drop = F]] <- (avl_wat_act[c, , , drop = F] - currHumanWC[c, , , drop = F] - prevHumanWC[c, , , drop = F])[sufficient_water[, , , drop = F]]

      #### (2) Available Water in cell is not sufficient to fulfill previously determined requirements ####
      ####     (avl_wat_act[c,,] < watReserved[c,,])                                           ####
      ####      -> no more water can be withdrawn locally (A + B)                                      ####
      ####      &  if possible: upstream consumption is reduced to release missing water    (A)        ####
      ####         if not: nothing can be consumed upstream (upstream consumption set to 0) (B)        ####

      # No water withdrawals locally if available water is not sufficient to fulfill
      # previously determined requirements
      currHumanWC[c, , ][!sufficient_water[, , , drop = F]] <- 0
      currHumanWW[c, , ][!sufficient_water[, , , drop = F]] <- 0

      # Update upstream cells' current consumption:
      if (length(rs$upstreamcells[[c]]) > 0) {

        # vector of upstreamcells of c
        up <- unlist(rs$upstreamcells[[c]])
        # vector of c in length of upstreamcells of c
        lc <- rep(c, length(rs$upstreamcells[[c]]))
        # vector of 1 in length of upstreamcells of c
        cc <- rep(1:length(c), length(rs$upstreamcells[[c]]))

        # Determine upstream current water consumption:
        upstreamWC[c, , ]  <- colSums(currHumanWC[up, , , drop = F], dims = 1)

        ### (A or B) Is there current upstream water consumption that can be reduced  ###
        ###          to release water required by previous (prioritary) uses?         ###
        sufficient_upstream <- (upstreamWC[c, , , drop = F] > (watReserved[c, , , drop = F] -
                                                                    avl_wat_act[c, , , drop = F]))

        ## (A) upstreamWC high enough to release required water: ##
        ## -> reduce upstream consumption respectively              ##
        # (II) Water consumption constraint: If water is required by priority
        #                                    use in downstream cell, cannot have
        #                                    been consumed for current use upstream
        # fraction that stays in upstream cell(s) = 1 - fraction of water that needs to be released by each upstream cell
        frac_wc_constraint <- (1 - (watReserved[lc, , , drop = F] -
                                      avl_wat_act[lc, , , drop = F])[!sufficient_water[cc, , , drop = F] & sufficient_upstream[cc, , , drop = F]] /
                                 upstreamWC[lc, , , drop = F][!sufficient_water[cc, , , drop = F] & sufficient_upstream[cc, , , drop = F]])

        # Current human uses (consumption and withdrawal) in upstreamcells is reduced by respective amount
        currHumanWC[up, , ][!sufficient_water[cc, , , drop = F] & sufficient_upstream[cc, , , drop = F]] <- frac_wc_constraint * (currHumanWC[up, , , drop = F])[!sufficient_water[cc, , , drop = F] & sufficient_upstream[cc, , , drop = F]]
        currHumanWW[up, , ][!sufficient_water[cc, , , drop = F] & sufficient_upstream[cc, , , drop = F]] <- frac_wc_constraint * (currHumanWW[up, , , drop = F])[!sufficient_water[cc, , , drop = F] & sufficient_upstream[cc, , , drop = F]]

        # Discharge in current cell when water not sufficient to fulfill requirements,
        # but missing water water requirements can be fulfilled by upstream cells (A)
        discharge[c, , ][!sufficient_water[, , , drop = F] & sufficient_upstream[, , , drop = F]]  <-  (watReserved[c, , , drop = F] - prevHumanWC[c, , , drop = F])[!sufficient_water[, , , drop = F] & sufficient_upstream[, , , drop = F]]

        ## (B) upstreamWC not high enough to release required water:
        ## -> no water can be used upstream
        # Current human uses (consumption and withdrawal) in upstreamcells are set to 0
        currHumanWC[up, , ][!sufficient_water[cc, , , drop = F] & !sufficient_upstream[cc, , , drop = F]] <- 0
        currHumanWW[up, , ][!sufficient_water[cc, , , drop = F] & !sufficient_upstream[cc, , , drop = F]] <- 0

        # Discharge in current cell when water not sufficient to fulfill requirements
        # and missing water water requirements cannot be fulfilled by upstream cells
        # (since there is not upstream consumption, this water is additionally available in the current cell)
        discharge[c, , ][!sufficient_water[, , , drop = F] & !sufficient_upstream[, , , drop = F]] <- (avl_wat_act[c, , , drop = F] + upstreamWC[c, , , drop = F] - prevHumanWC[c, , , drop = F])[!sufficient_water[, , , drop = F] & !sufficient_upstream[, , , drop = F]]

      } else {

        # Discharge when water is not sufficient to fulfill previously (priority)
        # requirements and there are no upstream cells
        discharge[c, , ][!sufficient_water[, , , drop = F]]  <- (avl_wat_act[c, , , drop = F] -
                                                                   prevHumanWC[c, , , drop = F])[!sufficient_water[, , , drop = F]]

      }

      # Inflow to nextcell (if there is a downstreamcell)
      if (rs$nextcell[c] > 0) {
        inflow[rs$nextcell[c], , ] <- inflow[rs$nextcell[c], , , drop = F] + discharge[c, , , drop = F]
      }
    }
  }

  # Update minimum water required in cell (for further river processing steps):
  watReserved[, , ] <- watReserved[, , , drop = F] + currHumanWW[, , , drop = F]

  ########################
  ### Output Variables ###
  ########################
  out <- new.magpie(cells_and_regions = getCells(watReserved),
                    years = getYears(watReserved),
                    names = c("required_wat_min", "currHuman_ww", "currHuman_wc"),
                    sets = c("x.y.iso", "year", "data"))
  out <- .transformObject(out)
  out[, , "required_wat_min"] <- as.magpie(watReserved, spatial = 1, temporal = 2)
  out[, , "currHuman_ww"]     <- as.magpie(currHumanWW, spatial = 1, temporal = 2)
  out[, , "currHuman_wc"]     <- as.magpie(currHumanWC, spatial = 1, temporal = 2)
  description <- paste0("river routing outputs taking human uses (", humanuse, ") into account")

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
