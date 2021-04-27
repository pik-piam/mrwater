#' @title       calcRiverHumanUses
#' @description This function calculates natural discharge for the river routing derived from inputs from LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param humanuse    Human use type to which river routing shall be applied (non_agriculture or committed_agriculture). Note: non_agriculture must be run prior to committed_agriculture
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear     Initialization year of irrigation system
#' @param EFRmethod   EFR method used including selected strictness of EFRs (Smakhtin:good, VMF:fair)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#' @import mrmagpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverHumanUses", aggregate = FALSE) }
#'

calcRiverHumanUses <- function(selectyears, humanuse, iniyear, climatetype, EFRmethod) {
  # # # # # # # # # # #
  # # # READ IN DATA # #
  # # # # # # # # # # #
  ### Read in river structure
  # Note: river structure derived from LPJmL input (drainage) [maybe later: implement readDrainage function]
  # Information contained:
  ## upstreamcells:   all cells that are upstream of current cell (list of cells)
  ## downstreamcells: all cells that are downstream of current cell (list of cells)
  ## nextcell:        cell to which discharge of current cell flows (exactly 1 cell)
  ## endcell:         estuary cell of current cell, i.e. last cell of the river of which current cell is part of (exactly 1 cell)
  ## calcorder:       ordering of cells for calculation from upstream to downstream
  ## cells:           LPJmL cell ordering with ISO code
  ## coordinates:     coordinate data of cells
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))

  ## Human uses
  # Non-Agricultural Water Withdrawals and Consumption (in mio. m^3 / yr) [smoothed]
  wat_nonag <- calcOutput("WaterUseNonAg", source="WATERGAP2020", seasonality="total", aggregate=FALSE, selectyears=selectyears, climatetype=NULL, lpjml=NULL, harmon_base_time="average")
  I_NAg_ww  <- collapseNames(wat_nonag[,,"withdrawal"])
  I_NAg_wc  <- collapseNames(wat_nonag[,,"consumption"])

  # Committed agricultural uses per crop (in mio. m^3 / yr)
  CAU_magpie <- calcOutput("WaterUseCommittedAg", aggregate=FALSE, selectyears=selectyears, climatetype=climatetype, iniyear=iniyear)
  # Total committed agricultural withdrawals (in mio. m^3 / yr)
  CAW_magpie <- collapseNames(dimSums(CAU_magpie[,,"withdrawal"],  dim=3))
  # Total committed agricultural consumption (in mio. m^3 / yr)
  CAC_magpie <- collapseNames(dimSums(CAU_magpie[,,"consumption"], dim=3))

  ## Water inputs
  # Runoff (on land and water)
  I_yearly_runoff <- collapseNames(calcOutput("YearlyRunoff",      aggregate=FALSE, selectyears=selectyears, climatetype=climatetype))

  # Lake evaporation as calculated by natural flow river routing
  lake_evap_new   <- collapseNames(calcOutput("RiverNaturalFlows", aggregate=FALSE, selectyears=selectyears, climatetype=climatetype)[,,"lake_evap_nat"])

  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(I_yearly_runoff), years = getYears(I_yearly_runoff), names = c(paste("on",getNames(I_NAg_ww),sep="."), paste("off",getNames(I_NAg_ww),sep=".")), fill=0, sets=c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out     <- object0 + x
    return(out)
  }

  # bring all inputs to correct object size and transform to array for faster calculation
  lake_evap_new <- as.array(.transformObject(lake_evap_new))
  CAC_magpie    <- as.array(.transformObject(CAC_magpie))
  CAW_magpie    <- as.array(.transformObject(CAW_magpie))
  NAg_wc        <- as.array(.transformObject(I_NAg_wc))
  NAg_ww        <- as.array(.transformObject(I_NAg_ww))
  yearly_runoff <- as.array(.transformObject(I_yearly_runoff))

  # helper variables for river routing
  inflow        <- as.array(.transformObject(0))
  avl_wat_act   <- as.array(.transformObject(0))
  upstream_cons <- as.array(.transformObject(0))

  # output variable that will be filled during river routing
  discharge     <- as.array(.transformObject(0))


  ########################################################
  ### River Routing accounting for Human Uses and EFRs ###
  ########################################################
  ## Inputs from previous river routings
  if (humanuse=="non_agriculture") {

    # Minimum flow requirements determined by natural flow river routing: Environmental Flow Requirements (in mio. m^3 / yr) [long-term average]
    IO_required_wat_min         <- new.magpie(cells_and_regions = getCells(yearly_runoff), years = getYears(yearly_runoff), names = c("on", "off"), fill = 0)
    IO_required_wat_min[,,"on"] <- calcOutput("EnvmtlFlowRequirements", selectyears=selectyears, climatetype=climatetype, EFRmethod=EFRmethod, aggregate=FALSE)
    # Bring to correct object size
    IO_required_wat_min <- as.array(.transformObject(IO_required_wat_min))

    ## Current human uses
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currHuman_ww <- NAg_ww
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currHuman_wc <- NAg_wc

    # There are no previous human uses yet to be considered (empty arrays)
    prevHuman_wc <- as.array(.transformObject(0))

  } else if (humanuse=="committed_agriculture") {

    prevHuman_routing <- calcOutput("RiverHumanUses", climatetype=climatetype, EFRmethod=EFRmethod, selectyears=selectyears, iniyear=iniyear, humanuse="non_agriculture", aggregate=FALSE)

    # Minimum flow requirements determined by previous river routing: Environmental Flow Requirements + Reserved for Non-Agricultural Uses (in mio. m^3 / yr)
    IO_required_wat_min <- as.array(collapseNames(prevHuman_routing[,,"required_wat_min"]))
    ## Previous human uses (determined in non-agricultural uses river routing) (in mio. m^3 / yr):
    prevHuman_wc        <- as.array(collapseNames(prevHuman_routing[,,"currHuman_wc"]))

    ## Current human uses
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currHuman_ww <- CAW_magpie
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currHuman_wc <- CAC_magpie

  } else {
    stop("Please specify for which type of human uses river routing shall be calculated: non_agriculture or committed_agriculture")
  }

  ####################################################
  ###### River Routing considering Human Uses ########
  ####################################################

  for (o in 1:max(rs$calcorder)) {
    # Note: the calcorder ensures that the upstreamcells are calculated first
    cells <- which(rs$calcorder==o)

    for (c in cells) {

      # Available water in cell
      avl_wat_act[c,,]   <- inflow[c,,,drop=F] + yearly_runoff[c,,,drop=F] - lake_evap_new[c,,,drop=F]

      ### Is there sufficient water available to fulfill previously determined requirements? ###
      sufficient_water   <- (avl_wat_act[c,,,drop=F] >= IO_required_wat_min[c,,,drop=F])

      #### (1) Available Water in cell is sufficient to fulfill previously determined requirements     ####
      ####     (avl_wat_act[c,,] >= IO_required_wat_min[c,,])                                          ####
      ####      -> further withdrawals possible                                                        ####

      # current withdrawals requested?
      withdrawals        <- (currHuman_ww[c,,,drop=F]>0)

      # (I) Water withdrawal constraint: All withdrawals that can be fulfilled considering
      #                                  local previously determined water requirements are served
      frac_ww_constraint <- pmin( (avl_wat_act[c,,,drop=F] - IO_required_wat_min[c,,,drop=F])[sufficient_water[,,,drop=F] & withdrawals[,,,drop=F]] / currHuman_ww[c,,,drop=F][sufficient_water[,,,drop=F] & withdrawals[,,,drop=F]], 1)
      # Current water uses (withdrawals and consumption) given withdrawal constraint
      currHuman_wc[c,,][sufficient_water[,,,drop=F] & withdrawals[,,,drop=F]] <- frac_ww_constraint * (currHuman_wc[c,,,drop=F])[sufficient_water[,,,drop=F] & withdrawals[,,,drop=F]]
      currHuman_ww[c,,][sufficient_water[,,,drop=F] & withdrawals[,,,drop=F]] <- frac_ww_constraint * (currHuman_ww[c,,,drop=F])[sufficient_water[,,,drop=F] & withdrawals[,,,drop=F]]

      # Discharge in current cell for case where sufficient water available for requirements
      # (Subtract local water consumption in current cell (and previous if applicable)
      discharge[c,,][sufficient_water[,,,drop=F]] <- (avl_wat_act[c,,,drop=F] - currHuman_wc[c,,,drop=F] - prevHuman_wc[c,,,drop=F])[sufficient_water[,,,drop=F]]

      #### (2) Available Water in cell is not sufficient to fulfill previously determined requirements ####
      ####     (avl_wat_act[c,,] < IO_required_wat_min[c,,])                                           ####
      ####      -> no more water can be withdrawn locally (A + B)                                      ####
      ####      &  if possible: upstream consumption is reduced to release missing water    (A)        ####
      ####         if not: nothing can be consumed upstream (upstream consumption set to 0) (B)        ####

      # No water withdrawals locally if available water is not sufficient to fulfill
      # previously determined requirements
      currHuman_wc[c,,][!sufficient_water[,,,drop=F]] <- 0
      currHuman_ww[c,,][!sufficient_water[,,,drop=F]] <- 0

      # Update upstream cells' current consumption:
      if (length(rs$upstreamcells[[c]]) > 0) {

        # vector of upstreamcells of c
        up <- unlist(rs$upstreamcells[[c]])
        # vector of c in length of upstreamcells of c
        lc <- rep(c, length(rs$upstreamcells[[c]]))
        # vector of 1 in length of upstreamcells of c
        cc <- rep(1:length(c), length(rs$upstreamcells[[c]]))

        # Determine upstream current water consumption:
        upstream_cons[c,,]  <- colSums(currHuman_wc[up,,,drop=F], dims=1)

        ### (A or B) Is there current upstream water consumption that can be reduced  ###
        ###          to release water required by previous (prioritary) uses?         ###
        sufficient_upstream <- (upstream_cons[c,,,drop=F] > (IO_required_wat_min[c,,,drop=F] - avl_wat_act[c,,,drop=F]))

        ## (A) upstream_cons high enough to release required water: ##
        ## -> reduce upstream consumption respectively              ##
        # (II) Water consumption constraint: If water is required by priority
        #                                    use in downstream cell, cannot have
        #                                    been consumed for current use upstream
        # fraction that stays in upstream cell(s) = 1 - fraction of water that needs to be released by each upstream cell
        frac_wc_constraint <- (1 - (IO_required_wat_min[lc,,,drop=F] - avl_wat_act[lc,,,drop=F])[!sufficient_water[cc,,,drop=F] & sufficient_upstream[cc,,,drop=F]] / upstream_cons[lc,,,drop=F][!sufficient_water[cc,,,drop=F] & sufficient_upstream[cc,,,drop=F]])

        # Current human uses (consumption and withdrawal) in upstreamcells is reduced by respective amount
        currHuman_wc[up,,][!sufficient_water[cc,,,drop=F] & sufficient_upstream[cc,,,drop=F]] <- frac_wc_constraint * (currHuman_wc[up,,,drop=F])[!sufficient_water[cc,,,drop=F] & sufficient_upstream[cc,,,drop=F]]
        currHuman_ww[up,,][!sufficient_water[cc,,,drop=F] & sufficient_upstream[cc,,,drop=F]] <- frac_wc_constraint * (currHuman_ww[up,,,drop=F])[!sufficient_water[cc,,,drop=F] & sufficient_upstream[cc,,,drop=F]]

        # Discharge in current cell when water not sufficient to fulfill requirements,
        # but missing water water requirements can be fulfilled by upstream cells (A)
        discharge[c,,][!sufficient_water[,,,drop=F] & sufficient_upstream[,,,drop=F]]  <-  (IO_required_wat_min[c,,,drop=F] - prevHuman_wc[c,,,drop=F])[!sufficient_water[,,,drop=F] & sufficient_upstream[,,,drop=F]]

        ## (B) upstream_cons not high enough to release required water:
        ## -> no water can be used upstream
        # Current human uses (consumption and withdrawal) in upstreamcells are set to 0
        currHuman_wc[up,,][!sufficient_water[cc,,,drop=F] & !sufficient_upstream[cc,,,drop=F]] <- 0
        currHuman_ww[up,,][!sufficient_water[cc,,,drop=F] & !sufficient_upstream[cc,,,drop=F]] <- 0

        # Discharge in current cell when water not sufficient to fulfill requirements
        # and missing water water requirements cannot be fulfilled by upstream cells
        # (since there is not upstream consumption, this water is additionally available in the current cell)
        discharge[c,,][!sufficient_water[,,,drop=F] & !sufficient_upstream[,,,drop=F]] <- (avl_wat_act[c,,,drop=F] + upstream_cons[c,,,drop=F] - prevHuman_wc[c,,,drop=F])[!sufficient_water[,,,drop=F] & !sufficient_upstream[,,,drop=F]]
      } else {
        # Discharge when water is not sufficient to fulfill previously (priority)
        # requirements and there are no upstream cells
        discharge[c,,][!sufficient_water[,,,drop=F]]  <- (avl_wat_act[c,,,drop=F] - prevHuman_wc[c,,,drop=F])[!sufficient_water[,,,drop=F]]
      }

      # Inflow to nextcell (if there is a downstreamcell)
      if (rs$nextcell[c]>0) {
        inflow[rs$nextcell[c],,] <- inflow[rs$nextcell[c],,,drop=F] + discharge[c,,,drop=F]
      }
    }
  }

  # Update minimum water required in cell (for further river processing steps):
  IO_required_wat_min[,,] <- IO_required_wat_min[,,,drop=F] + currHuman_ww[,,,drop=F]

  ########################
  ### Output Variables ###
  ########################
  out <- new.magpie(cells_and_regions = getCells(IO_required_wat_min), years=getYears(IO_required_wat_min), names=c("required_wat_min", "currHuman_ww", "currHuman_wc"), sets=c("x.y.iso", "year", "data"))
  out <- .transformObject(out)
  out[,,"required_wat_min"] <- as.magpie(IO_required_wat_min, spatial=1, temporal=2)
  out[,,"currHuman_ww"]     <- as.magpie(currHuman_ww,        spatial=1, temporal=2)
  out[,,"currHuman_wc"]     <- as.magpie(currHuman_wc,        spatial=1, temporal=2)
  description <- paste0("river routing outputs taking human uses (",humanuse,") into account")

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
