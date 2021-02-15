#' @title calcRiverHumanUses
#' @description This function calculates natural discharge for the river routing derived from inputs from LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param subtype     Subtype to be returned: discharge or required_wat_min or frac_fulfilled
#' @param humanuse    Human use type to which river routing shall be applied (non_agriculture or committed_agriculture). Note: non_agriculture must be run prior to committed_agriculture
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param iniyear          Initialization year of irrigation system
#' @param irrigini         When "initialization" selected for irrigation system: choose initialization data set for irrigation system initialization ("Jaegermeyr_lpjcell", "LPJmL_lpjcell")
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

calcRiverHumanUses <- function(selectyears="all", humanuse="non_agriculture", subtype="discharge",
                               iniyear=1995, irrigini="Jaegermeyr_lpjcell",
                               version="LPJmL4", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015") {
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

  ### Internal functions
  ## Read in LPJmL data
  .getLPJmLData <- function(subtype, cfg) {
    x <- collapseNames(calcOutput("LPJmL", version="LPJmL4", selectyears=cfg$selectyears,
                                  climatetype=cfg$climatetype, harmonize_baseline=cfg$harmonize_baseline, ref_year=cfg$ref_year, time=cfg$time, dof=cfg$dof, averaging_range=cfg$averaging_range,
                                  subtype=subtype, aggregate=FALSE))
    return(x)
  }
  ## Non-agricultural water demand data
  .getNonAgData <- function(subtype, cfg) {
    x <- collapseNames(calcOutput("WaterUseNonAg", source="WATERGAP2020", waterusetype=subtype, seasonality="total", finalcells="lpjcell", aggregate=FALSE,
                                  selectyears=cfg$selectyears, climatetype=cfg$climatetype, harmonize_baseline=cfg$harmonize_baseline, ref_year=cfg$ref_year, time=cfg$time, dof=cfg$dof, averaging_range=cfg$averaging_range))
    # sort cells
    x <- x[rs$coordinates,,]
    # rename cells (NOTE: THIS IS ONLY TEMPORARILY NECESSARY UNTIL ALL INPUTS ARE PROVIDED AT COORDINATE DATA!!!!)
    x <- toolLPJcellCoordinates(x, type="coord2lpj")
    return(x)
  }
  ## Committed agricultural water demand data
  # .getCommAgData

  ### Required inputs for River Routing:
  ## LPJmL water data
  #!# NOTE: Only for development purposes.
  #!# In future: can drop smoothing and harmonization argument.
  #!# Water inputs should always be harmonized and smoothed before read in...
  cfg <- list(selectyears=selectyears, climatetype=climatetype,
              harmonize_baseline=harmonize_baseline, ref_year=ref_year,
              time=time, dof=dof, averaging_range=averaging_range)
  # Yearly runoff (mio. m^3 per yr) [smoothed & harmonized]
  I_yearly_runoff <- .getLPJmLData("runoff_lpjcell",     cfg)
  # Precipitation/Runoff on lakes and rivers from LPJmL (in mio. m^3 per year) [smoothed & harmonized]
  input_lake      <- .getLPJmLData("input_lake_lpjcell", cfg)

  # Calculate Runoff (on land and water)
  I_yearly_runoff <- I_yearly_runoff + input_lake

  ## Human uses
  # Non-Agricultural Water Withdrawals and Consumption (in mio. m^3 / yr) [smoothed]
  I_NAg_ww <- .getNonAgData("withdrawal",  cfg)
  I_NAg_wc <- .getNonAgData("consumption", cfg)

  # Harmonize non-agricultural consumption and withdrawals (withdrawals > consumption)
  I_NAg_ww <- pmax(I_NAg_ww, I_NAg_wc)
  I_NAg_wc <- pmax(I_NAg_wc, 0.01*I_NAg_ww)

  # Committed agricultural uses (in mio. m^3 / yr) [for initialization year]
  CAU_magpie <- calcOutput("WaterUseCommittedAg",selectyears=selectyears,cells="lpjcell",iniyear=iniyear,irrigini=paste0(unlist(str_split(irrigini, "_"))[[1]],"_lpjcell"),time=time,dof=dof,averaging_range=averaging_range,harmonize_baseline=harmonize_baseline,ref_year=ref_year,aggregate=FALSE)
  CAW_magpie <- collapseNames(dimSums(CAU_magpie[,,"withdrawal"],dim=3))
  CAC_magpie <- collapseNames(dimSums(CAU_magpie[,,"consumption"],dim=3))

  # Lake evaporation as calculated by natural flow river routing
  lake_evap_new <- collapseNames(calcOutput("RiverNaturalFlows", selectyears=selectyears, version="LPJmL4", aggregate=FALSE,
                                            climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof,
                                            harmonize_baseline=harmonize_baseline, ref_year=ref_year)[,,"lake_evap_nat"])

  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(I_yearly_runoff), years = getYears(I_yearly_runoff), names = c(paste("on",getNames(I_NAg_ww),sep="."), paste("off",getNames(I_NAg_ww),sep=".")), fill=0)
    # bring object x to dimension of object0
    out     <- x + object0
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

  # true-false objects as if conditions
  sufficient_water      <- as.array(.transformObject(NA))
  insufficient_water    <- as.array(.transformObject(NA))
  sufficient_upstream   <- as.array(.transformObject(NA))
  insufficient_upstream <- as.array(.transformObject(NA))

  # output variable that will be filled during river routing
  O_discharge    <- as.array(.transformObject(0))


  ########################################################
  ### River Routing accounting for Human Uses and EFRs ###
  ########################################################
  ## Inputs from previous river routings
  if (humanuse=="non_agriculture") {

    # Minimum flow requirements determined by natural flow river routing: Environmental Flow Requirements (in mio. m^3 / yr) [long-term average]
    IO_required_wat_min         <- new.magpie(cells_and_regions = getCells(yearly_runoff), years = getYears(yearly_runoff), names = c("on", "off"), fill = 0)
    IO_required_wat_min[,,"on"] <- calcOutput("EnvmtlFlowRequirements", selectyears=selectyears, version="LPJmL4", aggregate=FALSE, climatetype=climatetype,
                                              harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof, averaging_range=averaging_range,
                                              LFR_val=0.1, HFR_LFR_less10=0.2, HFR_LFR_10_20=0.15, HFR_LFR_20_30=0.07, HFR_LFR_more30=0.00,
                                              EFRyears=c(1980:2010))
    # Bring to correct object size
    IO_required_wat_min <- as.array(.transformObject(IO_required_wat_min))

    ## Current human uses
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currHuman_ww <- as.array(NAg_ww)
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currHuman_wc <- as.array(NAg_wc)

    # There are no previous human uses yet to be considered (empty arrays)
    prevHuman_wc <- as.array(.transformObject(0))

  } else if (humanuse=="committed_agriculture") {

    # Minimum flow requirements determined by previous river routing: Environmental Flow Requirements + Reserved for Non-Agricultural Uses (in mio. m^3 / yr)
    IO_required_wat_min <- as.array(calcOutput("RiverHumanUses", selectyears=selectyears, humanuse="non_agriculture", subtype="required_wat_min", aggregate=FALSE,
                                               version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year))
    ## Previous human uses (determined in non-agricultural uses river routing) (in mio. m^3 / yr):
    prevHuman_wc        <- as.array(calcOutput("RiverHumanUses", selectyears=selectyears, humanuse="non_agriculture", subtype="currHuman_wc", aggregate=FALSE,
                                               version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year))

    ## Current human uses
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currHuman_ww <- as.array(CAW_magpie)
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currHuman_wc <- as.array(CAC_magpie)

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

      # available water in cell
      avl_wat_act[c,,]  <- inflow[c,,,drop=F] + yearly_runoff[c,,,drop=F] - lake_evap_new[c,,,drop=F]

      ### Is there sufficient water available to fulfill previously determined requirements? ###
      sufficient_water[c,,]   <- (avl_wat_act[c,,,drop=F] >= IO_required_wat_min[c,,,drop=F])
      insufficient_water[c,,] <- !sufficient_water[c,,,drop=F]

      #### (1) Available Water in cell is not sufficient to fulfill previously determined requirements ####
      ####     (avl_wat_act[c,,] < IO_required_wat_min[c,,])                                           ####
      ####      -> no more water can be withdrawn locally (A + B)                                      ####
      ####      &  if possible upstream consumption is reduced to release missing water     (A)        ####
      ####         if not: nothing can be consumed upstream (upstream consumption set to 0) (B)        ####

      # If there are any upstream cells:
      if (length(rs$upstreamcells[[c]]) > 0) {

        # vector of upstreamcells of c
        up <- unlist(rs$upstreamcells[[c]])
        # vector of c in length of upstreamcells of c
        lc <- rep(c, length(rs$upstreamcells[[c]]))

        # Determine upstream current water consumption:
        upstream_cons[c,,] <- colSums(currHuman_wc[up,,,drop=F], dims=1)

        ### (A or B) Is there current upstream water consumption that can be reduced to release water ###
        ###          required by previous (prioritary) uses?                                          ###
        sufficient_upstream[c,,]   <- (upstream_cons[c,,,drop=F] > (IO_required_wat_min[c,,,drop=F] - avl_wat_act[c,,,drop=F]))
        insufficient_upstream[c,,] <- !sufficient_upstream[c,,,drop=F]

        ## (A) upstream_cons high enough to release required water:
        ## -> reduce upstream consumption respectively
        # Consumption constraint: fraction that stays in upstream cell(s) = 1 - fraction of water that needs to be released by each upstream cell
        frac_wc_constraint <- 1 - pmin(((IO_required_wat_min[lc,,,drop=F] - avl_wat_act[lc,,,drop=F]) / upstream_cons[lc,,,drop=F]), 1)
        # Current human uses (consumption and withdrawal) in upstreamcells is reduced by respective amount
        currHuman_wc[up,,][insufficient_water[c,,,drop=F] & sufficient_upstream[c,,,drop=F]] <- (currHuman_wc[up,,,drop=F] * frac_wc_constraint)[insufficient_water[c,,,drop=F] & sufficient_upstream[c,,,drop=F]]
        currHuman_ww[up,,][insufficient_water[c,,,drop=F] & sufficient_upstream[c,,,drop=F]] <- (currHuman_ww[up,,,drop=F] * frac_wc_constraint)[insufficient_water[c,,,drop=F] & sufficient_upstream[c,,,drop=F]]

        # Discharge in current cell when water not sufficient to fulfill requirements,
        # but missing water water requirements can be fulfilled by upstream cells (A)
        O_discharge[c,,][insufficient_water[c,,,drop=F] & sufficient_upstream[c,,,drop=F]]  <-  (IO_required_wat_min[c,,,drop=F] - prevHuman_wc[c,,,drop=F])[insufficient_water[c,,,drop=F] & sufficient_upstream[c,,,drop=F]]

        ## (B) upstream_cons not high enough to release required water:
        ## -> no water can be used upstream (current human uses in upstreamcells are set to 0)
        currHuman_wc[up,,][insufficient_water[c,,,drop=F] & insufficient_upstream[c,,,drop=F]] <- 0
        currHuman_ww[up,,][insufficient_water[c,,,drop=F] & insufficient_upstream[c,,,drop=F]] <- 0

        # Discharge in current cell when (1) Water not sufficient to fulfill requirements, (1.1) upstreamcells available,
        # (1.1.1) (B) missing water water requirements cannot be fulfilled by upstream cells
        # (since there is not upstream consumption, this water is additionally available in the current cell)
        O_discharge[c,,][insufficient_water[c,,,drop=F] & insufficient_upstream[c,,,drop=F]] <- (avl_wat_act[c,,,drop=F] + upstream_cons[c,,,drop=F] - prevHuman_wc[c,,,drop=F])[insufficient_water[c,,,drop=F] & insufficient_upstream[c,,,drop=F]]
      }

      #### (2) Available Water in cell is sufficient to fulfill previously determined requirements     ####
      ####     (avl_wat_act[c,,] >= IO_required_wat_min[c,,])                            ####
      ####      -> further withdrawals possible                                                        ####

      ## If there are current human water withdrawals (currHuman_ww[c,,] > 0)
      # Water withdrawal constraint: All withdrawals that can be fulfilled given available water are served
      frac_ww_constraint <- pmin( (avl_wat_act[c,,,drop=F] - IO_required_wat_min[c,,,drop=F]) / currHuman_ww[c,,,drop=F], 1)
      # correct fraction where NA (0/0)
      frac_ww_constraint[is.na(frac_ww_constraint)] <- 0
      currHuman_wc[c,,][sufficient_water[c,,,drop=F]] <- (currHuman_wc[c,,,drop=F] * frac_ww_constraint)[sufficient_water[c,,,drop=F]]
      currHuman_ww[c,,][sufficient_water[c,,,drop=F]] <- (currHuman_ww[c,,,drop=F] * frac_ww_constraint)[sufficient_water[c,,,drop=F]]

      # Discharge in current cell for case where sufficient water available for requirements (2)
      # (Subtract local water consumption in current cell (and previous if applicable)
      O_discharge[c,,][sufficient_water[c,,,drop=F]] <- (avl_wat_act[c,,,drop=F] - currHuman_wc[c,,,drop=F] - prevHuman_wc[c,,,drop=F])[sufficient_water[c,,,drop=F]]

      # Inflow to nextcell (if there is a downstreamcell)
      if (rs$nextcell[c]>0) {
        inflow[rs$nextcell[c],,] <- inflow[rs$nextcell[c],,,drop=F] + O_discharge[c,,,drop=F]
      }
    }
  }

  # Update minimum water required in cell (for further river processing steps):
  IO_required_wat_min[,,] <- IO_required_wat_min[,,,drop=F] + currHuman_ww[,,,drop=F]

  ########################
  ### Output Variables ###
  ########################
  if (subtype=="discharge") {
    out         <- as.magpie(O_discharge, spatial=1)
    description <- paste0("Cellular discharge after accounting for human uses: ", humanuse)
  } else if (subtype=="required_wat_min") {
    out         <- as.magpie(IO_required_wat_min, spatial=1)
    description <- paste0("Minimum requirements that need to stay in respective cell after human uses river routing: ", humanuse)
  } else if (subtype=="currHuman_wc") {
    out         <- as.magpie(currHuman_wc, spatial=1)
    description <- paste0("Human consumption (", humanuse, ") that can be fulfilled given local water availabilities and previous water requirements")
  } else if (subtype=="currHuman_ww") {
    out         <- as.magpie(currHuman_wc, spatial=1)
    description <- paste0("Human withdrawal (", humanuse, ") that can be fulfilled given local water availabilities and previous water requirements")
  } else {
    stop("Please specify subtype that should be returned by this function: discharge, required_wat_min or frac_fulfilled")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
