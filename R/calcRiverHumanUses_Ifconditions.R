#' @title calcRiverHumanUses_withif
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
#' @param irrigini         When "initialization" selected for irrigation system: choose initialization data set for irrigation system initialization ("Jaegermeyr_lpjcell", "LPJmL_lpjcell")
#' @param iniyear          Initialization year of irrigation system
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
#' \dontrun{ calcOutput("RiverHumanUses_withif", aggregate = FALSE) }
#'

calcRiverHumanUses_withif <- function(selectyears="all", humanuse="non_agriculture", subtype="discharge",
                                      irrigini="Jaegermeyr_lpjcell", iniyear=1995,
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
  input_lake    <- .getLPJmLData("input_lake_lpjcell", cfg)

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
  lake_evap_new <- collapseNames(calcOutput("RiverNaturalFlows", selectyears=selectyears, version=version, aggregate=FALSE,
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
  avl_wat_act <- as.array(.transformObject(0))

  # output variable that will be filled during river routing
  O_frac_currHuman_fulfilled <- as.array(.transformObject(0))
  O_discharge                <- as.array(.transformObject(0))

  ########################################################
  ### River Routing accounting for Human Uses and EFRs ###
  ########################################################
  ## Inputs from previous river routings
  if (humanuse=="non_agriculture") {

    # Minimum flow requirements determined by natural flow river routing: Environmental Flow Requirements (in mio. m^3 / yr) [long-term average]
    IO_required_wat_min         <- new.magpie(cells_and_regions = getCells(yearly_runoff), years = getYears(yearly_runoff), names = c("on", "off"), fill = 0)
    IO_required_wat_min[,,"on"] <- calcOutput("EnvmtlFlowRequirements", selectyears=selectyears, version="LPJmL4", climatetype=climatetype, aggregate=FALSE,
                                              LFR_val=0.1, HFR_LFR_less10=0.2, HFR_LFR_10_20=0.15, HFR_LFR_20_30=0.07, HFR_LFR_more30=0.00,
                                              EFRyears=c(1980:2010))
    # Bring to correct object size
    IO_required_wat_min <- .transformObject(IO_required_wat_min)

    ## Current human uses
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    currHuman_ww <- NAg_ww
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    currHuman_wc <- NAg_wc

    # There are no previous human uses yet to be considered (empty arrays)
    frac_prevHuman_fulfilled     <- .transformObject(0)
    prevHuman_wc                 <- .transformObject(0)
    prevHuman_ww                 <- .transformObject(0)

  } else if (humanuse=="committed_agriculture") {

    # Minimum flow requirements determined by previous river routing: Environmental Flow Requirements + Reserved for Non-Agricultural Uses (in mio. m^3 / yr)
    IO_required_wat_min <- calcOutput("RiverHumanUses", selectyears=selectyears, humanuse="non_agriculture", subtype="required_wat_min",
                                      version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
    ## Previous human uses (determined in non-agricultural uses river routing):
    frac_prevHuman_fulfilled <- calcOutput("RiverHumanUses", selectyears=selectyears, humanuse="non_agriculture", subtype="frac_fulfilled",
                                           version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
    ## Previous human uses (inputs)
    # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
    prevHuman_ww <- NAg_ww
    # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
    prevHuman_wc <- NAg_wc

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
      # available water in cell
      avl_wat_act[c,,]  <- inflow[c,,,drop=F] + yearly_runoff[c,,,drop=F] - lake_evap_new[c,,,drop=F]

      # available water in cell not sufficient to fulfill requirements
      # -> no more water can be withdrawn
      if ( avl_wat_act[c,,,drop=F] < IO_required_wat_min[c,,,drop=F] ) {

        # if cell has upstreamcells: upstreamcells must release missing water (cannot be consumed upstream)
        # -> reduce committed agricultural water consumption in upstream cells
        # -> locally: cannot withdraw
        if ( length(rs$upstreamcells[c]) > 0 ) {
          # upstream committed agricultural water consumption:
          upstream_cons <- sum(currHuman_wc[rs$upstreamcells[[c]],,,drop=F] * O_frac_currHuman_fulfilled[rs$upstreamcells[[c]],,,drop=F])

          # if upstream_cons high enough to account for difference: reduce upstream consumption respectively
          if ( upstream_cons[,,,drop=F] > (IO_required_wat_min[c,,,drop=F] - avl_wat_act[c,,,drop=F]) ) {
            O_frac_currHuman_fulfilled[rs$upstreamcells[[c]],,] <- ( 1 - (IO_required_wat_min[c,,,drop=F] - avl_wat_act[c,,,drop=F]) / upstream_cons[,,,drop=F] ) * O_frac_currHuman_fulfilled[rs$upstreamcells[[c]],,,drop=F]
            O_discharge[c,,]                                    <- IO_required_wat_min[c,,,drop=F] - prevHuman_wc[c,,,drop=F]*frac_prevHuman_fulfilled[c,,,drop=F]

          # if upstream_cons not sufficient to account for difference: no water can be used upstream
          } else {
            O_frac_currHuman_fulfilled[rs$upstreamcells[[c]],,] <- 0
            O_discharge[c,,]                                    <- avl_wat_act[c,,,drop=F] + upstream_cons[,,,drop=F] - prevHuman_wc[c,,,drop=F] * frac_prevHuman_fulfilled[c,,,drop=F]
          }
        }

        # available water in cell is sufficient to fulfill requirements
        # -> further withdrawal possible
      } else {

        # Current human water withdrawals
        if ( currHuman_ww[c,,,drop=F] > 0 ) {
          ## Water withdrawal constraint:
          O_frac_currHuman_fulfilled[c,,] <- min( (avl_wat_act[c,,,drop=F] - IO_required_wat_min[c,,,drop=F]) / currHuman_ww[c,,,drop=F], 1)
        }

        ## Outflow from one cell to the next
        # (Subtract local water consumption in current cell (and previous if applicable)
        O_discharge[c,,] <- avl_wat_act[c,,,drop=F] - currHuman_wc[c,,,drop=F] * O_frac_currHuman_fulfilled[c,,,drop=F] - prevHuman_wc[c,,,drop=F] * frac_prevHuman_fulfilled[c,,,drop=F]
      }

      # Inflow to nextcell
      if (rs$nextcell[c]>0) {
        inflow[rs$nextcell[c],,] <- inflow[rs$nextcell[c],,,drop=F] + O_discharge[c,,,drop=F]
      }
    }
  }

  # Update minimum water required in cell (for further river processing steps):
  IO_required_wat_min[,,] <- IO_required_wat_min[,,,drop=F] + currHuman_ww[,,,drop=F] * O_frac_currHuman_fulfilled[,,,drop=F]

  ########################
  ### Output Variables ###
  ########################
  if (subtype=="discharge") {
    out         <- as.magpie(O_discharge, spatial=1)
    description <- paste0("Cellular discharge after accounting for human uses: ", humanuse)
  } else if (subtype=="required_wat_min") {
    out         <- as.magpie(IO_required_wat_min, spatial=1)
    description <- paste0("Minimum requirements that need to stay in respective cell after human uses river routing: ", humanuse)
  } else if (subtype=="frac_fulfilled") {
    out         <- as.magpie(O_frac_currHuman_fulfilled, spatial=1)
    description <- paste0("Fraction of human uses (", humanuse, ") that can be fulfilled given local water availabilities and previous water requirements")
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
