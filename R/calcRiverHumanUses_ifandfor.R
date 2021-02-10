#' @title calcRiverHumanUses_ifandfor
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
#' \dontrun{ calcOutput("RiverHumanUses_ifandfor", aggregate = FALSE) }
#'

calcRiverHumanUses_ifandfor <- function(selectyears="all", humanuse="non_agriculture", subtype="discharge",
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
    x <- mrwater:::toolLPJcellCoordinates(x, type="coord2lpj")
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

  # Lake evaporation as calculated by natural flow river routing
  lake_evap_new <- collapseNames(calcOutput("RiverNaturalFlows", selectyears=selectyears, version=version, aggregate=FALSE,
                                                      climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof,
                                                      harmonize_baseline=harmonize_baseline, ref_year=ref_year)[,,"lake_evap_nat"])

  # bring all inputs to correct object size and transform to array for faster calculation
  lake_evap_new <- as.array(lake_evap_new)[,,1]
  NAg_wc        <- as.array(I_NAg_wc)
  NAg_ww        <- as.array(I_NAg_ww)
  yearly_runoff <- as.array(I_yearly_runoff)[,,1]


  ####################################################
  ###### River Routing considering Human Uses ########
  ####################################################

  for (EFP in c("on", "off")) {

    required_wat_min <- calcOutput("EnvmtlFlowRequirements", selectyears=selectyears, version="LPJmL4", climatetype=climatetype, aggregate=FALSE,
                                      LFR_val=0.1, HFR_LFR_less10=0.2, HFR_LFR_10_20=0.15, HFR_LFR_20_30=0.07, HFR_LFR_more30=0.00,
                                      EFRyears=c(1980:2010))
    required_wat_min <- as.array(required_wat_min)[,,1]

    if (EFP=="off"){
      required_wat_min[,] <- 0
    }

    for (scen in getNames(NAg_ww)){
      for (y in selectyears){

        # Discharge considering human uses
        discharge   <- array(data=0,dim=67420)
        inflow      <- array(data=0,dim=67420)
        avl_wat_act <- array(data=0,dim=67420)
        # Water fractions reserved for certain uses
        frac_NAg_fulfilled <- array(data=0,dim=67420)

        ### River Routing 2: Non-agricultural uses considering local EFRs ###
        for (o in 1:max(rs$calcorder)) {
          # Note: the calcorder ensures that the upstreamcells are calculated first
          cells <- which(rs$calcorder==o)

          for (c in cells){
            # available water in cell
            avl_wat_act[c]  <- inflow[c]+yearly_runoff[c,y]-lake_evap_new[c,y]

            # available water in cell not sufficient to fulfill requirements
            # -> no more water can be withdrawn
            if (avl_wat_act[c]<required_wat_min[c,y]){
              # if cell has upstreamcells: upstreamcells must release missing water (cannot be consumed upstream)
              # -> reduce non-agricultural water consumption in upstream cells
              # -> locally: cannot withdraw
              if (length(rs$upstreamcells[[c]])>0){
                # upstream non-agricultural water consumption
                upstream_cons <- sum(NAg_wc[rs$upstreamcells[[c]],y,scen]*frac_NAg_fulfilled[rs$upstreamcells[[c]]])
                if (upstream_cons>required_wat_min[c,y]-avl_wat_act[c]){
                  # if missing water (difference) can be fulfilled by upstream consumption: reduce upstream consumption
                  frac_NAg_fulfilled[rs$upstreamcells[[c]]] <- (1-(required_wat_min[c,y]-avl_wat_act[c])/upstream_cons)*frac_NAg_fulfilled[rs$upstreamcells[[c]]]
                  discharge[c] <- required_wat_min[c,y]
                } else {
                  # if missing water (difference) cannot be fulfilled by upstream consumption: no upstream consumption
                  frac_NAg_fulfilled[rs$upstreamcells[[c]]] <- 0
                  discharge[c] <- avl_wat_act[c]+upstream_cons
                }
              }

              # available water in cell is sufficient to fulfill requirements
              # -> further withdrawals are possible
            } else {
              # Non-agricultural withdrawals
              if (NAg_ww[c,y,scen]>0){
                ## Water withdrawal constraint:
                frac_NAg_fulfilled[c] <- min((avl_wat_act[c]-required_wat_min[c,y])/NAg_ww[c,y,scen], 1)
              }

              ## Outflow from one cell to the next
              # (Subtract local water consumption in current cell (non-ag. consumption))
              discharge[c] <- avl_wat_act[c] - NAg_wc[c,y,scen]*frac_NAg_fulfilled[c]
            }

            if (rs$nextcell[c]>0){
              inflow[rs$nextcell[c]] <- inflow[rs$nextcell[c]] + discharge[c]
            }
          }
        }

        # Update minimum water required in cell:
        required_wat_min <- required_wat_min[,y] + NAg_ww[,y,scen]*frac_NAg_fulfilled
      }
    }
  }

  ########################
  ### Output Variables ###
  ########################
  if (subtype=="discharge") {
    out         <- as.magpie(discharge, spatial=1)
    description <- paste0("Cellular discharge after accounting for human uses: ", humanuse)
  } else if (subtype=="required_wat_min") {
    out         <- as.magpie(required_wat_min, spatial=1)
    description <- paste0("Minimum requirements that need to stay in respective cell after human uses river routing: ", humanuse)
  } else if (subtype=="frac_fulfilled") {
    out         <- as.magpie(frac_NAg_fulfilled, spatial=1)
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
