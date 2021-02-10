#' @title calcRiverNaturalFlows_old
#' @description This function calculates natural discharge for the river routing derived from inputs from LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#'
#' @importFrom magclass collapseNames new.magpie getCells mbind setYears
#' @importFrom madrat calcOutput
#' @import mrcommons
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverNaturalFlows_old", aggregate = FALSE) }
#'

calcRiverNaturalFlows_old <- function(selectyears="all",
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
  rs <- readRDS(system.file("extdata/riverstructure_stn.rds", package="mrwater"))

  ### Required inputs for Natural Flows River Routing:
  ## LPJmL water data
  .getLPJmLData <- function(subtype, cfg) {
    x <- calcOutput("LPJmL", version="LPJmL4", selectyears=cfg$selectyears,
                    climatetype=cfg$climatetype, harmonize_baseline=cfg$harmonize_baseline, ref_year=cfg$ref_year, time=cfg$time, dof=cfg$dof, averaging_range=cfg$averaging_range,
                    subtype=subtype, aggregate=FALSE)
    x <- as.array(collapseNames(x))[,,1]
    return(x)
  }
  #!# NOTE: Only for development purposes.
  #!# In future: can drop smoothing and harmonization argument.
  #!# Water inputs should always be harmonized and smoothed before read in...
  cfg <- list(selectyears=selectyears, climatetype=climatetype,
              harmonize_baseline=harmonize_baseline, ref_year=ref_year,
              time=time, dof=dof, averaging_range=averaging_range)
  # Yearly runoff (mio. m^3 per yr) [smoothed & harmonized]
  yearly_runoff <- .getLPJmLData("runoff_lpjcell",     cfg)
  # Yearly lake evapotranspiration (in mio. m^3 per year) [smoothed & harmonized]
  lake_evap     <- .getLPJmLData("evap_lake_lpjcell",  cfg)
  # Precipitation/Runoff on lakes and rivers from LPJmL (in mio. m^3 per year) [smoothed & harmonized]
  input_lake    <- .getLPJmLData("input_lake_lpjcell", cfg)

  # # # # # # # # # # #
  # # # CALCULATIONS # #
  # # # # # # # # # # #
  ### Runoff (on land and water)
  yearly_runoff <- yearly_runoff + input_lake

  ############################################
  ###### River Routing: Natural Flows ########
  ############################################
  if (class(selectyears)=="numeric") {
    selectyears <- paste0("y",selectyears)
  }

  tmp <- NULL
  out <- NULL

  for (y in selectyears){

    #############################
    ####### River routing #######
    #############################

    ## Global river routing variables
    # Naturalized discharge
    discharge_nat <- array(data=0,dim=67420)
    inflow_nat    <- array(data=0,dim=67420)
    lake_evap_new <- array(data=0,dim=67420)

    ### River Routing 1.1: Natural flows ###
    # Determine natural discharge
    for (o in 1:max(rs$calcorder)){
      # Note: the calcorder ensures that upstreamcells are calculated first
      cells <- which(rs$calcorder==o)

      for (c in cells){
        ### Natural water balance
        # lake evap that can be fulfilled (if water available: lake evaporation considered; if not: lake evap is reduced respectively):
        lake_evap_new[c] <- min(lake_evap[c,y], inflow_nat[c]+yearly_runoff[c,y])
        # natural discharge
        discharge_nat[c] <- inflow_nat[c] + yearly_runoff[c,y] - lake_evap_new[c]
        # inflow into nextcell
        if (rs$nextcell[c]>0){
          inflow_nat[rs$nextcell[c]] <- inflow_nat[rs$nextcell[c]] + discharge_nat[c]
        }
      }
    }

    tmp <- new.magpie(cells_and_regions = getCells(yearly_runoff), years=y, names=c("discharge_nat", "lake_evap_nat"))

    discharge_nat <- setCells(setNames(setYears(as.magpie(discharge_nat,spatial=1),y), nm="discharge_nat"), getCells(yearly_runoff))
    lake_evap_nat <- setCells(setNames(setYears(as.magpie(lake_evap_new,spatial=1),y), nm="lake_evap_nat"), getCells(yearly_runoff))

    tmp[,,"discharge_nat"] <- discharge_nat[,,]
    tmp[,,"lake_evap_nat"] <- lake_evap_nat[,,]

    out <- mbind(out, tmp)
    tmp <- NULL
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="Cellular natural discharge and lake evaporation in natural river condition",
    isocountries=FALSE))
}
