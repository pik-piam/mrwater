#' @title calcRiverNaturalFlows
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
#' \dontrun{ calcOutput("calcRiverNaturalFlows", aggregate = FALSE) }
#'

calcRiverNaturalFlows <- function(selectyears="all",
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
    # read in LPJmL data
    x <- calcOutput("LPJmL", version="LPJmL4", selectyears=cfg$selectyears,
                    climatetype=cfg$climatetype, harmonize_baseline=cfg$harmonize_baseline, ref_year=cfg$ref_year, time=cfg$time, dof=cfg$dof, averaging_range=cfg$averaging_range,
                    subtype=subtype, aggregate=FALSE)
    # transform to array for faster calculation
    x <- as.array(collapseNames(x))
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

  ## Empty magpie objects that will be filled by the following natural river routing algorithm
  discharge_nat     <- yearly_runoff
  discharge_nat[,,] <- 0
  inflow_nat        <- discharge_nat
  lake_evap_new     <- discharge_nat

  ### River Routing 1: Natural flows ###
  # Determine natural discharge
  for (o in 1:max(rs$calcorder)){
    # Note: the calcorder ensures that upstreamcells are calculated first
    cells <- which(rs$calcorder==o)

    for (c in cells){
      ### Natural water balance
      # lake evap that can be fulfilled (if water available: lake evaporation considered; if not: lake evap is reduced respectively):
      lake_evap_new[c,,] <- min(lake_evap[c,,], inflow_nat[c,,] + yearly_runoff[c,,])
      # natural discharge
      discharge_nat[c,,] <- inflow_nat[c,,] + yearly_runoff[c,,] - lake_evap_new[c,,]
      # inflow into nextcell
      if (rs$nextcell[c]>0){
        inflow_nat[rs$nextcell[c],,] <- inflow_nat[rs$nextcell[c],,] + discharge_nat[c,,]
      }
    }
  }

  out <- new.magpie(cells_and_regions = getCells(discharge_nat), years=getYears(discharge_nat), names=c("discharge_nat", "lake_evap_nat"))
  out[,,"discharge_nat"] <- discharge_nat[,,]
  out[,,"lake_evap_nat"] <- lake_evap_new[,,]

return(list(
  x=out,
  weight=NULL,
  unit="mio. m^3",
  description="Cellular natural discharge and lake evaporation in natural river condition",
  isocountries=FALSE))
}