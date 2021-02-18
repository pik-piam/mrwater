#' @title calcRiverNaturalFlows_magpie_magpie
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
#' @importFrom magclass collapseNames new.magpie getCells mbind setYears setNames
#' @importFrom madrat calcOutput
#' @import mrcommons
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverNaturalFlows_magpie", aggregate = FALSE) }
#'

calcRiverNaturalFlows2 <- function(selectyears="all",
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
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))

  # Yearly lake evapotranspiration (in mio. m^3 per year) [smoothed & harmonized]
  lake_evap     <- as.array(collapseNames(calcOutput("LPJmL",        aggregate=FALSE, version=version, selectyears=selectyears, climatetype=climatetype, time=time, dof=dof, averaging_range=averaging_range, harmonize_baseline=harmonize_baseline, ref_year=ref_year)))

  # Runoff (on land and water)
  yearly_runoff <- as.array(collapseNames(calcOutput("YearlyRunoff", aggregate=FALSE, version=version, selectyears=selectyears, climatetype=climatetype, time=time, dof=dof, averaging_range=averaging_range, harmonize_baseline=harmonize_baseline, ref_year=ref_year)))

  ############################################
  ###### River Routing: Natural Flows ########
  ############################################

  ## Empty magpie objects that will be filled by the following natural river routing algorithm
  discharge_nat     <- yearly_runoff
  discharge_nat[,,] <- 0
  lake_evap_new <- inflow_nat <- discharge_nat

  ### River Routing 1: Natural flows ###
  # Determine natural discharge
  calculated <- NULL
  for (o in 1:max(rs$calcorder)){
    # Note: the calcorder ensures that upstreamcells are calculated first
    c <- which(rs$calcorder==o)
    if(!is.null(calculated) && !all(unlist(rs$upstreamcells[c]) %in% calculated)) stop("Inconsistent calculation order!")
    ### Natural water balance
    # lake evap that can be fulfilled (if water available: lake evaporation considered; if not: lake evap is reduced respectively):
    lake_evap_new[c,,] <- pmin(lake_evap[c,,], inflow_nat[c,,] + yearly_runoff[c,,])
    # natural discharge
    discharge_nat[c,,] <- inflow_nat[c,,] + yearly_runoff[c,,] - lake_evap_new[c,,]
    # inflow into nextcell
    nextcell <- rs$nextcell[c]
    c <- c[nextcell > 0]
    nextcell <- nextcell[nextcell > 0]
    while (length(c) > 0) {
      dn <- duplicated(nextcell)
      rest_c <- c[dn]
           c <- c[!dn]
      rest_nextcell <- nextcell[dn]
           nextcell <- nextcell[!dn]

      inflow_nat[nextcell,,] <- inflow_nat[nextcell,,] + discharge_nat[c,,]

      nextcell <- rest_nextcell
      c        <- rest_c
    }

    calculated <- c(calculated, c)
  }

  out <- mbind(setNames(as.magpie(discharge_nat, spatial=1, temporal=2), "discharge_nat"),
               setNames(as.magpie(lake_evap_new, spatial=1, temporal=2),"lake_evap_nat"))

return(list(
  x=out,
  weight=NULL,
  unit="mio. m^3",
  description="Cellular natural discharge and lake evaporation in natural river condition",
  isocountries=FALSE))
}
