#' @title calcRiverNaturalFlows_magpie_magpie
#' @description This function calculates natural discharge for the river routing derived from inputs from LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop. Note: Default version arguments need to be updated when new versions are used!
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#'
#' @importFrom magclass collapseNames new.magpie getCells mbind setYears
#' @importFrom madrat calcOutput
#' @import mrcommons
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverNaturalFlows_magpie", aggregate = FALSE) }
#'

calcRiverNaturalFlows_magpie <- function(selectyears, lpjml=c(natveg="LPJmL4_for_MAgPIE_84a69edd", crop="ggcmi_phase3_nchecks_72c185fa"), climatetype) {
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

  ### Read in input data already time-smoothed and for climate scenarios harmonized to the baseline
  if (climatetype=="GSWP3-W5E5:historical") {
    # Baseline is only smoothed (not harmonized)
    stage <- "smoothed"
  } else {
    # Climate scenarios are harmonized to baseline
    stage <- "harmonized2020"
  }

  # Yearly lake evapotranspiration (in mio. m^3 per year) [smoothed & harmonized]
  lake_evap     <- collapseNames(calcOutput("LPJmL_new", version=lpjml["natveg"], subtype="lake_evap", climatetype=climatetype, stage=stage, years=selectyears, aggregate=FALSE))

  # Runoff (on land and water)
  yearly_runoff <- collapseNames(calcOutput("YearlyRunoff", aggregate=FALSE, selectyears=selectyears, climatetype=climatetype))


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
      lake_evap_new[c,,] <- pmin(lake_evap[c,,], inflow_nat[c,,]+yearly_runoff[c,,])
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
