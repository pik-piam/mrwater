#' @title calcRiverNaturalFlows_old
#' @description This function calculates natural discharge for the river routing derived from inputs from LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop
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
#' \dontrun{ calcOutput("RiverNaturalFlows_old", aggregate = FALSE) }
#'

calcRiverNaturalFlows_old <- function(selectyears, lpjml, climatetype) {

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
  if (grepl("historical", climatetype)) {
    # Baseline is only smoothed (not harmonized)
    stage <- "smoothed"
  } else {
    # Climate scenarios are harmonized to baseline
    stage <- "harmonized2020"
  }

  # Yearly lake evapotranspiration (in mio. m^3 per year) [smoothed & harmonized]
  lake_evap     <- as.array(calcOutput("LPJmL_new", version=lpjml["natveg"], subtype="lake_evap", climatetype=climatetype, stage=stage, years=selectyears, aggregate=FALSE))

  # Runoff (on land and water)
  yearly_runoff <- as.array(collapseNames(calcOutput("YearlyRunoff", aggregate=FALSE, lpjml=lpjml, selectyears=selectyears, climatetype=climatetype)))

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
