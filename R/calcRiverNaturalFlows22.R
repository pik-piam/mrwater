#' @title calcRiverNaturalFlows_magpie_magpie22
#' @description This function calculates natural discharge and associated lake evaporation for the river routing derived from inputs from LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop. Note: Default version arguments need to be updated when new versions are used!
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#'
#' @importFrom magclass collapseNames new.magpie getCells mbind setYears setNames
#' @importFrom madrat calcOutput
#' @import mrcommons
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverNaturalFlows_magpie22", aggregate = FALSE) }
#'

calcRiverNaturalFlows22 <- function(selectyears, lpjml, climatetype) {

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
  lake_evap     <- as.array(setYears(calcOutput("LPJmL_new", version=lpjml["natveg"], subtype="lake_evap", climatetype=climatetype, stage=stage, years=selectyears, aggregate=FALSE), selectyears))

  # Runoff (on land and water)
  yearly_runoff <- as.array(collapseNames(calcOutput("YearlyRunoff", aggregate=FALSE, lpjml=lpjml, selectyears=selectyears, climatetype=climatetype)))

  ############################################
  ###### River Routing: Natural Flows ########
  ############################################

  ## Empty magpie objects that will be filled by the following natural river routing algorithm
  discharge_nat     <- yearly_runoff
  discharge_nat[,,] <- 0
  inflow_nat        <- discharge_nat
  lake_evap_new     <- discharge_nat

  ### River Routing 1: Natural flows ###
  calculated <- NULL

  for (o in 1:max(rs$calcorder)){
    # Note: the calcorder ensures that upstreamcells are calculated first
    c <- which(rs$calcorder==o)

    if(!is.null(calculated) && !all(unlist(rs$upstreamcells[c]) %in% calculated)) stop("Inconsistent calculation order!")
    if(!is.null(calculated) && c %in% calculated) stop("Cell was already calculated!")

    ### Natural water balance
    # lake evap that can be fulfilled (if water available: lake evaporation considered; if not: lake evap is reduced respectively):
    lake_evap_new[c,,] <- pmin(lake_evap[c,,,drop=F], inflow_nat[c,,,drop=F] + yearly_runoff[c,,,drop=F])
    # natural discharge
    discharge_nat[c,,] <- inflow_nat[c,,,drop=F] + yearly_runoff[c,,,drop=F] - lake_evap_new[c,,,drop=F]

    # inflow into nextcell
    #toolInflowIntoNextcell(cell= c , previous_inflow = inflow_nat, cell_discharge = discharge_nat)
    if (any(rs$nextcell[c] > 0)) {
      current  <- c[rs$nextcell[c] > 0]
      nextcell <- rs$nextcell[current]

      # inflow needs to be calculated for all nextcells separately to account for
      # fact that there can be several cells that discharge into same nextcell
      while (length(current) > 0) {
        dn            <- duplicated(nextcell)
        rest_c        <- current[dn]
        current       <- current[!dn]
        rest_nextcell <- nextcell[dn]
        nextcell      <- nextcell[!dn]

        inflow_nat[nextcell,,] <- inflow_nat[nextcell,,] + discharge_nat[current,,]

        nextcell <- rest_nextcell
        current  <- rest_c
      }
    }

    calculated <- c(calculated, c)
  }

  out <- new.magpie(cells_and_regions = getCells(discharge_nat), years=getYears(discharge_nat), names=c("discharge_nat", "lake_evap_nat"))
  out[,,"discharge_nat"] <- as.magpie(discharge_nat, spatial=1, temporal=2)
  out[,,"lake_evap_nat"] <- as.magpie(lake_evap_new, spatial=1, temporal=2)

return(list(
  x=out,
  weight=NULL,
  unit="mio. m^3",
  description="Cellular natural discharge and lake evaporation in natural river condition",
  isocountries=FALSE))
}
