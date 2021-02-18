#' @title       calcRiverDischargeNatAndHuman
#' @description This function calculates cellular discharge after considering known human consumption (non-agricultural and committed agricultural) along the river calculated and accounted for in previous river routings (see calcRiverNaturalFlows and calcRiverHumanUses)
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
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#' @import mrmagpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverDischargeNatAndHuman", aggregate = FALSE) }
#'

calcRiverDischargeNatAndHuman <- function(selectyears="all", version="LPJmL4", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015") {

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))

  # Non-agricultural human consumption that can be fulfilled by available water determined in previous river routings
  NAg_wc <- collapseNames(calcOutput("RiverHumanUses", humanuse="non_agriculture",       aggregate=FALSE, selectyears=selectyears, version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)[,,"currHuman_wc"])
  # Committed agricultural human consumption that can be fulfilled by available water determined in previous river routings
  CAg_wc <- collapseNames(calcOutput("RiverHumanUses", humanuse="committed_agriculture", aggregate=FALSE, selectyears=selectyears, version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)[,,"currHuman_wc"])

  # Yearly Runoff (on land and water)
  yearly_runoff <- collapseNames(calcOutput("YearlyRunoff",      aggregate=FALSE, version=version, selectyears=selectyears, climatetype=climatetype, time=time, dof=dof, averaging_range=averaging_range, harmonize_baseline=harmonize_baseline, ref_year=ref_year))
  # Lake evaporation as calculated by natural flow river routing
  lake_evap_new <- collapseNames(calcOutput("RiverNaturalFlows", aggregate=FALSE, version=version, selectyears=selectyears, climatetype=climatetype, time=time, dof=dof, averaging_range=averaging_range, harmonize_baseline=harmonize_baseline, ref_year=ref_year)[,,"lake_evap_nat"])

  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(yearly_runoff), years = getYears(yearly_runoff), names = c(paste("on",getNames(NAg_wc),sep="."), paste("off",getNames(NAg_wc),sep=".")), fill=0)
    # bring object x to dimension of object0
    out     <- x + object0
    return(out)
  }

  # bring all inputs to correct object size and transform to array for faster calculation
  lake_evap_new <- as.array(.transformObject(lake_evap_new))
  CAg_wc        <- as.array(.transformObject(CAg_wc))
  NAg_wc        <- as.array(.transformObject(NAg_wc))
  yearly_runoff <- as.array(.transformObject(yearly_runoff))

  # helper variables for river routing
  inflow        <- as.array(.transformObject(0))
  avl_wat_act   <- as.array(.transformObject(0))

  # output variable that will be filled during river routing
  O_discharge   <- as.array(.transformObject(0))

  ###########################################
  ###### River Discharge Calculation ########
  ###########################################

  for (o in 1:max(rs$calcorder)) {
    # Note: the calcorder ensures that the upstreamcells are calculated first
    cells <- which(rs$calcorder==o)

    for (c in cells) {
      # available water
      avl_wat_act[c,,] <- inflow[c,,,drop=F] + yearly_runoff[c,,,drop=F] - lake_evap_new[c,,,drop=F]
      # discharge
      O_discharge[c,,] <- avl_wat_act[c,,,drop=F] - NAg_wc[c,,,drop=F] - CAg_wc[c,,,drop=F]
      # inflow into nextcell
      if (rs$nextcell[c]>0) {
        inflow[rs$nextcell[c],,] <- inflow[rs$nextcell[c],,,drop=F] + O_discharge[c,,,drop=F]
      }
    }
  }

  out <- as.magpie(O_discharge, spatial=1)

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="Cellular discharge after accounting for environmental flow requirements and known human uses along the river",
    isocountries=FALSE))
}
