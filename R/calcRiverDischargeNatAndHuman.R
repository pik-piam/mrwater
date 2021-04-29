#' @title       calcRiverDischargeNatAndHuman
#' @description This function calculates cellular discharge after considering known human consumption (non-agricultural and committed agricultural) along the river calculated and accounted for in previous river routings (see calcRiverNaturalFlows and calcRiverHumanUses)
#'
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear     Initialization year of irrigation system
#' @param EFRmethod   EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverDischargeNatAndHuman", aggregate = FALSE) }
#'

calcRiverDischargeNatAndHuman <- function(lpjml, selectyears, iniyear, climatetype, EFRmethod) {

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))

  # Non-agricultural human consumption that can be fulfilled by available water determined in previous river routings
  NAg_wc <- collapseNames(calcOutput("RiverHumanUses", humanuse="non_agriculture",       aggregate=FALSE, lpjml=lpjml, iniyear=iniyear, selectyears=selectyears, climatetype=climatetype, EFRmethod=EFRmethod)[,,"currHuman_wc"])
  # Committed agricultural human consumption that can be fulfilled by available water determined in previous river routings
  CAg_wc <- collapseNames(calcOutput("RiverHumanUses", humanuse="committed_agriculture", aggregate=FALSE, lpjml=lpjml, iniyear=iniyear, selectyears=selectyears, climatetype=climatetype, EFRmethod=EFRmethod)[,,"currHuman_wc"])

  # Yearly Runoff (on land and water)
  yearly_runoff <- collapseNames(calcOutput("YearlyRunoff",      aggregate=FALSE, lpjml=lpjml, selectyears=selectyears, climatetype=climatetype))
  # Lake evaporation as calculated by natural flow river routing
  lake_evap_new <- collapseNames(calcOutput("RiverNaturalFlows", aggregate=FALSE, lpjml=lpjml, selectyears=selectyears, climatetype=climatetype)[,,"lake_evap_nat"])

  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(yearly_runoff), years = getYears(yearly_runoff), names = getNames(NAg_wc), fill=0, sets=c("x.y.iso", "year", "data"))
    # bring object x to dimension of object0
    out     <- x + object0
    return(out)
  }

  #######################################
  ###### Transform object size   ########
  #######################################
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

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA discharge")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="Cellular discharge after accounting for environmental flow requirements and known human uses along the river",
    isocountries=FALSE))
}
