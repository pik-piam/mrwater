#' @title       calcRiverSurplusDischargeAllocation_final
#' @description This function distributes surplus basin discharge after the previous river routings following certain management assumptions
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param output output to be reported
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param conservationstatus Conservation status or management objective according to Smakthin EFR method: "fair", "good", "natural". Details: The strictness of the conservation status affects the LFRs (low flow requirements, baseflow that needs to be maintained in the river)
#' @param variabilitythreshold Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod      method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype   Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear          Initialization year of irrigation system
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param proxycrop        historical crop mix pattern ("historical") or list of proxycrop(s)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames as.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverSurplusDischargeAllocation_final", aggregate = FALSE) }
#'

calcRiverSurplusDischargeAllocation_final <- function(selectyears, output, climatetype, conservationstatus, variabilitythreshold, rankmethod, allocationrule, thresholdtype, gainthreshold, irrigationsystem, iniyear, avlland_scen, proxycrop) {

  # Check
  if (!is.na(as.list(strsplit(avlland_scen, split=":"))[[1]][2]) && iniyear != as.numeric(as.list(strsplit(avlland_scen, split=":"))[[1]][2])) stop("Initialization year in calcRiverSurplusDischargeAllocation does not match: iniyear and avlland_scen should have same initialization year")

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River Structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))
  # numeric cell numbers in order of rs object
  #rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

  # Minimum flow requirements determined by previous river routing: Environmental Flow Requirements + Reserved for Non-Agricultural Uses + Reserved Committed Agricultural Uses (in mio. m^3 / yr)
  required_wat_min_allocation <- collapseNames(calcOutput("RiverHumanUses", humanuse="committed_agriculture", climatetype=climatetype, conservationstatus=conservationstatus, selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)[,,"required_wat_min"])

  # Discharge determined by previous river routings (in mio. m^3 / yr)
  discharge                   <- calcOutput("RiverDischargeNatAndHuman", selectyears=selectyears, iniyear=iniyear, climatetype=climatetype, conservationstatus=conservationstatus, aggregate=FALSE)

  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig      <- calcOutput("FullIrrigationRequirement", selectyears=selectyears, climatetype=climatetype, comagyear=iniyear, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, aggregate=FALSE)
  required_wat_fullirrig_ww   <- pmax(collapseNames(required_wat_fullirrig[,,"withdrawal"]), 0)
  required_wat_fullirrig_wc   <- pmax(collapseNames(required_wat_fullirrig[,,"consumption"]), 0)

  # Yield gain potential through irrigation of proxy crops
  irrig_yieldgainpotential    <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears, proxycrop=proxycrop, monetary=thresholdtype, iniyear=iniyear, aggregate=FALSE)

  # Initialization of fraction of full irrigation requirements that can be fulfilled
  frac_fullirrig              <- new.magpie(cells_and_regions = getCells(discharge), years = getYears(discharge), names = getNames(discharge), fill=0, sets=c("x.y.iso", "year", "EFP.scen"))

  # Discharge Accessibility Share
  access_shr <- calcOutput("DischargeAccessibilityShare", selectyears=selectyears, climatetype=climatetype, variabilitythreshold=variabilitythreshold, aggregate=FALSE)

  # Initialization of objects
  tmp <- as.magpie(discharge)

  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(tmp), years = getYears(tmp), names = getNames(tmp), fill=0, sets=c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out     <- object0 + x
    return(out)
  }

  discharge                   <- as.array(discharge)
  required_wat_min_allocation <- as.array(required_wat_min_allocation)
  frac_fullirrig              <- as.array(.transformObject(frac_fullirrig))
  required_wat_fullirrig_ww   <- as.array(.transformObject(required_wat_fullirrig_ww))
  required_wat_fullirrig_wc   <- as.array(.transformObject(required_wat_fullirrig_wc))
  irrig_yieldgainpotential    <- as.array(.transformObject(irrig_yieldgainpotential))
  avl_wat_ww                  <- as.array(.transformObject(0))
  avl_wat_wc                  <- as.array(.transformObject(0))
  access_shr                  <- as.array(.transformObject(access_shr))

  ################################################
  ####### River basin discharge allocation #######
  ################################################
  out     <- NULL
  if (class(selectyears)=="numeric") {
    selectyears <- paste0("y", selectyears)
  }

  # list of objects that are inputs and outputs to the allocation function
  l_inout <- list(discharge=discharge, required_wat_min_allocation=required_wat_min_allocation, frac_fullirrig=frac_fullirrig)
  # list of objects that are inputs to the allocation function
  l_in    <- list(irrig_yieldgainpotential=irrig_yieldgainpotential, required_wat_fullirrig_ww=required_wat_fullirrig_ww, required_wat_fullirrig_wc=required_wat_fullirrig_wc, gainthreshold=gainthreshold, avl_wat_ww=avl_wat_ww, avl_wat_wc=avl_wat_wc, access_shr=access_shr)

  # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
  glocellrank     <- calcOutput("IrrigCellranking", climatetype=climatetype, cellrankyear=selectyears, method=rankmethod, proxycrop=proxycrop, iniyear=iniyear, aggregate=FALSE)
  glocellrank     <- as.array(glocellrank)[,,1]

  for (y in selectyears) {
    l_inout <- toolDischargeAllocation(y=y, rs=rs, l_inout=l_inout, l_in=l_in, glocellrank=glocellrank, allocationrule=allocationrule)
  }

  if (output=="discharge") {
    # Main output for MAgPIE: water available for agricultural consumption
    out         <- as.magpie(l_inout$discharge, spatial=1)
    description <- "Cellular discharge after accounting for known human uses along the river"
  } else if (output=="frac_fullirrig") {
    # Main output for MAgPIE: water available for agricultural withdrawal
    out         <- as.magpie(l_inout$frac_fullirrig, spatial=1)
    description <- "Fraction of full irrigation requirements that can be fulfilled"
  } else {
    stop("specify outputtype")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}