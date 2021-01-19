#' @title calcIrrigWatRequirements
#' @description This function calculates irrigation water requirements based on LPJmL blue water consumption of plants and considering irrigation efficiencies
#'
#' @param selectyears Years to be returned
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param cells       Switch between "lpjcell" (67420) and "magpiecell" (59199)
#' @param crops       Selects "magpie" (default) or "lpjml" crops
#' @param time            Time smoothing: average, spline or raw (default) of input data to this function
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization of input data to this function, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline of input data to this function (just specify when harmonize_baseline=TRUE)
#' @param irrig_requirement  Consumptive (consumption) or non-consumptive (withdrawals) irrigation water requirements
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("IrrigWatRequirements", aggregate=FALSE) }
#'
#' @import magpiesets
#' @import magclass
#' @import madrat

calcIrrigWatRequirements <- function(selectyears="all", cells="lpjcell", crops="magpie",
                                     version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4,
                                     harmonize_baseline=FALSE, ref_year=NULL, irrig_requirement="withdrawal"){

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit=1e+12)
  on.exit(options(magclass_sizeLimit=sizelimit))

  ##############################
  ######## Read in data ########
  ##############################
  ### Mappings
  lpj_cells_map <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell")
  LPJ2MAG       <- toolGetMapping( "MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")

  ### Read in blue water consumption for irrigated crops (in m^3 per ha per yr):
  blue_water_consumption <- collapseNames(calcOutput("LPJmL", version=version, climatetype=climatetype, harmonize_baseline=harmonize_baseline, ref_year=ref_year,
                                                     time=time, averaging_range=averaging_range, dof=dof,
                                                     selectyears=selectyears, subtype="cwater_b_lpjcell", aggregate=FALSE)[,,"irrigated"])
  names(dimnames(blue_water_consumption))[1] <- "iso.cell"
  names(dimnames(blue_water_consumption))[3] <- "crop"
  years       <- getYears(blue_water_consumption)
  cropnames   <- getNames(blue_water_consumption)
  systemnames <- c("drip","sprinkler","surface")

  ### Field efficiencies from Jägermeyr et al. (global values) [placeholder!]
  ### Alternatively: use regional efficiencies from Sauer et al. (2010), Table 5,
  field_efficiency                <- new.magpie(1:67420,years,sort(paste(systemnames, rep(cropnames,3), sep=".")),sets=c("iso.cell","year","system.crop"))
  getCells(field_efficiency)      <- paste(lpj_cells_map$ISO,1:67420,sep=".")
  field_efficiency[,,"drip"]      <- 0.88 # Sauer: 0.8-0.93
  field_efficiency[,,"sprinkler"] <- 0.78 # Sauer: 0.6-0.86
  field_efficiency[,,"surface"]   <- 0.52 # Sauer: 0.25-0.5
  ### Use field efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?]

  ### Conveyance efficiency proxy [placeholder]
  conveyance_efficiency                <- new.magpie(1:67420,years,sort(paste(systemnames, rep(cropnames,3), sep=".")),sets=c("iso.cell","year","system.crop"))
  getCells(conveyance_efficiency)      <- paste(lpj_cells_map$ISO,1:67420,sep=".")
  conveyance_efficiency[,,"drip"]      <- 0.95
  conveyance_efficiency[,,"sprinkler"] <- 0.95
  conveyance_efficiency[,,"surface"]   <- 0.7
  ### Use field efficiency from LPJmL here (by system, on 0.5 degree) [Does it vary by year?]

  ##############################
  ######## Calculations ########
  ##############################

  # Calculate project efficiency from given field and conveyance efficiencies
  project_efficiency     <- field_efficiency * conveyance_efficiency

  # Water withdrawal = crop water consumption + field losses + conveyance losses
  water_withdrawal       <- blue_water_consumption/project_efficiency

  # Conveyance loss (from river to field)
  conveyance_loss        <- water_withdrawal*(1-conveyance_efficiency)

  # consumptive irrigation water = consumptive plant transpiration + evaporative conveyance loss
  # (Note: According to Rost et al. (2007) 50% of conveyance loss are evaporative)
  water_consumption      <- blue_water_consumption + 0.5*conveyance_loss

  # Output: irrigation water requirements (consumption or withdrawals)
  if (irrig_requirement=="consumption"){
    irrig_requirements <- water_consumption
  } else if (irrig_requirement=="withdrawal"){
    irrig_requirements <- water_withdrawal
  } else {
    stop("Specify consumption or withdrawal in irrig_requirement")
  }

  # Aggregate to MAgPIE crops
  if (crops=="magpie") {
    irrig_requirements  <- toolAggregate(irrig_requirements, LPJ2MAG, from="LPJmL", to="MAgPIE", dim=3.1, partrel=TRUE)
  }

  if(selectyears!="all"){
    years               <- sort(findset(selectyears,noset="original"))
    irrig_requirements  <- irrig_requirements[,years,]
  }

  ### Correct number of cells
  if (cells=="lpjcell"){
    out <- irrig_requirements
  } else if (cells=="magpiecell"){
    irrig_requirements <- irrig_requirements[magclassdata$cellbelongings$LPJ_input.Index,,]
    irrig_requirements <- toolCell2isoCell(irrig_requirements)
    out <- irrig_requirements
  } else {
    stop("Cell argument not supported. Select lpjcell for 67420 cells or magpiecell for 59199 cells")
  }

  # Check for NAs and negative values
  if(any(is.na(out))){
    stop("produced NA irrigation water requirements")
  }
  if(any(out<0)){
    stop("produced negative irrigation water requirements")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="m^3 per ha per yr",
    description="Irrigation water requirements for irrigation for different crop types under different irrigation systems",
    isocountries=FALSE))
}
