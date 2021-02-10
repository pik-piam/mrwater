#' @title calcEnvmtlFlowRequirements
#' @description This function calculates environmental flow requirements (EFR) for MAgPIE based on EFR share calculated from LPJmL monthly discharge following Smakthin et al. (2004)
#'
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param cells       Number of cells to be reported: lpjcell (67420, default) or magpiecell (59199)
#' @param LFR_val        Strictness of environmental flow requirements
#' @param HFR_LFR_less10 High flow requirements (share of total water for cells) with LFR<10percent of total water
#' @param HFR_LFR_10_20  High flow requirements (share of total water for cells) with 10percent < LFR < 20percent of total water
#' @param HFR_LFR_20_30  High flow requirements (share of total water for cells) with 20percent < LFR < 30percent of total water
#' @param HFR_LFR_more30 High flow requirements (share of total water for cells) with LFR>30percent of total water
#' @param EFRyears Long-term reference time frame for EFR calculation
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#'
#' @importFrom magclass collapseNames as.magpie
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("EnvmtlFlowRequirements", aggregate=FALSE) }
#'

calcEnvmtlFlowRequirements <- function(version="LPJmL4", selectyears="all",
                                       climatetype="HadGEM2_ES:rcp2p6:co2", cells="lpjcell",
                                       time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015",
                                       LFR_val=0.1, HFR_LFR_less10=0.2, HFR_LFR_10_20=0.15, HFR_LFR_20_30=0.07, HFR_LFR_more30=0.00,
                                       EFRyears=c(1985:2015)){
  # Read in EFR share
  EFR_magpie_frac <- calcOutput("EnvmtlFlowRequirementsShare", version=version, climatetype=climatetype, aggregate=FALSE,
                                  LFR_val=LFR_val, HFR_LFR_less10=HFR_LFR_less10, HFR_LFR_10_20=HFR_LFR_10_20, HFR_LFR_20_30=HFR_LFR_20_30, HFR_LFR_more30=HFR_LFR_more30,
                                  EFRyears=EFRyears)

  # Read in natural discharge (in mio. m^3 / yr)
  discharge_nat   <- collapseNames(calcOutput("RiverNaturalFlows", selectyears=selectyears, version=version, aggregate=FALSE,
                                             climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof,
                                             harmonize_baseline=harmonize_baseline, ref_year=ref_year)[,,"discharge_nat"])

  # Calculate EFRs (mio. m^3 / yr)
  EFR <- EFR_magpie_frac*discharge_nat

  ### Correct number of cells and transform to magpie object
  if (cells=="magpiecell"){
    EFR <- EFR[magclassdata$cellbelongings$LPJ_input.Index]
    EFR <- as.magpie(EFR, spatial=1)
    dimnames(EFR)[[1]] <- paste(magclassdata$half_deg$region,1:59199,sep='.')
  }

  # Check for NAs
  if(any(is.na(EFR))){
    stop("produced NA EFR")
  }

  return(list(
    x=EFR,
    weight=NULL,
    unit="mio. m^3",
    description="Environmental flow requirements per cell per year",
    isocountries=FALSE))
}
