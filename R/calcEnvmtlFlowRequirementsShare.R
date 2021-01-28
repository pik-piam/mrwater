#' @title calcEnvmtlFlowRequirementsShare
#' @description This function calculates environmental flow requirements (EFR) (as share of discharge) based on LPJmL monthly discharge following Smakthin et al. (2004)
#'
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param LFR_val        Strictness of environmental flow requirements
#' @param HFR_LFR_less10 High flow requirements (share of total water for cells) with LFR<10percent of total water
#' @param HFR_LFR_10_20  High flow requirements (share of total water for cells) with 10percent < LFR < 20percent of total water
#' @param HFR_LFR_20_30  High flow requirements (share of total water for cells) with 20percent < LFR < 30percent of total water
#' @param HFR_LFR_more30 High flow requirements (share of total water for cells) with LFR>30percent of total water
#' @param EFRyears Long-term reference time frame for EFR calculation
#'
#' @importFrom magclass collapseNames new.magpie getYears
#' @importFrom madrat calcOutput
#' @importFrom stats quantile
#'
#' @return magpie object in cellular resolution representing share of discharge that is reserved for environmental flows
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("EnvmtlFlowRequirementsShare", aggregate=FALSE) }
#'

calcEnvmtlFlowRequirementsShare <- function(version="LPJmL4", climatetype="HadGEM2_ES:rcp2p6:co2",
                                       LFR_val=0.1, HFR_LFR_less10=0.2, HFR_LFR_10_20=0.15, HFR_LFR_20_30=0.07, HFR_LFR_more30=0.00,
                                       EFRyears=c(1985:2015)){

  # Long-term reference period for EFR calculation
  EFRyears <- paste0("y",EFRyears)

  ### Monthly Discharge from LPJmL
  monthly_discharge_magpie <- calcOutput("LPJmL", selectyears=EFRyears, version=version, climatetype=climatetype, subtype="mdischarge_lpjcell", aggregate=FALSE,
                                         harmonize_baseline=FALSE, ref_year=NULL, time="raw", averaging_range=NULL, dof=NULL)

  # Extract years
  years <- getYears(monthly_discharge_magpie, as.integer=TRUE)
  # Transform to array (faster calculation)
  monthly_discharge_magpie <-  as.array(collapseNames(monthly_discharge_magpie))

  ### Calculate LFRs
  ## Note: LFRs correspond to the Q90-value (i.e. to the discharge that is exceeded in nine out of ten months)
  ## (Bonsch et al. 2015). This is calculated via the 10%-quantile of monthly discharge.

  # Get the monthly LFR_val quantile for all cells (across selected long-term reference time period)
  LFR_quant <- apply(monthly_discharge_magpie, MARGIN=c(1), quantile, probs=LFR_val)

  # Yearly LFRs
  LFR <- LFR_quant*12

  ### Mean annual discharge
  mean_annual_discharge <- apply(monthly_discharge_magpie, MARGIN=c(1), sum)/length(years)

  ### Calculate HFR
  ## Note: "For rivers with low Q90 values, high-flow events are important
  ## for river channel maintenance, wetland flooding, and riparian vegetation.
  ## HFRs of 20% of available water are therefore assigned to rivers with a
  ## low fraction of Q90 in total discharge. Rivers with a more stable flow
  ## regime receive a lower HFR." (Bonsch et al. 2015)
  HFR <- LFR
  HFR <- NA
  HFR[LFR<0.1*mean_annual_discharge]  <- HFR_LFR_less10 * mean_annual_discharge[LFR<0.1*mean_annual_discharge]
  HFR[LFR>=0.1*mean_annual_discharge] <- HFR_LFR_10_20  * mean_annual_discharge[LFR>=0.1*mean_annual_discharge]
  HFR[LFR>=0.2*mean_annual_discharge] <- HFR_LFR_20_30  * mean_annual_discharge[LFR>=0.2*mean_annual_discharge]
  HFR[LFR>=0.3*mean_annual_discharge] <- HFR_LFR_more30 * mean_annual_discharge[LFR>=0.3*mean_annual_discharge]
  HFR[mean_annual_discharge<=0]       <- 0

  ### EFR
  EFR <- LFR+HFR
  # Reduce EFR to 50% of available water where it exceeds this threshold (according to Smakhtin 2004)
  EFR <- pmin(EFR, 0.5*mean_annual_discharge)
  # EFR as fraction of mean annual discharge
  EFR <- ifelse(mean_annual_discharge>0, EFR/mean_annual_discharge, 0)

  ### Transform to magpie object
  EFR <- as.magpie(EFR, spatial=1)

  # Check for NAs
  if(any(is.na(EFR))){
    stop("produced NA EFR")
  }

  return(list(
    x=EFR,
    weight=NULL,
    unit="fraction of discharge",
    description="Environmental flow requirements share per cell per year",
    isocountries=FALSE))
}