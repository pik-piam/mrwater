#' @title       calcEnvmtlFlowRequirementsShare
#' @description This function calculates environmental flow requirements (EFR) (as share of discharge) based on LPJmL monthly discharge following Smakthin et al. (2004)
#'
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop. Note: Default version arguments need to be updated when new versions are used!
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
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

calcEnvmtlFlowRequirementsShare <- function(lpjml=c(natveg="LPJmL4_for_MAgPIE_84a69edd", crop="ggcmi_phase3_nchecks_72c185fa"), climatetype="GSWP3-W5E5:historical") {

  #### Settings for "fair" state of aquatic ecosystem (see Smakhtin 2004 and Bonsch 2015) ####
  # Long-term reference time frame for EFR calculation:
  EFRyears <- c(1985:2015)
  # LFR: Q90
  LFR_val  <- 0.1
  # HFR: high flow requirements dependent on LFRs
  HFR_LFR_less10 <- 0.2    # for LFR < 10percent of total water
  HFR_LFR_10_20  <- 0.15   # for 10percent < LFR < 20percent of total water
  HFR_LFR_20_30  <- 0.07   # for 20percent < LFR < 30percent of total water
  HFR_LFR_more30 <- 0.00   # for LFR > 30percent of total water
  #### Settings for "fair" state of aquatic ecosystem (see Smakhtin 2004 and Bonsch 2015) ####

  ### Monthly Discharge from LPJmL (raw: including variation)
  monthly_discharge_magpie <- calcOutput("LPJmL_new", version=lpjml["natveg"], subtype="mdischarge", climatetype=climatetype, stage="raw", years=EFRyears, aggregate=FALSE)

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
  LFR       <- LFR_quant * 12

  ### Mean annual discharge
  mean_annual_discharge <- apply(monthly_discharge_magpie, MARGIN=c(1), sum) / length(years)

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
  EFR <- LFR + HFR
  # Reduce EFR to 50% of available water where it exceeds this threshold (according to Smakhtin 2004)
  EFR <- pmin(EFR, 0.5*mean_annual_discharge)
  # EFR as fraction of mean annual discharge
  EFR <- ifelse(mean_annual_discharge>0, EFR/mean_annual_discharge, 0)

  ### Transform to magpie object
  EFR                     <- as.magpie(EFR, spatial=1)
  names(dimnames(EFR))[1] <- "x.y.iso"

  # Check for NAs
  if (any(is.na(EFR))) {
    stop("produced NA EFR share")
  }

  # Check range of object
  if (any(range(EFR)>1) | any(range(EFR)<0)) {
    stop("EFR share is not between 0 and 1")
  }

  return(list(
    x=EFR,
    weight=NULL,
    unit="fraction of discharge",
    description="Environmental flow requirements share per cell per year",
    isocountries=FALSE))
}
