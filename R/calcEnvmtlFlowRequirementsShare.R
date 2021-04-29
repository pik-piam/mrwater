#' @title       calcEnvmtlFlowRequirementsShare
#' @description This function calculates environmental flow requirements (EFR) (as share of discharge) based on LPJmL monthly discharge following Smakthin et al. (2004)
#'
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop. Note: Default version arguments need to be updated when new versions are used!
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod   EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#'
#' @importFrom magclass collapseNames new.magpie getYears
#' @importFrom madrat calcOutput
#' @importFrom stats quantile
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution representing share of discharge that is reserved for environmental flows
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("EnvmtlFlowRequirementsShare", aggregate=FALSE) }
#'

calcEnvmtlFlowRequirementsShare <- function(lpjml, climatetype, EFRmethod) {

  # Long-term reference time frame for EFR calculation:
  EFRyears <- c(1985:2015)

  # retrieve ecosystem preservation status:
  preservationstatus <- strsplit(EFRmethod, ":")[[1]][2]

  # Monthly Discharge from LPJmL (raw: including variation)
  monthly_discharge     <- calcOutput("LPJmL_new", version=lpjml["natveg"], subtype="mdischarge", climatetype=climatetype, stage="raw", years=EFRyears, aggregate=FALSE)

  # Transform to array (faster calculation)
  monthly_discharge     <-  as.array(collapseNames(monthly_discharge))

  # Mean annual discharge
  mean_annual_discharge <- apply(monthly_discharge, MARGIN=c(1), sum) / length(EFRyears)

  # Mean monthly flow
  mean_monthly_flow     <- apply(monthly_discharge, MARGIN=c(1,3), mean)

  if (grepl("Smakhtin", EFRmethod)) {

    # LFR: dependent on chosen ecosystem preservation status
    if (preservationstatus=="fair") {
      # "fair condition" -> Q90 (discharge, which is exceeded 90 percent of the time on average throughout a year)
      LFR_val  <- 0.1
    } else if (preservationstatus=="good") {
      # "good condition" -> Q75 (discharge, which is exceeded 75 percent of the time on average throughout a year)
      LFR_val  <- 0.25
    } else if (preservationstatus=="natural") {
      # "natural condition" -> Q50 (discharge, which is exceeded 50 percent of the time on average throughout a year)
      LFR_val  <- 0.5
    } else {
      stop("Please select strictness of Environmental Flow Requirements: Options of the preservationstatus argument are fair, good, or natural riverrine ecosystem condition")
    }

    # HFR: high flow requirements dependent on Q90
    HFR_Q90_less10 <- 0.2    # for Q90 < 10percent of total water
    HFR_Q90_10_20  <- 0.15   # for 10percent < Q90 < 20percent of total water
    HFR_Q90_20_30  <- 0.07   # for 20percent < Q90 < 30percent of total water
    HFR_Q90_more30 <- 0.00   # for Q90 > 30percent of total water

    ### Calculate LFRs
    ## Note: LFRs correspond to the Q90/Q75/Q50-value (depending on EFR conservation status)
    ## This is calculated via the 10%/25%/50%-quantile of monthly discharge.
    # Get the monthly LFR_val quantile for all cells (across selected long-term reference time period)
    LFR_quant <- apply(monthly_discharge, MARGIN=c(1), quantile, probs=LFR_val)
    # Yearly LFRs
    LFR       <- LFR_quant * 12
    # LFR-Share: LFR as fraction of mean annual discharge
    LFR       <- ifelse(mean_annual_discharge>0, LFR/mean_annual_discharge, 0)

    ### Calculate HFR-Share
    # Yearly Q90
    Q90       <- apply(monthly_discharge, MARGIN=c(1), quantile, probs=0.1) * 12

    ## Note: "For rivers with low Q90 values, high-flow events are important
    ## for river channel maintenance, wetland flooding, and riparian vegetation.
    ## HFRs of 20% of available water are therefore assigned to rivers with a
    ## low fraction of Q90 in total discharge. Rivers with a more stable flow
    ## regime receive a lower HFR." (Bonsch et al. 2015)
    HFR <- LFR
    HFR <- NA
    HFR[Q90<0.1*mean_annual_discharge]  <- HFR_Q90_less10
    HFR[Q90>=0.1*mean_annual_discharge] <- HFR_Q90_10_20
    HFR[Q90>=0.2*mean_annual_discharge] <- HFR_Q90_20_30
    HFR[Q90>=0.3*mean_annual_discharge] <- HFR_Q90_more30
    HFR[mean_annual_discharge<=0]       <- HFR_Q90_less10

    # EFR Share
    EFR <- LFR + HFR
    # EFR fraction capped to 1
    EFR <- ifelse(EFR>1, 1, EFR)

  } else if (grepl("VMF", EFRmethod)) {

    if (preservationstatus!="fair") stop("The VMF method as of Pastor et al. (2014) is parametrized to a fair ecosystem preservation status. The good and natural argument can only be combined when the Smakhtin method is used.")

    # Monthly value of mean annual flow:
    mean_annual_flow  <- as.magpie(mean_annual_discharge / 12, spatial=1)
    # Mean monthly flow
    mean_monthly_flow <- as.magpie(mean_monthly_flow, spatial=1)

    ## Determination of LF/IF/HF-months according to Pastor et al. (2014)
    LFmonths <- mean_monthly_flow <= 0.4 * mean_annual_flow
    HFmonths <- mean_monthly_flow >  0.8 * mean_annual_flow
    IFmonths <- !LFmonths & !HFmonths

    EFR <- new.magpie(cells_and_regions=getCells(mean_annual_flow), names = getNames(mean_monthly_flow))
    EFR[LFmonths] <- 0.6  * mean_monthly_flow[LFmonths]
    EFR[IFmonths] <- 0.45 * mean_monthly_flow[IFmonths]
    EFR[HFmonths] <- 0.3  * mean_monthly_flow[HFmonths]

    # Yearly EFRs
    EFR <- as.array(dimSums(EFR, dim=3))

    # EFR as share of natural discharge
    EFR <- ifelse(mean_annual_discharge>0, EFR / mean_annual_discharge, 0)

  } else {
    stop("Please select EFR method (Smakhtin, VMF) and the respective strictness of EFRs (acquatic ecosystem status: fair, good, natural) separate by : separator")
  }


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
