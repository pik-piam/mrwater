#' @title       calcEnvmtlFlowRequirementsShare
#' @description This function calculates environmental flow requirements (EFR)
#'              (as share of discharge) based on LPJmL monthly discharge
#'
#' @param lpjml       LPJmL version used for monthly discharge
#' @param climatetype Switch between different climate scenarios
#'                    or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod   EFR method used including selected strictness of EFRs
#'                    (e.g. Smakhtin:good, VMF:fair)
#'
#' @importFrom magclass collapseNames new.magpie getYears setYears as.array as.magpie add_dimension mbind
#' @importFrom madrat calcOutput
#' @importFrom stats quantile
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution representing share of discharge
#'         that is reserved for environmental flows
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("EnvmtlFlowRequirementsShare", aggregate = FALSE)
#' }
#'
calcEnvmtlFlowRequirementsShare <- function(lpjml,
                                            climatetype, efrMethod) {

  # Long-term reference time frame for EFR calculation:
  EFRyears <- c(1985:2015)

  # retrieve ecosystem preservation status:
  preservationstatus <- strsplit(efrMethod, ":")[[1]][2]

  # Monthly Discharge from LPJmL (raw: including variation)
  monthlyDischarge    <- setYears(calcOutput("LPJmL_new", version = lpjml["natveg"],
                                             subtype = "mdischarge", climatetype = climatetype,
                                             stage = "raw", years = EFRyears, aggregate = FALSE),
                                  EFRyears)

  # Transform to array (faster calculation)
  monthlyDischarge    <- as.array(collapseNames(monthlyDischarge))

  # Mean annual discharge
  meanAnnualDischarge <- apply(monthlyDischarge, MARGIN = c(1), sum) / length(EFRyears)

  # Mean monthly flow
  meanMonthlyFlow     <- as.magpie(apply(monthlyDischarge, MARGIN = c(1, 3), mean), spatial = 1)

  if (grepl("Smakhtin", efrMethod)) {

    # LFR: dependent on chosen ecosystem preservation status
    if (preservationstatus == "fair") {
      # "fair condition" -> Q90 (discharge, which is exceeded 90 percent of the time on average throughout a year)
      LFR_val  <- 0.1
    } else if (preservationstatus == "good") {
      # "good condition" -> Q75 (discharge, which is exceeded 75 percent of the time on average throughout a year)
      LFR_val  <- 0.25
    } else if (preservationstatus == "natural") {
      # "natural condition" -> Q50 (discharge, which is exceeded 50 percent of the time on average throughout a year)
      LFR_val  <- 0.5
    } else {
      stop("Please select strictness of Environmental Flow Requirements:
           Options of the preservationstatus argument are fair, good, or natural
           riverrine ecosystem condition")
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
    LFR_quant <- apply(monthlyDischarge, MARGIN = c(1), quantile, probs = LFR_val)
    # Yearly LFRs
    LFR       <- LFR_quant * 12
    # LFR-Share: LFR as fraction of mean annual discharge
    LFR       <- as.magpie(ifelse(meanAnnualDischarge > 0, LFR / meanAnnualDischarge, 0),
                           spatial = 1)

    ### Calculate HFR-Share
    # Yearly Q90
    Q90       <- as.magpie(apply(monthlyDischarge, MARGIN = c(1), quantile, probs = 0.1) * 12,
                           spatial = 1)

    meanAnnualDischarge <- as.magpie(meanAnnualDischarge, spatial = 1)

    ## Note: "For rivers with low Q90 values, high-flow events are important
    ## for river channel maintenance, wetland flooding, and riparian vegetation.
    ## HFRs of 20% of available water are therefore assigned to rivers with a
    ## low fraction of Q90 in total discharge. Rivers with a more stable flow
    ## regime receive a lower HFR." (Bonsch et al. 2015)
    HFR       <- LFR
    HFR[, , ] <- NA
    HFR[Q90 < 0.1 * meanAnnualDischarge]  <- HFR_Q90_less10
    HFR[Q90 >= 0.1 * meanAnnualDischarge] <- HFR_Q90_10_20
    HFR[Q90 >= 0.2 * meanAnnualDischarge] <- HFR_Q90_20_30
    HFR[Q90 >= 0.3 * meanAnnualDischarge] <- HFR_Q90_more30
    HFR[meanAnnualDischarge <= 0]         <- HFR_Q90_less10

    # Naming of dimensions
    LFR <- add_dimension(LFR, dim = 3.1, add = "EFR", nm = "LFR")
    HFR <- add_dimension(HFR, dim = 3.1, add = "EFR", nm = "HFR")

  } else if (grepl("VMF", efrMethod)) {

    if (preservationstatus != "fair") {
      stop("The VMF method as of Pastor et al. (2014) is parametrized to a fair
           ecosystem preservation status. The good and natural argument can only
           be combined when the Smakhtin method is used.")
    }

    # Monthly value of mean annual flow:
    meanAnnualDischarge <- as.magpie(meanAnnualDischarge, spatial = 1)
    meanAnnualFlow      <- meanAnnualDischarge / 12

    ## Determination of LF/IF/HF-months according to Pastor et al. (2014)
    LFmonths <- meanMonthlyFlow <= 0.4 * meanAnnualFlow
    HFmonths <- meanMonthlyFlow >  0.8 * meanAnnualFlow
    IFmonths <- !LFmonths & !HFmonths

    ## Determination of low and high flow requirements
    # LFR: low flow months + half of intermediate flow months requirements
    # HFR: half of intermediate flow months + high flow months requirements
    LFR <- HFR <- new.magpie(cells_and_regions = getCells(meanAnnualFlow),
                             names = getNames(meanMonthlyFlow), fill = 0)
    LFR[LFmonths] <- 0.6  * meanMonthlyFlow[LFmonths]
    LFR[IFmonths] <- 0.45 * meanMonthlyFlow[IFmonths] * 0.5
    HFR[IFmonths] <- 0.45 * meanMonthlyFlow[IFmonths] * 0.5
    HFR[HFmonths] <- 0.3  * meanMonthlyFlow[HFmonths]

    # Yearly LFRs and HFRs
    LFR <- dimSums(LFR, dim = 3)
    HFR <- dimSums(HFR, dim = 3)

    # LFR and HFR as share of natural discharge
    LFR <- ifelse(meanAnnualDischarge > 0, LFR / meanAnnualDischarge, 0)
    HFR <- ifelse(meanAnnualDischarge > 0, HFR / meanAnnualDischarge, 0)

    # Naming of dimensions
    LFR <- add_dimension(LFR, dim = 3.1, add = "EFR", nm = "LFR")
    HFR <- add_dimension(HFR, dim = 3.1, add = "EFR", nm = "HFR")

  } else {
    stop("Please select EFR method (Smakhtin, VMF) and the respective strictness
         of EFRs (acquatic ecosystem status: fair, good, natural) separate by : separator")
  }

  # EFR Share
  EFR <- collapseNames(LFR) + collapseNames(HFR)

  # Correction where EFRs exceed 1
  HFR <- HFR - collapseNames(ifelse(EFR > 1, EFR - 1, 0))
  EFR <- collapseNames(LFR) + collapseNames(HFR)
  EFR <- add_dimension(EFR, dim = 3.1, add = "EFR", nm = "EFR")

  out <- collapseNames(mbind(EFR, LFR, HFR))

  names(dimnames(out))[1] <- "x.y.iso"

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA EFR, LFR, or HFR share")
  }

  # Check range of object
  if (any(range(out) > 1) | any(range(out) < 0)) {
    stop("EFR, LFR or HFR share is not between 0 and 1")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "fraction of discharge",
              description  = "Environmental flow requirements (LFR, HFR, EFR)
                              share per cell per year",
              isocountries = FALSE))
}
