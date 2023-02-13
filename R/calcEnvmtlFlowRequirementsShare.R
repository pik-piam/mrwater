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
#' @importFrom mrcommons toolLPJmLVersion
#' @importFrom stats quantile
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
  refYears <- c(1985:2015)

  cfg <- toolLPJmLVersion(version = lpjml[["natveg"]],
                          climatetype = climatetype)

  # retrieve ecosystem preservation status:
  preservationstatus <- strsplit(efrMethod, ":")[[1]][2]

  # Monthly Discharge from LPJmL based on historical baseline (raw: including variation)
  monthlyDischarge   <- setYears(calcOutput("LPJmL_new", version = lpjml[["natveg"]],
                                             subtype = "mdischarge", climatetype = cfg$baseline_hist,
                                             stage = "raw", years = refYears, aggregate = FALSE),
                                  refYears)

  # Transform to array (faster calculation)
  monthlyDischarge    <- as.array(collapseNames(monthlyDischarge))

  # Mean annual discharge
  meanAnnualDischarge <- apply(monthlyDischarge,
                               MARGIN = c(1), sum) / length(refYears)

  # Mean monthly flow
  meanMonthlyFlow     <- as.magpie(apply(monthlyDischarge,
                                         MARGIN = c(1, 3), mean),
                                   spatial = 1)

  if (grepl("Smakhtin", efrMethod)) {

    # LFR: dependent on chosen ecosystem preservation status
    if (preservationstatus == "fair") {
      # "fair condition" -> Q90 (discharge, which is exceeded 90 percent of the time on average throughout a year)
      valueLFR  <- 0.1
    } else if (preservationstatus == "good") {
      # "good condition" -> Q75 (discharge, which is exceeded 75 percent of the time on average throughout a year)
      valueLFR  <- 0.25
    } else if (preservationstatus == "natural") {
      # "natural condition" -> Q50 (discharge, which is exceeded 50 percent of the time on average throughout a year)
      valueLFR  <- 0.5
    } else {
      stop("Please select strictness of Environmental Flow Requirements:
           Options of the preservationstatus argument are fair, good, or natural
           riverrine ecosystem condition")
    }

    # HFR: high flow requirements dependent on Q90
    hfrQ90less10 <- 0.2    # for Q90 < 10percent of total water
    hfrQ90p10p20 <- 0.15   # for 10percent < Q90 < 20percent of total water
    hfrQ90p20p30 <- 0.07   # for 20percent < Q90 < 30percent of total water
    hfrQ90more30 <- 0.00   # for Q90 > 30percent of total water

    ### Calculate LFRs
    ## Note: LFRs correspond to the Q90/Q75/Q50-value (depending on EFR conservation status)
    ## This is calculated via the 10%/25%/50%-quantile of monthly discharge.
    # Get the monthly valueLFR quantile for all cells (across selected long-term reference time period)
    quantileLFR <- apply(monthlyDischarge, MARGIN = c(1),
                         quantile, probs = valueLFR)
    # Yearly LFRs
    lfr       <- quantileLFR * 12
    # LFR-Share: LFR as fraction of mean annual discharge
    lfr       <- as.magpie(ifelse(meanAnnualDischarge > 0,
                                    lfr / meanAnnualDischarge,
                                  0),
                           spatial = 1)

    ### Calculate HFR-Share
    # Yearly Q90
    q90       <- as.magpie(apply(monthlyDischarge, MARGIN = c(1),
                                 quantile, probs = 0.1) * 12,
                           spatial = 1)

    meanAnnualDischarge <- as.magpie(meanAnnualDischarge, spatial = 1)

    ## Note: "For rivers with low Q90 values, high-flow events are important
    ## for river channel maintenance, wetland flooding, and riparian vegetation.
    ## HFRs of 20% of available water are therefore assigned to rivers with a
    ## low fraction of Q90 in total discharge. Rivers with a more stable flow
    ## regime receive a lower HFR." (Bonsch et al. 2015)
    hfr       <- lfr
    hfr[, , ] <- NA
    hfr[q90 < 0.1 * meanAnnualDischarge]  <- hfrQ90less10
    hfr[q90 >= 0.1 * meanAnnualDischarge] <- hfrQ90p10p20
    hfr[q90 >= 0.2 * meanAnnualDischarge] <- hfrQ90p20p30
    hfr[q90 >= 0.3 * meanAnnualDischarge] <- hfrQ90more30
    hfr[meanAnnualDischarge <= 0]         <- hfrQ90less10

    # Naming of dimensions
    lfr <- add_dimension(lfr, dim = 3.1, add = "EFR", nm = "LFR")
    hfr <- add_dimension(hfr, dim = 3.1, add = "EFR", nm = "HFR")

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
    lfMonths <- meanMonthlyFlow <= 0.4 * meanAnnualFlow
    hfMonths <- meanMonthlyFlow >  0.8 * meanAnnualFlow
    ifMonths <- !lfMonths & !hfMonths

    ## Determination of low and high flow requirements
    # LFR: low flow months + half of intermediate flow months requirements
    # HFR: half of intermediate flow months + high flow months requirements
    lfr <- hfr <- new.magpie(cells_and_regions = getCells(meanAnnualFlow),
                             names = getNames(meanMonthlyFlow), fill = 0)
    lfr[lfMonths] <- 0.6  * meanMonthlyFlow[lfMonths]
    lfr[ifMonths] <- 0.45 * meanMonthlyFlow[ifMonths] * 0.5
    hfr[ifMonths] <- 0.45 * meanMonthlyFlow[ifMonths] * 0.5
    hfr[hfMonths] <- 0.3  * meanMonthlyFlow[hfMonths]

    # Yearly LFRs and HFRs
    lfr <- dimSums(lfr, dim = 3)
    hfr <- dimSums(hfr, dim = 3)

    # LFR and HFR as share of natural discharge
    lfr <- ifelse(meanAnnualDischarge > 0, lfr / meanAnnualDischarge, 0)
    hfr <- ifelse(meanAnnualDischarge > 0, hfr / meanAnnualDischarge, 0)

    # Naming of dimensions
    lfr <- add_dimension(lfr, dim = 3.1, add = "EFR", nm = "LFR")
    hfr <- add_dimension(hfr, dim = 3.1, add = "EFR", nm = "HFR")

  } else {
    stop("Please select EFR method (Smakhtin, VMF) and the respective strictness
         of EFRs (acquatic ecosystem status: fair, good, natural) separate by : separator")
  }

  # EFR Share
  efr <- collapseNames(lfr) + collapseNames(hfr)

  # Correction where EFRs exceed 1
  hfr <- hfr - collapseNames(ifelse(efr > 1, efr - 1, 0))
  efr <- collapseNames(lfr) + collapseNames(hfr)
  efr <- add_dimension(efr, dim = 3.1, add = "EFR", nm = "EFR")

  out <- collapseNames(mbind(efr, lfr, hfr))

  names(dimnames(out))[1] <- "x.y.iso"

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA EFR, LFR, or HFR share")
  }

  # Check range of object
  if (any(range(out) > 1) || any(range(out) < 0)) {
    stop("EFR, LFR or HFR share is not between 0 and 1")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "fraction of discharge",
              description  = "Environmental flow requirements (LFR, HFR, EFR)
                              share per cell per year",
              isocountries = FALSE))
}
