#' @title       calcDischargeAccessibilityShare
#' @description This function calculates the share of discharge that is useable
#'              given the variability of monthly flows. If discharge is highly
#'              variable, it is harder to bring into productive use and therefore
#'              water availability (for human use) is reduced.
#'
#' @param lpjml             LPJmL version used
#' @param climatetype       Switch between different climate scenarios
#'                          or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears       Years to be returned
#'                          (Note: does not affect years of harmonization or smoothing)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          is inaccessible (e.g. Q:1 all discharge accessible,
#'                          Q:0.75 0.75-quantile is accessible everything that is more variable inaccessible)
#'                          or base value for exponential curve, separated by : (CV:2)
#'
#' @importFrom magclass collapseNames getYears setYears as.magpie mbind
#' @importFrom madrat calcOutput
#' @importFrom stats quantile sd
#'
#' @return magpie object in cellular resolution representing share of discharge
#'         that is accessible to humans
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("DischargeAccessibilityShare", aggregate = FALSE)
#' }
#'
calcDischargeAccessibilityShare <- function(lpjml, selectyears, climatetype,
                                            accessibilityrule) {

  # retrieve function arguments
  coeff  <- as.numeric(as.list(strsplit(accessibilityrule, split = ":"))[[1]][2])
  method <- as.list(strsplit(accessibilityrule, split = ":"))[[1]][1]

  # Monthly Discharge from LPJmL (raw: including variation)
  monthlyDischargeLPJmL <- calcOutput("LPJmL_new", subtype = "mdischarge",
                                        version = lpjml[["natveg"]], climatetype = climatetype,
                                        stage = "raw", aggregate = FALSE)

  # Extract years
  years         <- getYears(monthlyDischargeLPJmL, as.integer = TRUE)
  if (!is.numeric(selectyears)) {
    selectyears <- as.numeric(gsub("y", "", selectyears))
  }

  out <- NULL
  for (y in selectyears) {

    # Long-term reference time frame for discharge variability calculation
    if ((y - 30) > years[1]) {
      longtermPeriod <- seq(y - 30, y, by = 1)
    } else {
      longtermPeriod <- seq(1980, 2010, by = 1)
    }
    monthlyDischarge <- monthlyDischargeLPJmL[, longtermPeriod, ]

    # Transform to array (faster calculation)
    monthlyDischarge <- as.array(collapseNames(monthlyDischarge))

    # Variability Accessibility Method:
    if (method == "Q") {

      ### Quantile Variability Threshold ###
      # Get the variability discharge quantile (across selected long-term reference time period) with selected threshold
      dischargeQuant   <- apply(monthlyDischarge, MARGIN = c(1), quantile, probs = coeff)

      # Share of monthly discharge that is accessible for human use
      x <- apply(pmin(monthlyDischarge, dischargeQuant), MARGIN = 1, sum) /
           apply(monthlyDischarge, MARGIN = 1, sum)
      x[apply(monthlyDischarge, MARGIN = 1, sum) == 0] <- 0

    } else if (method == "CV") {

      ### Variation Coefficient Method ###
      # Mean and standard deviation of discharge
      meanDischarge <- apply(monthlyDischarge, MARGIN = 1, mean)
      stdDischarge  <- apply(monthlyDischarge, MARGIN = 1, sd)

      # Coefficient of Variation
      cv <- ifelse(meanDischarge > 0, stdDischarge / meanDischarge, 0)

      # Functional form: The higher the variability, the harder it is to access the water
      # Accessibility coefficient (share of discharge that is accessible) decreases with variability
      x  <- coeff^(-cv)

    } else {
      stop("Please select an accessibility rule defining the method of variability
           determination and threshold value or functional form in the accessibilityrule
           argument: e.g. Q:0.25, CV:2")
    }

    x <- setYears(as.magpie(x, spatial = 1), y)
    names(dimnames(x))[1] <- "x.y.iso"

    out <- mbind(out, x)
  }

  # Check for NAs
  if (any(is.na(out))) {
    stop("Produced NA Discharge Accessibility Share")
  }

  # Check range of object
  if (any(range(out) > 1) | any(range(out) < 0)) {
    stop("Discharge Accessibility Share is not between 0 and 1")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "fraction of discharge",
              description  = "Share of discharge that is accessible per cell per year",
              isocountries = FALSE))
}
