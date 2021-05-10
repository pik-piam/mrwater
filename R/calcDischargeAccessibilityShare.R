#' @title       calcDischargeAccessibilityShare
#' @description This function calculates the share of discharge that is useable
#'              given the variability of monthly flows. If discharge is highly
#'              variable, it is harder to bring into productive use and therefore
#'              water availability (for human use) is reduced.
#'
#' @param lpjml             LPJmL version required for respective inputs: natveg or crop
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears       Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param accessibilityrule Method used: Quantile method (Q) or coefficient of variation (CV) combined with Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5) or base value for expontential curve separated by :
#'
#' @importFrom magclass collapseNames getYears setYears as.magpie mbind
#' @importFrom madrat calcOutput
#' @importFrom stats quantile sd
#'
#' @return magpie object in cellular resolution representing share of discharge that is reserved for environmental flows
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("DischargeAccessibilityShare", aggregate=FALSE) }
#'

calcDischargeAccessibilityShare <- function(lpjml, selectyears, climatetype, accessibilityrule) {

  # retrieve function arguments
  coeff  <- as.numeric(as.list(strsplit(accessibilityrule, split=":"))[[1]][2])
  method <- as.list(strsplit(accessibilityrule, split=":"))[[1]][1]

  # Monthly Discharge from LPJmL (raw: including variation)
  monthly_discharge_lpjml <- calcOutput("LPJmL_new", version=lpjml["natveg"], subtype="mdischarge", climatetype=climatetype, stage="raw", aggregate=FALSE)

  # Extract years
  years         <- getYears(monthly_discharge_lpjml, as.integer=TRUE)
  if (class(selectyears)!="numeric") {
    selectyears <- as.numeric(gsub("y", "", selectyears))
  }

  out <- NULL
  for (y in selectyears) {

    # Long-term reference time frame for discharge variability calculation
    if ((y-30) > years[1]) {
      longterm_period <- seq(y - 30, y, by=1)
    } else {
      longterm_period <- seq(1980, 2010, by=1)
    }
    monthly_discharge <- monthly_discharge_lpjml[,longterm_period,]

    # Transform to array (faster calculation)
    monthly_discharge <- as.array(collapseNames(monthly_discharge))

    # Variability Accessibility Method:
    if (method=="Q") {
      ### Quantile Variability Threshold ###
      # Get the variability discharge quantile (across selected long-term reference time period) with selected threshold
      Discharge_quant   <- apply(monthly_discharge, MARGIN=c(1), quantile, probs=coeff)

      # Share of monthly discharge that is accessible for human use
      x <- apply(pmin(monthly_discharge, Discharge_quant), MARGIN=1, sum) / apply(monthly_discharge, MARGIN=1, sum)
      x[apply(monthly_discharge, MARGIN=1, sum)==0] <- 0

    } else if (method=="CV") {
      ### Variation Coefficient Method ###
      # Mean and standard deviation of discharge
      mean_discharge <- apply(monthly_discharge, MARGIN=1, mean)
      std_discharge  <- apply(monthly_discharge, MARGIN=1, sd)

      # Coefficient of Variation
      CV <- ifelse(mean_discharge>0, std_discharge / mean_discharge, 0)

      # Functional form: The higher the variability, the harder it is to access the water
      # Accessibility coefficient (share of discharge that is accessible) decreases with variability
      x  <- coeff^(-CV)
    } else {
      stop("Please select a accessibility rule defining the method of variability determination and threshold value or functional form in the accessibilityrule argument: e.g. Q:0.25, CV:2")
    }

    x <- setYears(as.magpie(x, spatial=1), y)
    names(dimnames(x))[1] <- "x.y.iso"

    out <- mbind(out, x)
  }

  # Check for NAs
  if (any(is.na(out))) {
    stop("Produced NA Discharge Accessibility Share")
  }

  # Check range of object
  if (any(range(out)>1) | any(range(out)<0)) {
    stop("Discharge Accessibility Share is not between 0 and 1")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="fraction of discharge",
    description="Share of discharge that is accessible per cell per year",
    isocountries=FALSE))
}
