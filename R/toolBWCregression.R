#' @title       toolBWCregression
#' @description This function calculates the functional relationship between
#'              crop consumptive blue water use for and grass consumptive blue water use
#'              using a linear fit and returns the coefficients and summary statistics as a list
#'
#' @param y Dependent variable (y) for blue water consumption regression
#'          (crop- and system-specific crop blue water consumption for given system in main season)
#'          as magpie object that is already filtered
#' @param x Independent variable (x) for blue water consumption regression
#'          (crop- and system-specific grass blue water consumption in irrigated growing period of respective crop)
#'          as magpie object that is already filtered
#'
#' @return list of regression coefficients and summary statistics
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("BlueWaterConsumptionOff", aggregate = FALSE)
#' }
#'
#' @importFrom stats lm coef
#' @importFrom utils capture.output write.csv
#' @export

toolBWCregression <- function(y, x) {

  yrs  <- getItems(y, dim = 2)
  crps <- getItems(y, dim = 3)

  # Regression (linear fit between crop blue water consumption
  # by irrigation system and grass evapotranspiration) to derive
  # coefficient used to derive off-season blue water consumption
  # Dependent variable (y): crop blue water consumption for given system in main season
  # Independent variable (x): delta grass ET in irrigated growing period of respective crop
  a <- b <- new.magpie(cells_and_regions = getItems(y, dim = 1),
                       years = yrs,
                       names = crps,
                       fill = 0)
  r2 <- rse <- new.magpie(cells_and_regions = "GLO",
                          years = yrs,
                          names = crps,
                          fill = NA)
  tmp <- new.magpie(cells_and_regions = c("a", "b", "R2", "RSE"),
                    years = yrs,
                    names = crps,
                    fill = NA)

  # regression is executed for each year since the relationship can change over time
  for (yr in yrs) {
    # regression is executed for each crop and each irrigation system separately
    for (i in crps) {
      if (all(is.na(y[, yr, i]))) {
        # For case of crops that are non-suitable for multiple cropping,
        # second season blue water consumption is 0
        a[, yr, i] <- 0
        b[, yr, i] <- 0
      } else {
        # Linear regression
        fit <- stats::lm(y ~ x,
                         na.action = "na.omit",
                         data = data.frame(y = as.vector(y[, yr, i]),
                                           x = as.vector(x[, yr, i])))
        # Extract intercept and slope coefficient for each crop and system and year
        a[, yr, i] <- tmp["a", yr, i] <- stats::coef(fit)[1]
        b[, yr, i] <- tmp["b", yr, i] <- stats::coef(fit)[2]
        # Extract statistical information of regression
        r2[, yr, i] <- tmp["R2", yr, i] <- summary(fit)$r.squared
        rse[, yr, i] <- tmp["RSE", yr, i] <- summary(fit)$sigma
      }
    }
  }

  # Check goodness of fit
  r2min <- min(r2, na.rm = TRUE)
  r2minWhere <- magclass::where(r2 == r2min)$true$individual
  r2minWhere <- paste0(r2minWhere[, 2], "/", r2minWhere[, 3],
                       collapse = "; ")

  # Return note for low R2
  mstools::toolExpectTrue(all(r2 > 0.7, na.rm = TRUE),
                          paste0("BWC regression has acceptable R2: minimum R2 = ",
                                 round(r2min, digits = 3), " for ", r2minWhere),
                          level = 0, falseStatus = "note")

  # Save table with regression outputs for checking
  tmp        <- as.data.frame(tmp)[, c("Region", "Data1", "Data2", "Value")]
  names(tmp) <- c("Regression", "System", "Crop", "Value")
  tmp$Value  <- round(tmp$Value, digits = 2)
  tmp        <- paste0(capture.output({
    write.csv(tmp, row.names = FALSE)
  }))
  writeLines(tmp, "BWCregression.log")

  out <- list(a = a,
              b = b,
              r2 = r2,
              rse = rse)

  return(out)
}
