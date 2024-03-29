#' @title       calcIrrigationSystem
#' @description This function returns the irrigation system share initialization
#'
#' @param datasource Data source to be used:
#'                   Jaegermeyr (irrigation system share based on FAO 2014, ICID 2012 and Rohwer et al. 2007) or
#'                   LPJmL (dominant irrigation system per country)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigationSystem", datasource = "Jaegermeyr", aggregate = FALSE)
#' }
#'
#' @importFrom utils read.csv
#' @importFrom madrat readSource
#' @importFrom magclass getSets getCells collapseDim new.magpie dimSums

calcIrrigationSystem <- function(datasource) {

  ### ToDo: Delete. Will be replaced with calcIrrigSystemShr and subsequent function calcIrrigSystem

  # Jägermeyr et al. (2015): Shares of surface, sprinkler and drip irrigated areas
  # (Note: compiled from FAO (2014), ICID (2012), Rohwer et al. (2007))
  if (datasource == "Jaegermeyr") {

    # Read in datasource
    x           <- readSource("IrrigationSystem", convert = FALSE, subtype = datasource)
    getNames(x) <- gsub("shr_AEI_", "", getNames(x))
  }

  # Irrigation functional type (IFT) from LPJmL representing the dominant irrigation system per country
  # (Note: share of 100% of dominant system assumed)
  if (datasource == "LPJmL") {

    # Read in datasource
    tmp <- readSource("IrrigationSystem", convert = FALSE, subtype = datasource)

    # Merge to obtain one magpie object containing irrigation system shares
    # (share of irrigated area per irrigation system)
    x   <- new.magpie(cells_and_regions = getCells(tmp),
                      years = NULL,
                      names = c("surface", "sprinkler", "drip"),
                      fill = 0)

    # Surface is dominant system:
    x[, , "surface"][tmp == 1]   <- 1
    x[, , "sprinkler"][tmp == 1] <- 0
    x[, , "drip"][tmp == 1]      <- 0

    # Sprinkler is dominant system:
    x[, , "surface"][tmp == 2]   <- 0
    x[, , "sprinkler"][tmp == 2] <- 1
    x[, , "drip"][tmp == 2]      <- 0

    # Drip is dominant system
    x[, , "surface"][tmp == 3]   <- 0
    x[, , "sprinkler"][tmp == 3] <- 0
    x[, , "drip"][tmp == 3]      <- 1
  }

  # When all three shares are 0, it is assumed that 100% of irrigated land (if any exists) is surface irrigation
  x[, , "surface"][which(x[, , "surface"] == 0 & x[, , "sprinkler"] == 0 & x[, , "drip"] == 0)] <- 1

  # Dimension and element names
  getSets(x, fulldim = FALSE)[3] <- "system"

  # Checks
  if (any(is.na(x))) {
    stop("produced NA irrigation system share")
  }
  if (any(round(dimSums(x, dim = 3)) != 1)) {
    stop("sum over shares not equal to 1")
  }

  return(list(x            = x,
              weight       = NULL,
              unit         = "share",
              description  = "irrigation system share (share of irrigated area)",
              isocountries = FALSE))
}
