#' @title calcAvlWater
#' @description This function calculates water availability for MAgPIE retrieved from LPJmL
#'              using the old water aggregation logic from Bonsch et al.
#'              This function will be replaced by the mrwater logic.
#'              It can be deleted when module 25_irrigation in MAgPIE is default
#'              and modules 41,42,43 are retired.
#'
#' @param lpjml       Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @param stage       Degree of processing: raw, smoothed, harmonized, harmonized2020
#' @param seasonality grper (default): water available in growing period per year;
#'                    total: total water available throughout the year;
#'                    monthly: monthly water availability (for further processing, e.g. in calcEnvmtlFlow)
#'
#' @import magclass
#' @import madrat
#' @importFrom mstools toolHarmonize2Baseline
#' @importFrom mrlandcore toolLPJmLHarmonize
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Kristine Karstens, Abhijeet Mishra
#'
#' @examples
#' \dontrun{
#' calcOutput("AvlWater", aggregate = FALSE)
#' }
calcAvlWater <- function(lpjml = "lpjml5.9.16-m1",
                         climatetype = "MRI-ESM2-0:ssp370",
                         stage = "harmonized2020", seasonality = "grper") {

  cfg <- toolLPJmLHarmonize(lpjmlversion = lpjml,
                            climatetype = climatetype)

  ######################################################
  ############ Water availability per cell #############
  # Runoff is distributed across the river basin cells #
  # based on discharge-weighted algorithm              #
  ######################################################
  if (stage %in% c("raw", "smoothed")) {
    ### Monthly Discharge (unit (after calcLPJmL): mio. m^3/month)
    monthDischargeMAG <- calcOutput("LPJmLTransform", subtype = "pnv:discharge",
                                    lpjmlversion = cfg$readinVersion, climatetype = climatetype,
                                    monthly = TRUE,
                                    stage = "raw:cut", aggregate = FALSE)
    getItems(monthDischargeMAG, dim = "month") <- c(1:12)

    ### Monthly Runoff (raw) (in mio. m^3/month)
    yrs <- getItems(monthDischargeMAG, dim = 2)
    monthRunoffMAG <- calcOutput("RunoffMonthly", lpjml = cfg$readinVersion,
                                 climatetype = climatetype,
                                 aggregate = FALSE)[, yrs, ]
    getItems(monthRunoffMAG, dim = "month") <- c(1:12)

    ## River basin water allocation algorithm:
    # Read in river structure
    rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds",
                              package = "mrwater"))
    basinCode <- rs$endcell

    if (any(paste(getItems(monthRunoffMAG, dim = "x", full = TRUE),
                  getItems(monthRunoffMAG, dim = "y", full = TRUE),
                  sep = ".") != rs$coordinates)) {
      stop("Wrong cell ordering of basin in calcAvlWater.R")
    }

    # Transform to array (faster calculation)
    monthDischargeMAG <- as.array(collapseNames(monthDischargeMAG))
    monthRunoffMAG    <- as.array(collapseNames(monthRunoffMAG))

    ### Calculate available water per month (monthAvlWat)
    # Empty array
    monthAvlWat <- monthRunoffMAG
    monthAvlWat[, , ] <- NA

    # Sum the runoff in all basins and allocate it to the basin cells with discharge as weight
    for (basin in unique(basinCode)) {
      basinCells     <- which(basinCode == basin)
      basinRunoff    <- colSums(monthRunoffMAG[basinCells, , , drop = FALSE])
      basinDischarge <- colSums(monthDischargeMAG[basinCells, , , drop = FALSE])
      for (month in dimnames(monthAvlWat)[[3]]) {
        monthAvlWat[basinCells, , month] <- t(basinRunoff[, month] *
                                                t(monthDischargeMAG[basinCells, , month]) / basinDischarge[, month])
      }
    }
    # Remove no longer needed objects
    rm(basinDischarge, basinRunoff)

    # monthAvlWat contain NA's wherever basinDischarge was 0 -> Replace NA's by 0
    monthAvlWat[is.nan(monthAvlWat)] <- 0
    monthAvlWat <- as.magpie(monthAvlWat, spatial = 1)

    if (stage == "smoothed") {
      monthAvlWat <- toolSmooth(monthAvlWat)
    }

    #######################
    ##### Aggregation #####
    #######################
    ### Available water per cell per month
    if (seasonality == "monthly") {
      # Check for NAs
      if (any(is.na(monthAvlWat))) {
        stop("produced NA water availability")
      }
      out <- monthAvlWat

      ### Total water available per cell per year
    } else if (seasonality == "total") {
      # Sum up over all month:
      totalAvlWat <- dimSums(monthAvlWat, dim = 3)
      # Check for NAs
      if (any(is.na(totalAvlWat))) {
        stop("produced NA water availability")
      }
      out <- totalAvlWat

      ### Water available in growing period per cell per year
    } else if (seasonality == "grper") {
      # magpie object with days per month with same dimension as monthAvlWat
      tmp <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      monthDAYS       <- new.magpie(names = dimnames(monthAvlWat)[[3]])
      monthDAYS[, , ] <- tmp
      monthDayMAG     <- as.magpie(monthAvlWat)
      monthDayMAG[, , ] <- 1
      monthDayMAG       <- monthDayMAG * monthDAYS

      # Daily water availability
      dailyAvlWat <- monthAvlWat / monthDayMAG

      # Growing days per month
      growDAYS <- calcOutput("GrowingPeriod", yield_ratio = 0.1,
                             lpjml = cfg$readinVersion, climatetype = climatetype,
                             stage = stage, aggregate = FALSE)
      getItems(growDAYS, dim = 3) <- c(1:12)
      getSets(growDAYS) <- c("x", "y", "iso", "year", "month")

      # Adjust years
      yearsWAT <- getYears(dailyAvlWat)
      yearsGRPER <- getYears(growDAYS)
      if (length(yearsWAT) >= length(yearsGRPER)) {
        years <- yearsGRPER
      } else {
        years <- yearsWAT
      }
      rm(yearsGRPER, yearsWAT)

      # Available water in growing period per month
      grperAvlWat <- dailyAvlWat[, years, ] * growDAYS[, years, ]
      # Available water in growing period per year
      grperAvlWat <- dimSums(grperAvlWat, dim = 3)

      # Check for NAs
      if (any(is.na(grperAvlWat))) {
        stop("produced NA water availability")
      }
      out <- grperAvlWat
    } else {
      stop("Please specify seasonality: monthly, total or grper")
    }

  } else if (stage == "harmonized") {
    # load smoothed data for historical baseline
    baseline <- calcOutput("AvlWater", stage = "smoothed",
                           lpjml = cfg$readinVersion, climatetype = cfg$baselineHist,
                           seasonality = seasonality, aggregate = FALSE)

    if (climatetype == cfg$baselineHist) {
      # no further harmonization required when climatetype is historical baseline
      out <- baseline
    } else {
      # load smoothed future scenario
      x   <- calcOutput("AvlWater", stage = "smoothed",
                        lpjml = cfg$readinVersion, climatetype = cfg$climatetype,
                        seasonality = seasonality, aggregate = FALSE)
      # Harmonize future scenario to baseline
      out <- toolHarmonize2Baseline(x = x, base = baseline, ref_year = cfg$refYearHist)
    }

  } else if (stage == "harmonized2020") {
    # load harmonized baseline GCM scenario
    baseline2020 <- calcOutput("AvlWater", stage = "harmonized",
                               lpjml = cfg$readinVersion, climatetype = cfg$baselineGcm,
                               seasonality = seasonality, aggregate = FALSE)

    if (climatetype == cfg$baselineGcm) {
      # no further harmonization required if climatetype is baseline GCM
      out <- baseline2020
    } else {
      # load smoothed future scenario
      x   <- calcOutput("AvlWater", stage = "smoothed",
                        lpjml = cfg$readinVersion, climatetype = cfg$climatetype,
                        seasonality = seasonality,
                        aggregate = FALSE)
      # harmonize future scenario to baseline
      out <- toolHarmonize2Baseline(x, baseline2020, ref_year = cfg$refYearGcm)
    }

  } else {
    stop("Stage argument not supported!")
  }

  description <- paste0("Available water in ", seasonality)

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
