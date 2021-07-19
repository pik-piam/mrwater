#' @title       calcIrrigYieldImprovementPotential
#' @description This function calculates the yield improvement potential
#'              through irrigation for different crops
#'
#' @param lpjml         LPJmL version used for yields
#' @param climatetype   Climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned by the function
#' @param monetary      Yield improvement potential in tDM (FALSE, default) or
#'                      priced yield improvement potential in USD05 (TRUE)
#' @param iniyear       initialization year for food price and cropmix area
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#'                      NULL returns all crops individually
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield) or
#'                      calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                      smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                      smoothed_calib (not harmonized, but calibrated)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigYieldImprovementPotential", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells getSets dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcIrrigYieldImprovementPotential <- function(lpjml, climatetype, monetary,
                                               iniyear, selectyears, cropmix, yieldcalib, multicropping) {

  # yield reduction parameters (share of yield potential that can be reached in second / third season):
  yld_shr_season2 <- 0.8
  yld_shr_season3 <- 0.7

  # read in cellular lpjml yields [in tons/ha]
  yields   <- calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
                         iniyear = iniyear, selectyears = selectyears,
                         yieldcalib = yieldcalib, aggregate = FALSE)

  # magpie crops
  croplist <- getNames(collapseNames(yields[, , "irrigated"]))

  if (monetary) {

    # Read in crop output price in initialization (USD05/tDM)
    p        <- calcOutput("IniFoodPrice", datasource = "FAO", products = "kcr",
                              years = NULL, year = iniyear, aggregate = FALSE)
    croplist <- intersect(croplist, getNames(p))

    # Calculate monetary yield gain (in USD05/ha)
    yields   <- yields[, , croplist] * p[, , croplist]
    unit     <- "USD05 per ha"

  } else {

    unit     <- "tons per ha"

  }

  # Selected crops
  if (!is.null(cropmix)) {

    # share of corp area by crop type
    if (length(cropmix) == 1 && grepl("hist", cropmix)) {

      # read in relevant croparea: total (irrigated + rainfed) or irrigated depending on chosen cropmix
      croparea <- setYears(calcOutput("Croparea", years = iniyear, sectoral = "kcr",
                                      cellular = TRUE, cells = "lpjcell", physical = TRUE,
                                      irrigation = TRUE, aggregate = FALSE),
                           NULL)
      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
      map                          <- toolGetMappingCoord2Country()
      getCells(croparea)           <- paste(map$coords, map$iso, sep = ".")
      names(dimnames(croparea))[1] <- "x.y.iso"
      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

      if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "irrig") {

        # irrigated croparea
        croparea <- collapseNames(croparea[, , "irrigated"])

      } else if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "total") {

        # total croparea (irrigated + rainfed)
        croparea <- dimSums(croparea, dim = "irrigation")

      } else {
        stop("Please select hist_irrig or hist_total when selecting historical cropmix")
      }

      # historical share of crop types in cropland per cell
      croparea_shr <- croparea / dimSums(croparea, dim = 3)
      # correct NAs: where no current cropland available,
      # representative crops (maize, rapeseed, pulses) assumed as proxy
      rep_crops    <- c("maiz", "rapeseed", "puls_pro")
      other_crops  <- setdiff(getNames(croparea), rep_crops)
      croparea_shr[, , rep_crops][dimSums(croparea, dim = 3) == 0]   <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
      croparea_shr[, , other_crops][dimSums(croparea, dim = 3) == 0] <- 0

      # average (rf/irr) yields over historical crops weighted with their croparea share
      croplist <- intersect(croplist, getNames(croparea_shr))
      yields   <- dimSums(yields[, , croplist] * croparea_shr[, , croplist], dim = "MAG")

      description <- "Average yield improvement potential for crop types weighted with historical croparea share"

    } else {

      # Note: equal crop area share for each proxycrop assumed
      # select proxy crops
      yields      <- yields[, , cropmix]
      # average over proxy crops
      yields      <- dimSums(yields, dim = "MAG") / length(cropmix)
      description <- "Average yield improvement potential for selection of crop types"

    }


    # Account for multicropping potential
    mc   <- calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)

    i1r1 <- new.magpie(cells_and_regions = getCells(yields),
                       years = getYears(yields),
                       fill = 0)
    i3r1 <- i3r2 <- i3r3 <- i2r1 <- i2r2 <- i2r3 <- i1r2 <- i1r3 <- i1r1
    i1r1[mc[, , "irrigated"] == 1 & mc[, , "rainfed"] == 1] <- 1
    i1r2[mc[, , "irrigated"] == 1 & mc[, , "rainfed"] == 2] <- 1
    i1r3[mc[, , "irrigated"] == 1 & mc[, , "rainfed"] == 3] <- 1
    i2r1[mc[, , "irrigated"] == 2 & mc[, , "rainfed"] == 1] <- 1
    i2r2[mc[, , "irrigated"] == 2 & mc[, , "rainfed"] == 2] <- 1
    i2r3[mc[, , "irrigated"] == 2 & mc[, , "rainfed"] == 3] <- 1
    i3r1[mc[, , "irrigated"] == 3 & mc[, , "rainfed"] == 1] <- 1
    i3r2[mc[, , "irrigated"] == 3 & mc[, , "rainfed"] == 2] <- 1
    i3r3[mc[, , "irrigated"] == 3 & mc[, , "rainfed"] == 3] <- 1

    # additional seasonal gains
    yield_gain_add <- new.magpie(cells_and_regions = getCells(yields),
                                 years = getYears(yields),
                                 names = c("ss", "sd", "st", "ds", "dd", "dt", "ts", "td", "tt"),
                                 fill = 0)

    yield_gain_add[, , "ss"] <- collapseNames(yields[, , "irrigated"]) - collapseNames(yields[, , "rainfed"])
    yield_gain_add[, , "ds"] <- yld_shr_season2 * collapseNames(yields[, , "irrigated"])
    yield_gain_add[, , "dd"] <- yld_shr_season2 * (collapseNames(yields[, , "irrigated"]) -
                                                     collapseNames(yields[, , "rainfed"]))
    yield_gain_add[, , "ts"] <- yld_shr_season3 * collapseNames(yields[, , "irrigated"])
    yield_gain_add[, , "td"] <- yld_shr_season3 * collapseNames(yields[, , "irrigated"])
    yield_gain_add[, , "tt"] <- yld_shr_season3 * (collapseNames(yields[, , "irrigated"]) -
                                                     collapseNames(yields[, , "rainfed"]))

    if (multicropping) {

      # report seasonal gains for three seasons
      yield_gain <- new.magpie(cells_and_regions = getCells(yields),
                               years = getYears(yields),
                               names = c("single", "double", "triple"),
                               fill = 0)

      yield_gain[, , "single"] <- collapseNames(yield_gain_add[, , "ss"])
      yield_gain[, , "single"] <- yield_gain[, , "single"] * (1 - i1r2) * (1 - i1r3)

      yield_gain[, , "double"] <- collapseNames(yield_gain_add[, , "ds"]) * i2r1 +
        collapseNames(yield_gain_add[, , "dd"]) * i2r2 +
        collapseNames(yield_gain_add[, , "dt"]) * i2r3

      yield_gain[, , "triple"] <- collapseNames(yield_gain_add[, , "ts"]) * i3r1 +
        collapseNames(yield_gain_add[, , "td"]) * i3r2 +
        collapseNames(yield_gain_add[, , "tt"]) * i3r3

    } else {

      # yield gain through irrigation for each crop [in tons/ha]
      yield_gain <- collapseNames(yield_gain_add[, , "ss"])
      # (Note: irrigation may lead to shift in growing period -> can have negative values;
      # also: under N-stress, irrigation may lead to lower yields,
      # the latter is only relevant for limited-N-LPJmL version, default: unlimited N)

    }

  } else {

    if (!multicropping) {

      yield_gain  <- collapseNames(yields[, , "irrigated"]) - collapseNames(yields[, , "rainfed"])
      description <- "Yield improvement potential by irrigation for all different crop types"

    } else {
      stop("Yield gain potential for different crop types not implemented for
           case of multicropping")
    }
  }

  # set negative yield gains to 0
  yield_gain[yield_gain < 0] <- 0

  # Check for NAs
  if (any(is.na(yield_gain))) {
    stop("Function YieldImprovementPotential produced NAs")
  }

  return(list(x            = yield_gain,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
