#' @title       calcYieldsAdjusted
#' @description This function returns irrigated and rainfed yields for magpie crops.
#'              It can either return LPJmL potential yields of different stages or
#'              LPJmL yields calibrated to FAO country yields
#'
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical" for yields
#' @param selectyears   Years to be returned by the function
#' @param iniyear       Year to be used for cropland of yield calibration
#' @param yieldcalib    Calibrated (LPJmL yield potentials smoothed and harmonized
#'                      to baseline and calibrated with global FAO calibration factor
#'                      for proxycrops where LPJmL crops mapped multiple times to MAgPIE crops) or
#'                      FAO (LPJmL yields calibrated with current FAO yield)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("YieldsAdjusted", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput toolAggregate
#' @importFrom magclass collapseNames getNames getCells getSets dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcYieldsAdjusted <- function(lpjml, climatetype, iniyear, selectyears, yieldcalib) {

  # read in cellular lpjml yields [in tons/ha]
  yields <- setYears(calcOutput("Yields", source = c(lpjml = lpjml[["crop"]], isimip = NULL),
                                cells = "lpjcell", climatetype = climatetype, years = selectyears, aggregate = FALSE),
                     selectyears)

  # only crops (pasture is not irrigated)
  yields     <- yields[, , "pasture", invert = T]
  # set correct dimension names
  getSets(yields)[c("d3.1", "d3.2")] <- c("MAG", "irrigation")
  #* #*#*# @KRISTINE: Glaubst du, es kann potenziell ein Problem sein, dass yields MAG.irrigation ist und croparea irrigation.MAG oder sind die MAgPIE-Objekte dafür ausreichend fool-proof? #*#*#*ä
  #* #*#*# @KRISTINE: Ist das mit dem getSets() ok so, oder hätte ich da potenziell das Problem, dass sich die Funktion ändert und das mit d3.1 d3.2 nicht mehr zusammen passt bzw. sich die Anzahl der Dimenstionen in calcYields ändert o.Ä.?

  croplist    <- getNames(collapseNames(yields[, , "irrigated"]))
  description <- "LPJmL yields for all different (MAgPIE) crop types"
  unit        <- "tDM per ha"

  if (yieldcalib == "FAO") {

    # read in total (irrigated + rainfed) croparea
    croparea <- setYears(calcOutput("CropareaAdjusted", years = iniyear,
                                    sectoral = "kcr", physical = TRUE,
                                    cells = "lpjcell", cellular = TRUE,
                                    irrigation = TRUE, aggregate = FALSE),
                         NULL)
    totalCroparea <- dimSums(croparea, dim = "irrigation")

    # LPJmL production on currently irrigated and rainfed area in initialization year
    LPJmLproduction <- setYears(dimSums(croparea[, , croplist] * yields[, iniyear, croplist],
                                         dim = "irrigation"),
                                 NULL)

    # LPJmL iso-country yields
    LPJmLproduction <- dimSums(LPJmLproduction, dim = c(1.1, 1.2))
    totalCroparea   <- dimSums(totalCroparea, dim = c(1.1, 1.2))

    LPJmLyields     <- ifelse(totalCroparea[, , croplist] > 0, LPJmLproduction[, , croplist] / totalCroparea[, , croplist], 0)
    LPJmLyields     <- toolCountryFill(LPJmLyields, fill = 0) # Note: "ABW" "AND" "ATA" "BES" "BLM" "BVT" "GIB" "LIE" "MAC" "MAF" "MCO" "SMR" "SXM" "VAT" "VGB" missing in LPJmL cells

    # FAO iso-country yields
    FAOyields <- setYears(calcOutput("FAOYield", physical = TRUE, attributes = "dm",
                                      irrigation = FALSE, cellular = FALSE, cut = 0.99,
                                      aggregate = FALSE)[, iniyear, ], NULL)
    # Note:
    # yield calculated by production / area -> when physical area selected
    # (rather than harvested area) cropping intensity (multicropping and fallow land)
    # is implicitly included

    # Calibration Factor:
    Calib      <- FAOyields[getCells(LPJmLyields), , croplist] / LPJmLyields[, , croplist]
    Calib[LPJmLyields[, , croplist] == 0] <- 0
    Calib[Calib[, , croplist] > 1.5]       <- 1.5
    names(dimnames(Calib))[1]              <- "iso"

    yields      <- yields[, , croplist] * Calib[intersect(unique(gsub(".*\\.", "",
                                                                      getCells(yields))),
                                                          getCells(Calib)), , croplist]
    description <- "LPJmL yields calibrated to FAO yield levels for all different (MAgPIE) crop types"
    unit        <- "tDM per ha"

  } else if (yieldcalib == "FAO_Kristine") {
    ### NOTE: This is only for testing until it will be fully integrated instead of the FAO calibration above
    yields <- calcOutput("CalibratedYields", source = c(lpjml = lpjml[["crop"]], isimip = NULL),
                         climatetype = climatetype, refYear = "y1995")

    description <- "LPJmL yields calibrated to FAO yield levels for all different (MAgPIE) crop types"
    unit        <- "tDM per ha"
  }

  # Check for NAs
  if (any(is.na(yields))) {
    stop("Function calcYieldsAdjusted produced NAs")
  }

  return(list(x            = yields,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
