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
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield) or
#'                      calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                      smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                      smoothed_calib (not harmonized, but calibrated)
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
  if (grepl("smoothed", yieldcalib)) {
    yields <- calcOutput("YieldsPotential", lpjml = lpjml, climatetype = climatetype,
                         selectyears = selectyears, proxycalib = grepl("calib", yieldcalib), aggregate = FALSE)
  } else {
    yields <- setYears(calcOutput("Yields", source = c(lpjml = lpjml[["crop"]], isimip = NULL),
                                  cells = "lpjcell", climatetype = climatetype, years = selectyears, aggregate = FALSE), selectyears)
  }

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
    croparea <- setYears(calcOutput("Croparea", years = iniyear, sectoral = "kcr",
                                    cells = "lpjcell", physical = TRUE, cellular = TRUE,
                                    irrigation = TRUE, aggregate = FALSE),
                         NULL)
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    map                          <- toolGetMappingCoord2Country()
    getCells(croparea)           <- paste(map$coords, map$iso, sep = ".")
    names(dimnames(croparea))[1] <- "x.y.iso"
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    croparea_total <- dimSums(croparea, dim = "irrigation")

    # LPJmL production on currently irrigated and rainfed area in initialization year
    LPJmL_production <- setYears(dimSums(croparea[, , croplist] * yields[, iniyear, croplist], dim = "irrigation"), NULL)

    # LPJmL iso-country yields
    mapping          <- toolGetMappingCoord2Country()
    mapping$coords   <- paste(mapping$coords, mapping$iso, sep = ".")
    LPJmL_production <- toolAggregate(LPJmL_production, rel = mapping, from = "coords", to = "iso", dim = 1)
    croparea_total   <- toolAggregate(croparea_total, rel = mapping, from = "coords", to = "iso", dim = 1)
    LPJmL_yields     <- ifelse(croparea_total[, , croplist] > 0, LPJmL_production[, , croplist] / croparea_total[, , croplist], 0)
    LPJmL_yields     <- toolCountryFill(LPJmL_yields, fill = 0) # Note: "ABW" "AND" "ATA" "BES" "BLM" "BVT" "GIB" "LIE" "MAC" "MAF" "MCO" "SMR" "SXM" "VAT" "VGB" missing in LPJmL cells

    # FAO iso-country yields
    FAO_yields <- setYears(calcOutput("FAOYield", physical = TRUE, attributes = "dm", irrigation = FALSE, cellular = FALSE, cut = 0.99, aggregate = FALSE)[, iniyear, ], NULL)

    # Calibration Factor:
    Calib      <- FAO_yields[getCells(LPJmL_yields), , croplist] / LPJmL_yields[, , croplist]
    Calib[LPJmL_yields[, , croplist] == 0] <- 0
    Calib[Calib[, , croplist] > 1.5]       <- 1.5
    names(dimnames(Calib))[1]              <- "iso"

    yields      <- yields[, , croplist] * Calib[intersect(unique(gsub(".*\\.", "",
                                                                      getCells(yields))),
                                                          getCells(Calib)), , croplist]
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
