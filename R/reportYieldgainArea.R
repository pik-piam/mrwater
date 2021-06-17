#' @title       reportYieldgainArea
#' @description reports potentially irrigated area depending on gainthreshold
#'              and land constraint only
#'
#' @param region        Regional resolution (can be country iso-code, "GLO" for global, or
#'                      region name and respective mapping "EUR:H12")
#' @param GT_range      Range of gainthreshold
#' @param scenario      non-agricultural water use scenario to be displayed in plot
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param selectyears   Years for which irrigatable area is calculated
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod     EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield) or
#'                      calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                      smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                      smoothed_calib (smoothed LPJmL yield potentials, not harmonized, but calibrated as in calcYields)
#' @param thresholdtype Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param avlland_scen  Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                      combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                      protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param cropmix       Cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' reportYieldgainArea(GT_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom stringr str_split
#'
#' @export

reportYieldgainArea <- function(region = "GLO", GT_range, scenario, lpjml, selectyears, climatetype, EFRmethod, yieldcalib, thresholdtype, avlland_scen, cropmix, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  x <- calcOutput("IrrigatableAreaUnlimited", selectyears = selectyears, avlland_scen = avlland_scen, lpjml = lpjml, climatetype = climatetype, cropmix = cropmix, yieldcalib = yieldcalib, thresholdtype = thresholdtype, gainthreshold = 0, multicropping = multicropping, aggregate = FALSE)
  d <- "Irrigatable Area only considering land constraint"
  u <- "Irrigatable Area (Mha)"

  if (region == "GLO") {
    x <- as.data.frame(dimSums(x, dim = 1))

  } else {
    map    <- str_split(region, ":")[[1]][2]
    region <- str_split(region, ":")[[1]][1]

    # aggregate to iso-countries
    mapping        <- toolGetMappingCoord2Country()
    mapping$coords <- paste(mapping$coords, mapping$iso, sep = ".")
    x <- toolAggregate(x, rel = mapping, from = "coords", to = "iso", dim = 1)
    x <- toolCountryFill(x, fill = 0) # Note: "ABW" "AND" "ATA" "BES" "BLM" "BVT" "GIB" "LIE" "MAC" "MAF" "MCO" "SMR" "SXM" "VAT" "VGB" missing in LPJmL cells

    # aggregate to regions
    if (!is.na(map) && map == "H12") {
      regmap        <- toolGetMapping("regionmappingH12.csv")
      names(regmap) <- c("Country", "iso", "reg")
      x             <- toolAggregate(x, rel = regmap, from = "iso", to = "reg", dim = 1)
    } else if (!is.na(map) && map != "H12") {
      stop("Selected regionmapping is not yet available. Please select region and respective mapping via region argument: e.g. EUR:H12")
    }

    x <- as.data.frame(dimSums(x[region, , ], dim = 1))
  }

  df <- data.frame(EFP = x$Data1, Scen = x$Data2, GT0 = x$Value, stringsAsFactors = FALSE)

  if (GT_range[1] == 0) {
    GT_range <- GT_range[-1]
  }

  for (gainthreshold in GT_range) {

    x <- calcOutput("IrrigatableAreaUnlimited", selectyears = selectyears, avlland_scen = avlland_scen, lpjml = lpjml, climatetype = climatetype, cropmix = cropmix, yieldcalib = yieldcalib, thresholdtype = thresholdtype, gainthreshold = gainthreshold, multicropping = multicropping, aggregate = FALSE)

    if (region == "GLO") {
      x <- as.data.frame(dimSums(x, dim = 1))

    } else {
      # aggregate to iso-countries
      x <- toolAggregate(x, rel = mapping, from = "coords", to = "iso", dim = 1)
      x <- toolCountryFill(x, fill = 0) # Note: "ABW" "AND" "ATA" "BES" "BLM" "BVT" "GIB" "LIE" "MAC" "MAF" "MCO" "SMR" "SXM" "VAT" "VGB" missing in LPJmL cells

      # aggregate to regions
      if (!is.na(map) && map == "H12") {
        x             <- toolAggregate(x, rel = regmap, from = "iso", to = "reg", dim = 1)
      } else if (!is.na(map) && map != "H12") {
        stop("Selected regionmapping is not yet available. Please select region and respective mapping via region argument: e.g. EUR:H12")
      }

      x <- as.data.frame(dimSums(x[region, , ], dim = 1))
    }

    tmp              <- data.frame(EFP = x$Data1, Scen = x$Data2, Value = x$Value)
    names(tmp)[3]    <- paste0("GT", gainthreshold)
    df               <- merge(df, tmp)
  }

  df        <- data.frame(t(data.frame(Scen = paste("IrrigArea", df$EFP, df$Scen, sep = "."), df[-c(1, 2)])), stringsAsFactors = FALSE)
  names(df) <- as.character(unlist(df[1, ]))
  df        <- df[-1, ]
  df        <- data.frame(GT = as.numeric(gsub("GT", "", rownames(df))), df, stringsAsFactors = FALSE)
  df        <- as.data.frame(lapply(df, as.numeric))

  return(list(data = df, description = d, unit = u))
}
