#' @title       reportYieldgainArea
#' @description reports potentially irrigated area depending on gainthreshold
#'              and land constraint only
#'
#' @param region        Regional resolution (can be country iso-code, "GLO" for global, or
#'                      region name and respective mapping "EUR:H12")
#' @param GT_range      Range of gainthreshold for calculation of potentially
#'                      irrigated areas
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

reportYieldgainArea <- function(region = "GLO", GT_range, lpjml, selectyears,
                                climatetype, EFRmethod, yieldcalib, thresholdtype,
                                avlland_scen, cropmix, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  x <- calcOutput("IrrigatableAreaUnlimited", gainthreshold = 0,
                  selectyears = selectyears, lpjml = lpjml, climatetype = climatetype,
                  cropmix = cropmix, yieldcalib = yieldcalib,
                  thresholdtype = thresholdtype, multicropping = multicropping,
                  avlland_scen = avlland_scen, aggregate = FALSE)
  d <- "Potentially Irrigated Area only considering land constraint"
  u <- "Mha"

  # sum up over regional dimension
  x  <- as.data.frame(toolRegionSums(x = x, region = region))

  # create data frame
  df <- data.frame(GT0 = x$Value, stringsAsFactors = FALSE)

  if (GT_range[1] == 0) {
    GT_range <- GT_range[-1]
  }

  for (gainthreshold in GT_range) {

    x <- calcOutput("IrrigatableAreaUnlimited", gainthreshold = gainthreshold,
                    selectyears = selectyears, lpjml = lpjml, climatetype = climatetype,
                    cropmix = cropmix, yieldcalib = yieldcalib,
                    thresholdtype = thresholdtype, multicropping = multicropping,
                    avlland_scen = avlland_scen, aggregate = FALSE)

    # sum up over regional dimension
    x  <- as.data.frame(toolRegionSums(x = x, region = region))

    tmp              <- data.frame(GT0 = x$Value, stringsAsFactors = FALSE)
    names(tmp)[1]    <- paste0("GT", gainthreshold)
    df               <- merge(df, tmp)
  }

  df           <- data.frame(GT            = as.numeric(gsub("GT", "", names(df))),
                             YieldGainArea = as.data.frame(t(df)))
  names(df)[2] <- "YieldGainArea"

  return(list(
    data        = df,
    description = d,
    unit        = u))
}
