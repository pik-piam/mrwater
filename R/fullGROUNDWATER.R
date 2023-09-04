#' @title fullGROUNDWATER
#' @description Function that produces output for analysis of
#'              non-renewable groundwater resources
#'
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                          "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                          separated by ":"
#'                          (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          USD_ha (USD per hectare) for relative area return, or
#'                          USD_m3 (USD per cubic meter) for relative volumetric return;
#'                          USD for absolute return (total profit);
#'                          Price aggregation:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices
#'                          and boolean indicating fullpotential (TRUE, i.e. cell
#'                          receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                          receives at later stage in allocation algorithm);
#'                          separated by ":"
#'
#' @author Felicitas Beier
#'
#' @importFrom madrat calcOutput
#' @importFrom stringr str_split
#' @export

fullGROUNDWATER <- function(multicropping = FALSE, rankmethod = "USD_ha:GLO:TRUE") {

  # Standard settings
  iniyear           <- "y2010"
  selectyears       <- "y2010"

  # Newest LPJmL runs
  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_bft_e511ac58")
  climatetype       <- "MRI-ESM2-0:ssp370"

  ##########################
  ### Groundwater Volume ###
  ##########################
  # Goundwater use in current agriculture
  calcOutput("NonrenGroundwatUse",
              output = "comAg", multicropping = multicropping,
              lpjml = lpjml, climatetype = climatetype,
              selectyears = selectyears, iniyear = iniyear,
              aggregate = FALSE,
              file = "gwAg.mz")
  # Goundwater use in current agriculture
  calcOutput("NonrenGroundwatUse",
             output = "nonAg", multicropping = multicropping,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             aggregate = FALSE,
             file = "gwNonAg.mz")
  # Goundwater use in current agriculture
  calcOutput("NonrenGroundwatUse",
             output = "total", multicropping = multicropping,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             aggregate = FALSE,
             file = "gwTotal.mz")

}
