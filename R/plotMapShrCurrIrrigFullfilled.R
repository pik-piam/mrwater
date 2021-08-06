#' @title       plotMapShrCurrIrrigFullfilled
#' @description plot map of share of current irrigation that can be fulfilled
#'              given renewable water availability of the algorithm
#'
#' @param scenario         EFP and non-agricultural water use scenario
#'                         separated by "." (e.g. "on.ssp2")
#' @param lpjml            LPJmL version used
#' @param climatetype      Switch between different climate scenarios or
#'                         historical baseline "GSWP3-W5E5:historical"
#' @param selectyears      Years to be returned
#' @param iniyear          Initialization year
#' @param EFRmethod        EFR method used including selected strictness of EFRs
#'                         (e.g. Smakhtin:good, VMF:fair)
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotMapShrCurrIrrigFullfilled()
#' }
#'
#' @importFrom luplot plotmap2
#'
#' @export

plotMapShrCurrIrrigFullfilled <- function(scenario, iniyear, lpjml, selectyears, climatetype, EFRmethod) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Map depicting the share of current irrigation
         that can be fulfilled given renewable water availability of the algorithm")
  }

  fulfilledShr <- calcOutput("ShrCurrIrrigFullfilled",
                             lpjml = lpjml, climatetype = climatetype,
                             selectyears = selectyears, iniyear = iniyear,
                             EFRmethod = EFRmethod, aggregate = FALSE)

  out <- plotmap2(toolLPJcell2MAgPIEcell(fulfilledShr[, selectyears, scenario]),
                  title = element_blank(), labs = FALSE, sea = FALSE, land_colour = "transparent") +
         scale_fill_continuous("", limits = c(0, 1), low = "#FFFF66", high = "darkgreen", na.value = "grey") +
         theme(title = element_blank(),
               legend.position  = c(0.06, 0.3), legend.direction = "vertical",
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background  = element_rect(fill = "transparent", colour = NA),
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               strip.background = element_rect(fill = "transparent", colour = NA),
               strip.text       = element_text(color = "white"))

  return(out)
}
