#' @title       calcRiverNaturalFlows
#' @description This function calculates natural discharge for the river routing
#'              derived from inputs from LPJmL
#'
#' @param selectyears Years to be returned
#'                    (Note: does not affect years of harmonization or smoothing)
#' @param lpjml       LPJmL version used
#' @param climatetype Switch between different climate scenarios
#'                    or historical baseline "GSWP3-W5E5:historical"
#'
#' @importFrom magclass collapseNames new.magpie getCells mbind setYears
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverNaturalFlows", aggregate = FALSE)
#' }
#'
calcRiverNaturalFlows <- function(selectyears, lpjml, climatetype) {

  ### Read in river structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds",
                            package = "mrwater"))

  ### Read in input data already time-smoothed and for climate scenarios harmonized to the baseline
  if (grepl("historical", climatetype)) {
    # Baseline is only smoothed (not harmonized)
    stage <- "smoothed"
  } else {
    # Climate scenarios are harmonized to baseline
    stage <- "harmonized2020"
  }

  # Yearly lake evapotranspiration (in mio. m^3 per year) [smoothed & harmonized]
  lakeEvap <- as.array(setYears(calcOutput("LPJmL_new", subtype = "lake_evap",
                                            version = lpjml[["natveg"]], climatetype = climatetype,
                                            stage = stage, years = selectyears,
                                            aggregate = FALSE),
                                     selectyears))

  # Runoff (on land and water)
  runoff   <- as.array(collapseNames(calcOutput("YearlyRunoff", selectyears = selectyears,
                                                 lpjml = lpjml, climatetype = climatetype,
                                                 aggregate = FALSE)))

  ############################################
  ###### River Routing: Natural Flows ########
  ############################################

  ## Empty arrays that will be filled by the following natural river routing algorithm
  natDischarge       <- runoff
  natDischarge[, , ] <- 0
  natInflow          <- natDischarge
  lakeEvapNEW        <- natDischarge

  ### River Routing 1: Natural flows ###
  # Determine natural discharge
  for (o in 1:max(rs$calcorder)) {

    # Note: the calcorder ensures that upstreamcells are calculated first
    cells <- which(rs$calcorder == o)

    for (c in cells) {
      ### Natural water balance
      # lake evap that can be fulfilled
      # (if water available: lake evaporation considered; if not: lake evap is reduced respectively):
      lakeEvapNEW[c, , ] <- pmin(lakeEvap[c, , , drop = FALSE],
                                 natInflow[c, , , drop = FALSE] + runoff[c, , , drop = FALSE])
      # natural discharge
      natDischarge[c, , ] <- natInflow[c, , , drop = FALSE] +
                              runoff[c, , , drop = FALSE] -
                              lakeEvapNEW[c, , , drop = FALSE]
      # inflow into nextcell
      if (rs$nextcell[c] > 0) {
        natInflow[rs$nextcell[c], , ] <- natInflow[rs$nextcell[c], , , drop = F] + natDischarge[c, , , drop = F]
      }
    }
  }

  out <- new.magpie(cells_and_regions = getItems(natDischarge, dim = 1),
                    years = getItems(natDischarge, dim = "year"),
                    names = c("discharge_nat", "lake_evap_nat", "inflow_nat"),
                    sets  = c("x.y.iso", "year", "data"))
  out[, , "discharge_nat"] <- as.magpie(natDischarge, spatial = 1, temporal = 2)
  out[, , "lake_evap_nat"] <- as.magpie(lakeEvapNEW, spatial = 1, temporal = 2)
  out[, , "inflow_nat"]    <- as.magpie(natInflow, spatial = 1, temporal = 2)

return(list(x            = out,
            weight       = NULL,
            unit         = "mio. m^3",
            description  = "Cellular natural discharge and lake evaporation
                            in natural river condition",
            isocountries = FALSE))
}
