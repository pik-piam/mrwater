#' @title       calcRiverDischargeNatAndHuman_new
#' @description This function calculates cellular discharge after considering
#'              known human consumption (non-agricultural and committed agricultural)
#'              along the river calculated and accounted for in previous river
#'              routings (see calcRiverNaturalFlows and calcRiverHumanUses)
#'
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param selectyears   Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear       Initialization year of irrigation system
#' @param efrMethod     EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param comAg         if TRUE: the currently already irrigated areas in
#'                               initialization year are reserved for irrigation,
#'                      if FALSE: no irrigation areas reserved (irrigation potential)
#' @param transDist     Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                      separated by ":"
#'                      (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverDischargeNatAndHuman", aggregate = FALSE)
#' }
#'
calcRiverDischargeNatAndHuman_new <- function(lpjml, climatetype,
                                          selectyears, iniyear, transDist,
                                          efrMethod, comAg, multicropping) {

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds",
                            package = "mrwater"))

  # Non-agricultural human consumption in current cell or provided to neighboring cells
  # that can be fulfilled by available water determined in previous river routings
  nAgWC <- dimSums(calcOutput("RiverHumanUses_new",
                              humanuse = "non_agriculture", aggregate = FALSE,
                              lpjml = lpjml, climatetype = climatetype,
                              efrMethod = efrMethod, multicropping = multicropping,
                              iniyear = iniyear, transDist = transDist,
                              selectyears = selectyears)[, , c("currHuman_wc_local",
                                                                "resNeighbor_wc")], dim = "data")

  if (comAg) {

    # Committed agricultural human consumption in current cell or provided to neighboring cells
    # that can be fulfilled by available water determined in previous river routings
    cAgWC <- dimSums(calcOutput("RiverHumanUses_new",
                                humanuse = "committed_agriculture", aggregate = FALSE,
                                lpjml = lpjml, climatetype = climatetype,
                                efrMethod = efrMethod, multicropping = multicropping,
                                iniyear = iniyear, transDist = transDist,
                                selectyears = selectyears)[, , c("currHuman_wc_local",
                                                                  "resNeighbor_wc")], dim = "data")
  } else {

    # No committed agricultural human consumption considered
    cAgWC <- new.magpie(cells_and_regions = getCells(nAgWC),
                        years = getYears(nAgWC),
                        names = getNames(nAgWC),
                        fill = 0)
  }

  # Yearly Runoff (on land and water)
  yearlyRunoff <- collapseNames(calcOutput("YearlyRunoff", selectyears = selectyears,
                                           lpjml = lpjml, climatetype = climatetype,
                                           aggregate = FALSE))
  # Lake evaporation as calculated by natural flow river routing
  naturalFlows <- calcOutput("RiverNaturalFlows", selectyears = selectyears,
                              lpjml = lpjml, climatetype = climatetype,
                              aggregate = FALSE)
  lakeEvap     <- collapseNames(naturalFlows[, , "lake_evap_nat"])

  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(yearlyRunoff),
                          years = getYears(yearlyRunoff),
                          names = getNames(nAgWC),
                          fill = 0,
                          sets = c("x.y.iso", "year", "data"))
    # bring object x to dimension of object0
    out     <- x + object0
    return(out)
  }

  #######################################
  ###### Transform object size   ########
  #######################################
  lakeEvap     <- as.array(.transformObject(lakeEvap))
  cAgWC        <- as.array(.transformObject(cAgWC))
  nAgWC        <- as.array(.transformObject(nAgWC))
  yearlyRunoff <- as.array(.transformObject(yearlyRunoff))

  # helper variables for river routing
  inflow       <- as.array(.transformObject(0))
  avlWat       <- as.array(.transformObject(0))

  # output variable that will be filled during river routing
  discharge    <- as.array(.transformObject(0))

  ###########################################
  ###### River Discharge Calculation ########
  ###########################################

  for (o in 1:max(rs$calcorder)) {

    # Note: the calcorder ensures that the upstreamcells are calculated first
    cells <- which(rs$calcorder == o)

    for (c in cells) {

      # available water
      avlWat[c, , ] <- inflow[c, , , drop = FALSE] +
                        yearlyRunoff[c, , , drop = FALSE] -
                          lakeEvap[c, , , drop = FALSE]

      # discharge
      discharge[c, , ] <- avlWat[c, , , drop = FALSE] -
                            nAgWC[c, , , drop = FALSE] -
                              cAgWC[c, , , drop = FALSE]

      # inflow into nextcell
      if (rs$nextcell[c] > 0) {
        inflow[rs$nextcell[c], , ] <- inflow[rs$nextcell[c], , , drop = FALSE] +
                                        discharge[c, , , drop = FALSE]
      }
    }
  }

  out <- as.magpie(discharge, spatial = 1)

  #############
  ### Tests ###
  #############
  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA discharge")
  }
  if (any(round(out, digits = 4) < 0)) {
    stop("calcRiverHumanUses produced negative values")
  }

  # Test whether any water is lost
  natDischarge       <- out
  natDischarge[, , ] <- collapseNames(naturalFlows[, , "discharge_nat"])

  basinDischarge       <- out
  basinDischarge[, , ] <- 0
  basinDischarge[unique(rs$endcell), , ] <- out[unique(rs$endcell), , ]
  totalWat <- dimSums(basinDischarge, dim = 1) +
                dimSums(as.magpie(cAgWC, spatial = 1), dim = 1) +
                  dimSums(as.magpie(nAgWC, spatial = 1), dim = 1)

  # Total water (summed basin discharge + consumed)
  # must be identical across scenarios
  if (!all(abs(totalWat - mean(totalWat)) < 1e-06)) {
    stop("Scenarios differ. That should not be the case.
        Total water volume should always be the same")
  }
  # Total water (summed basin discharge + consumed)
  # must be same as natural summed basin discharge
  if (any(round(dimSums(natDischarge[unique(rs$endcell), , ],
                    dim = 1) - totalWat[, , 1], digits = 6) != 0)) {
    stop("Water has been lost during the Neighbor Water Provision Algorithm")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "Cellular discharge after accounting for
                              environmental flow requirements and
                              known human uses along the river",
              isocountries = FALSE))
}
