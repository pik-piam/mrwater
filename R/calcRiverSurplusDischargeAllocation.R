#' @title       calcRiverSurplusDischargeAllocation
#' @description This function distributes surplus basin discharge after
#'              previous river routings following certain management assumptions
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years to be returned
#'                          (Note: does not affect years of harmonization or smoothing)
#' @param output            Output to be reported
#' @param climatetype       Switch between different climate scenarios
#'                          or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod         EFR method used including selected strictness of EFRs
#'                          (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return;
#'                          and boolean indicating fullpotential (TRUE, i.e. cell receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst")
#' @param thresholdtype     Unit of yield improvement potential used as threshold:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return
#' @param gainthreshold     Threshold of yield improvement potential required for
#'                          water allocation in upstreamfirst algorithm
#'                          (in same unit as thresholdtype)
#' @param irrigationsystem  Irrigation system to be used for river basin discharge
#'                          allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear           Initialization year of irrigation system
#' @param avlland_scen      Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                          combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                          protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param cropmix           cropmix for which irrigation yield improvement is calculated
#'                          can be selection of proxycrop(s) for calculation of average yield gain
#'                          or hist_irrig or hist_total for historical cropmix
#' @param yieldcalib        FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param com_ag            if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential), if only_discharge: already irrigated areas are reserved in Human Water Use Accounting, but Algorithm can decide freely to use it or not
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames as.magpie getCells setCells mbind setYears
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverSurplusDischargeAllocation", aggregate = FALSE)
#' }
#'
calcRiverSurplusDischargeAllocation <- function(lpjml, climatetype, selectyears,
                                                output, EFRmethod, accessibilityrule, rankmethod,
                                                allocationrule, thresholdtype, gainthreshold,
                                                irrigationsystem, iniyear, avlland_scen, cropmix,
                                                yieldcalib, com_ag, multicropping) {

  # Check
  if (!is.na(as.list(strsplit(avlland_scen, split = ":"))[[1]][2]) &&
      iniyear != as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])) {
    stop("Initialization year in calcRiverSurplusDischargeAllocation does not match:
         iniyear and avlland_scen should have same initialization year")
  }

  # Retrieve arguments
  if (com_ag == TRUE) {
    comagyear <- iniyear
  } else if (com_ag == FALSE) {
    comagyear <- NULL
  }

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River Structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))
  # numeric cell numbers in order of rs object
  rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

  # Minimum reserved flow requirements: Inaccessible discharge +
  #                                     [Environmental Flow Requirements (adjusted for part that is fulfilled by inaccessible water), if activated] +
  #                                     Reserved for Non-Agricultural Uses +
  #                                     [Reserved Committed Agricultural Uses, if activated] (in mio. m^3 / yr)
  required_wat_min_allocation <- calcOutput("RiverWatReserved", aggregate = FALSE,
                                            selectyears = selectyears, iniyear = iniyear,
                                            lpjml = lpjml, climatetype = climatetype, com_ag = com_ag,
                                            EFRmethod = EFRmethod, accessibilityrule = accessibilityrule)

  # Discharge determined by previous river routings (in mio. m^3 / yr)
  discharge                   <- calcOutput("RiverDischargeNatAndHuman", selectyears = selectyears,
                                            lpjml = lpjml, climatetype = climatetype, iniyear = iniyear,
                                            EFRmethod = EFRmethod, com_ag = com_ag, aggregate = FALSE)

  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig      <- calcOutput("FullIrrigationRequirement", selectyears = selectyears,
                                            lpjml = lpjml, climatetype = climatetype, comagyear = comagyear,
                                            irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                                            cropmix = cropmix, multicropping = multicropping, aggregate = FALSE)
  required_wat_fullirrig_ww   <- pmax(collapseNames(required_wat_fullirrig[, , "withdrawal"]), 0)
  required_wat_fullirrig_wc   <- pmax(collapseNames(required_wat_fullirrig[, , "consumption"]), 0)

  # Yield gain potential through irrigation of proxy crops
  irrig_yieldgainpotential    <- calcOutput("IrrigYieldImprovementPotential", selectyears = selectyears,
                                            lpjml = lpjml, climatetype = climatetype, iniyear = iniyear,
                                            cropmix = cropmix, unit = thresholdtype, yieldcalib = yieldcalib,
                                            multicropping = multicropping, aggregate = FALSE)

  if (allocationrule == "optimization") {

    # Retrieve arguments
    fullpotential <- as.logical(strsplit(rankmethod, ":")[[1]][2])

    # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
    glocellrank   <- setYears(calcOutput("IrrigCellranking", cellrankyear = selectyears,
                                         lpjml = lpjml, climatetype = climatetype, unit = rankmethod,
                                         cropmix = cropmix, iniyear = iniyear, yieldcalib = yieldcalib,
                                         multicropping = multicropping, aggregate = FALSE),
                              selectyears)

    # Share of full irrigation water requirements to be allocated for each round of the allocation algorithm
    if (multicropping) {

      # allocation share needs to be adjusted for multicropping length
      tmp <- calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)
      tmp <- range(tmp)[2]

      allocationshare           <- 1 / (length(glocellrank[, 1, 1]) / tmp / 67420)

    } else {

      # allocation share depends on chosen cellranking
      allocationshare           <- 1 / (length(glocellrank[, 1, 1]) / 67420)

    }
    required_wat_fullirrig_ww <- required_wat_fullirrig_ww * allocationshare
    required_wat_fullirrig_wc <- required_wat_fullirrig_wc * allocationshare

    # transform to array
    glocellrank               <- as.array(glocellrank)
  }

  ### Transform Objects ###
  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(discharge),
                          years = getYears(discharge),
                          names = getNames(discharge), fill = 0,
                          sets = c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out     <- object0 + x
    return(out)
  }

  IO_discharge                <- as.array(discharge)
  required_wat_min_allocation <- as.array(required_wat_min_allocation)
  required_wat_fullirrig_ww   <- as.array(.transformObject(required_wat_fullirrig_ww))
  required_wat_fullirrig_wc   <- as.array(.transformObject(required_wat_fullirrig_wc))
  irrig_yieldgainpotential    <- as.array(.transformObject(irrig_yieldgainpotential))
  frac_fullirrig              <- as.array(.transformObject(0))
  avl_wat_ww                  <- as.array(.transformObject(0))
  avl_wat_wc                  <- as.array(.transformObject(0))

  ################################################
  ####### River basin discharge allocation #######
  ################################################
  out <- NULL
  if (class(selectyears) == "numeric") {
    selectyears <- paste0("y", selectyears)
  }

  for (y in selectyears) {

    # Allocate water for full irrigation to cell with highest yield improvement through irrigation
    if (allocationrule == "optimization") {

      for (o in (1:max(glocellrank[, y, ], na.rm = T))) {

        # Extract the cell number (depending on type of cellranking)
        if (!fullpotential) {
          c <- rs$cells[rs$coordinates == paste(strsplit(gsub(".*_", "", names(which(glocellrank[, y, ] == o))), "\\.")[[1]][1], strsplit(gsub(".*_", "", names(which(glocellrank[, y, ] == o))), "\\.")[[1]][2], sep = ".")]
        } else {
          c <- rs$cells[glocellrank[, y, ] == o]
        }

        # vector of downstreamcells of c
        down <- unlist(rs$downstreamcells[[c]])
        # vector of c in length of downstreamcells of c
        lc   <- rep(c, length(rs$downstreamcells[[c]]))
        # vector of 1s in length of downstreamcells of c
        cc   <- rep(1:length(c), length(rs$downstreamcells[[c]]))

        # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
        irriggain <- (irrig_yieldgainpotential[c, y, , drop = F] > gainthreshold)

        # available water for additional irrigation withdrawals
        avl_wat_ww[c, y, ][irriggain[, , , drop = F]] <- pmax(IO_discharge[c, y, , drop = F] -
                                                                required_wat_min_allocation[c, y, , drop = F],
                                                              0)[irriggain[, , , drop = F]]

        # withdrawal constraint
        ww_constraint   <- (required_wat_fullirrig_ww[c, y, , drop = F] > 0 & irriggain[, , , drop = F])

        # how much withdrawals can be fulfilled by available water
        frac_fullirrig[c, y, ][ww_constraint[, , , drop = F]] <- pmin(avl_wat_ww[c, y, , drop = F][ww_constraint[, , , drop = F]] /
                                                                        required_wat_fullirrig_ww[c, y, , drop = F][ww_constraint[, , , drop = F]],
                                                                      1)

        if (length(down) > 0) {

          # consumption constraint
          wc_constraint <- (required_wat_fullirrig_wc[c, y, , drop = F] > 0 & ww_constraint[, , , drop = F])

          # available water for additional irrigation consumption (considering downstream availability)
          # (downstream availability is constrained by EFRs and inaccessible discharge just as local withdrawal constraint above)
          avl_wat_wc[c, y, ][wc_constraint[, , , drop = F]]     <- pmax(apply(IO_discharge[down, y, , drop = F] -
                                                                                required_wat_min_allocation[down, y, , drop = F], MARGIN = 3, min),
                                                                        0)[wc_constraint[, , , drop = F]]

          # how much consumption can be fulfilled by available water
          frac_fullirrig[c, y, ][wc_constraint[, , , drop = F]] <- pmin(avl_wat_wc[c, y, , drop = F][wc_constraint[, , , drop = F]] /
                                                                          required_wat_fullirrig_wc[c, y, , drop = F][wc_constraint[, , , drop = F]],
                                                                        frac_fullirrig[c, y, , drop = F][wc_constraint[, , , drop = F]])
        }

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        IO_discharge[c(down, c), y, ][ww_constraint[c(cc, 1), , , drop = F]] <- (IO_discharge[c(down, c), y, , drop = F] -
                                                                                   required_wat_fullirrig_wc[c(lc, c), y, , drop = F] * frac_fullirrig[c(lc, c), y, , drop = F])[ww_constraint[c(cc, 1), , , drop = F]]
        # update minimum water required and previously committed water withdrawal in cell:
        required_wat_min_allocation[c, y, ][ww_constraint[, , , drop = F]] <- (required_wat_min_allocation[c, y, , drop = F] +
                                                                                 frac_fullirrig[c, y, , drop = F] * required_wat_fullirrig_ww[c, y, , drop = F])[ww_constraint[, , , drop = F]]
      }

    } else if (allocationrule == "upstreamfirst") {

      # Allocate full irrigation requirements to most upstream cell first (calcorder)
      for (o in 1:max(rs$calcorder)) {
        cells <- which(rs$calcorder == o)

        for (c in cells) {

          # vector of downstreamcells of c
          down <- unlist(rs$downstreamcells[[c]])
          # vector of c in length of downstreamcells of c
          lc   <- rep(c, length(rs$downstreamcells[[c]]))
          # vector of 1 in length of downstreamcells of c
          cc   <- rep(1:length(c), length(rs$downstreamcells[[c]]))

          # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
          irriggain <- (irrig_yieldgainpotential[c, y, , drop = F] > gainthreshold)

          # available water for additional irrigation withdrawals
          avl_wat_ww[c, y, ][irriggain[, , , drop = F]] <- pmax(IO_discharge[c, y, , drop = F] -
                                                                  required_wat_min_allocation[c, y, , drop = F],
                                                                0)[irriggain[, , , drop = F]]

          # withdrawal constraint
          ww_constraint <- (required_wat_fullirrig_ww[c, y, , drop = F] > 0 & irriggain[, , , drop = F])

          # how much withdrawals can be fulfilled by available water
          frac_fullirrig[c, y, ][ww_constraint[, , , drop = F]] <- pmin(avl_wat_ww[c, y, , drop = F][ww_constraint[, , , drop = F]] /
                                                                          required_wat_fullirrig_ww[c, y, , drop = F][ww_constraint[, , , drop = F]],
                                                                        1)

          if (length(down) > 0) {

            # consumption constraint
            wc_constraint <- (required_wat_fullirrig_wc[c, y, , drop = F] > 0 & ww_constraint[, , , drop = F])

            # available water for additional irrigation consumption (considering downstream availability)
            # (downstream availability is constrained by EFRs and inaccessible discharge just as local withdrawal constraint above)
            avl_wat_wc[c, y, ][wc_constraint[, , , drop = F]]     <- pmax(apply(IO_discharge[down, y, , drop = F] -
                                                                                  required_wat_min_allocation[down, y, , drop = F], MARGIN = 3, min),
                                                                          0)[wc_constraint[, , , drop = F]]

            # how much consumption can be fulfilled by available water
            frac_fullirrig[c, y, ][wc_constraint[, , , drop = F]] <- pmin(avl_wat_wc[c, y, , drop = F][wc_constraint[, , , drop = F]] /
                                                                            required_wat_fullirrig_wc[c, y, , drop = F][wc_constraint[, , , drop = F]],
                                                                          frac_fullirrig[c, y, , drop = F][wc_constraint[, , , drop = F]])
          }

          # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
          IO_discharge[c(down, c), y, ][ww_constraint[c(cc, 1), , , drop = F]] <- (IO_discharge[c(down, c), y, , drop = F] -
                                                                                     required_wat_fullirrig_wc[c(lc, c), y, , drop = F] * frac_fullirrig[c(lc, c), y, , drop = F])[ww_constraint[c(cc, 1), , , drop = F]]
          # update minimum water required in cell:
          required_wat_min_allocation[c, y, ][ww_constraint[, , , drop = F]] <- (required_wat_min_allocation[c, y, , drop = F] +
                                                                                   frac_fullirrig[c, y, , drop = F] * required_wat_fullirrig_ww[c, y, , drop = F])[ww_constraint[, , , drop = F]]
        }
      }
    } else {
      stop("Please choose allocation rule for river basin discharge allocation algorithm")
    }
  }

  if (output == "discharge") {

    # Main output for MAgPIE: water available for agricultural consumption
    out         <- as.magpie(IO_discharge, spatial = 1)
    description <- "Cellular discharge after surplus discharge allocation algorithm"

  } else if (output == "frac_fullirrig") {

    # Main output for MAgPIE: water available for agricultural withdrawal
    out         <- as.magpie(frac_fullirrig, spatial = 1)
    description <- "Fraction of full irrigation requirements that can be fulfilled"

  } else {
    stop("specify outputtype")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
