#' @title       calcRiverSurplusDischargeAllocation_new
#' @description This function distributes surplus basin discharge after the previous river routings following certain management assumptions
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param output output to be reported
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype   Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear          Initialization year of irrigation system
#' @param protect_scen     land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection
#' @param proxycrop        historical crop mix pattern ("historical") or list of proxycrop(s)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames as.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverSurplusDischargeAllocation_new", aggregate = FALSE) }
#'

calcRiverSurplusDischargeAllocation_new <- function(selectyears, output, climatetype, allocationrule, thresholdtype, gainthreshold, irrigationsystem, iniyear, protect_scen, proxycrop) {

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River Structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))
  # numeric cell numbers in order of rs object
  rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

  # Minimum flow requirements determined by previous river routing: Environmental Flow Requirements + Reserved for Non-Agricultural Uses + Reserved Committed Agricultural Uses (in mio. m^3 / yr)
  required_wat_min_allocation <- collapseNames(calcOutput("RiverHumanUses", humanuse="committed_agriculture", climatetype=climatetype, selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)[,,"required_wat_min"])

  # Discharge determined by previous river routings (in mio. m^3 / yr)
  discharge                   <- calcOutput("RiverDischargeNatAndHuman", selectyears=selectyears, iniyear=iniyear, climatetype=climatetype, aggregate=FALSE)

  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig      <- calcOutput("FullIrrigationRequirement", selectyears=selectyears, climatetype=climatetype, iniyear=iniyear, iniareayear=iniyear, irrigationsystem=irrigationsystem, protect_scen=protect_scen, proxycrop=proxycrop, aggregate=FALSE)
  required_wat_fullirrig_ww   <- pmax(collapseNames(required_wat_fullirrig[,,"withdrawal"]), 0)
  required_wat_fullirrig_wc   <- pmax(collapseNames(required_wat_fullirrig[,,"consumption"]), 0)

  # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
  meancellrank                <- calcOutput("IrrigCellranking", climatetype=climatetype, cellrankyear=selectyears, method="meanpricedcroprank", proxycrop=proxycrop, iniyear=iniyear, aggregate=FALSE)

  # Yield gain potential through irrigation of proxy crops
  irrig_yieldgainpotential    <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears, proxycrop=proxycrop, monetary=thresholdtype, iniyear=iniyear, aggregate=FALSE)

  ### Transform Objects ###
  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(discharge), years = getYears(discharge), names = getNames(discharge), fill=0, sets=c("x.y.iso", "year", "EFP.scen"))
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
  meancellrank                <- as.array(meancellrank)[,,1]

  ################################################
  ####### River basin discharge allocation #######
  ################################################
  out      <- NULL

  for (y in getYears(meancellrank)) {

    # Allocate water for full irrigation to cell with highest yield improvement through irrigation
    if (allocationrule=="optimization") {

      for (o in (1:max(meancellrank[,y], na.rm=T))) {

        # Cells that have (next) highest rank
        c  <- rs$cells[meancellrank[,y]==o]

        # vector of downstreamcells of c
        down <- unlist(rs$downstreamcells[[c]])
        # vector of c in length of downstreamcells of c
        lc   <- rep(c, length(rs$downstreamcells[[c]]))
        # vector of 1 in length of downstreamcells of c
        cc   <- rep(1:length(c), length(rs$downstreamcells[[c]]))

        # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
        irriggain <- (irrig_yieldgainpotential[c,y,,drop=F] > gainthreshold)

        # available water for additional irrigation withdrawals
        avl_wat_ww[c,y,][irriggain[,,,drop=F]] <- pmax(IO_discharge[c,y,,drop=F] - required_wat_min_allocation[c,y,,drop=F], 0)[irriggain[,,,drop=F]]

        # withdrawal constraint
        ww_constraint   <- (required_wat_fullirrig_ww[c,y,,drop=F]>0 & irriggain[,,,drop=F])

        # how much withdrawals can be fulfilled by available water
        frac_fullirrig[c,y,][ww_constraint[,,,drop=F]] <- pmin(avl_wat_ww[c,y,,drop=F][ww_constraint[,,,drop=F]] / required_wat_fullirrig_ww[c,y,,drop=F][ww_constraint[,,,drop=F]], 1)

        if (length(down)>0) {
          # consumption constraint
          wc_constraint <- (required_wat_fullirrig_wc[c,y,,drop=F]>0 & ww_constraint[,,,drop=F])

          # available water for additional irrigation consumption (considering downstream availability)
          avl_wat_wc[c,y,][wc_constraint[,,,drop=F]]     <- pmax(apply((IO_discharge[down,y,,drop=F] - required_wat_min_allocation[down,y,,drop=F]), 3, min)[wc_constraint[,,,drop=F]], 0)
          # how much consumption can be fulfilled by available water
          frac_fullirrig[c,y,][wc_constraint[,,,drop=F]] <- pmin(avl_wat_wc[c,y,,drop=F][wc_constraint[,,,drop=F]] / required_wat_fullirrig_wc[c,y,,drop=F][wc_constraint[,,,drop=F]], frac_fullirrig[c,y,,drop=F][wc_constraint[,,,drop=F]])
        }

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        IO_discharge[c(down,c),y,][ww_constraint[c(cc,1),,,drop=F]] <- (IO_discharge[c(down,c),y,,drop=F] - required_wat_fullirrig_wc[c(cc,c),y,,drop=F] * frac_fullirrig[c(cc,c),y,,drop=F])[ww_constraint[c(cc,1),,,drop=F]]
        # update minimum water required in cell:
        required_wat_min_allocation[c,y,][ww_constraint[,,,drop=F]] <- (required_wat_min_allocation[c,y,,drop=F] + frac_fullirrig[c,y,,drop=F] * required_wat_fullirrig_ww[c,y,,drop=F])[ww_constraint[,,,drop=F]]
      }

    } else if (allocationrule=="upstreamfirst") {
      # Allocate full irrigation requirements to most upstream cell first (calcorder)

      for (o in 1:max(rs$calcorder)) {
        cells <- which(rs$calcorder==o)

        for (c in cells){

          # vector of downstreamcells of c
          down <- unlist(rs$downstreamcells[[c]])
          # vector of c in length of downstreamcells of c
          lc   <- rep(c, length(rs$downstreamcells[[c]]))
          # vector of 1 in length of downstreamcells of c
          cc   <- rep(1:length(c), length(rs$downstreamcells[[c]]))

          # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
          irriggain <- (irrig_yieldgainpotential[c,y,,drop=F] > gainthreshold)

          # available water for additional irrigation withdrawals
          avl_wat_ww[c,y,][irriggain[,,,drop=F]] <- pmax(IO_discharge[c,y,,drop=F] - required_wat_min_allocation[c,y,,drop=F], 0)[irriggain[,,,drop=F]]

          # withdrawal constraint
          ww_constraint <- (required_wat_fullirrig_ww[c,y,,drop=F]>0 & irriggain[,,,drop=F])

          # how much withdrawals can be fulfilled by available water
          frac_fullirrig[c,y,][ww_constraint[,,,drop=F]] <- pmin(avl_wat_ww[c,y,,drop=F][ww_constraint[,,,drop=F]] / required_wat_fullirrig_ww[c,y,,drop=F][ww_constraint[,,,drop=F]], 1)

          if (length(down)>0) {
            # consumption constraint
            wc_constraint <- (required_wat_fullirrig_wc[c,y,,drop=F]>0 & ww_constraint[,,,drop=F])

            # available water for additional irrigation consumption (considering downstream availability)
            avl_wat_wc[c,y,][wc_constraint[,,,drop=F]]     <- pmax(apply((IO_discharge[down,y,,drop=F] - required_wat_min_allocation[down,y,,drop=F]), 3, min)[wc_constraint[,,,drop=F]], 0)
            # how much consumption can be fulfilled by available water
            frac_fullirrig[c,y,][wc_constraint[,,,drop=F]] <- pmin(avl_wat_wc[c,y,,drop=F][wc_constraint[,,,drop=F]] / required_wat_fullirrig_wc[c,y,,drop=F][wc_constraint[,,,drop=F]], frac_fullirrig[c,y,,drop=F][wc_constraint[,,,drop=F]])
          }

          # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
          IO_discharge[c(down,c),y,][ww_constraint[c(cc,1),,,drop=F]] <- (IO_discharge[c(down,c),y,,drop=F] - required_wat_fullirrig_wc[c(cc,c),y,,drop=F] * frac_fullirrig[c(cc,c),y,,drop=F])[ww_constraint[c(cc,1),,,drop=F]]
          # update minimum water required in cell:
          required_wat_min_allocation[c,y,][ww_constraint[,,,drop=F]] <- (required_wat_min_allocation[c,y,,drop=F] + frac_fullirrig[c,y,,drop=F] * required_wat_fullirrig_ww[c,y,,drop=F])[ww_constraint[,,,drop=F]]
        }
      }
    } else {
      stop("Please choose allocation rule for river basin discharge allocation algorithm")
    }
  }

  if (output=="discharge") {
    # Main output for MAgPIE: water available for agricultural consumption
    out         <- as.magpie(IO_discharge, spatial=1)
    description <- "Cellular discharge after accounting for known human uses along the river"
  } else if (output=="frac_fullirrig") {
    # Main output for MAgPIE: water available for agricultural withdrawal
    out         <- as.magpie(frac_fullirrig, spatial=1)
    description <- "Fraction of full irrigation requirements that can be fulfilled"
  } else {
    stop("specify outputtype")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
