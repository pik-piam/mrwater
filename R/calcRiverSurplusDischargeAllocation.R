#' @title calcRiverSurplusDischargeAllocation
#' @description This function distributes surplus basin discharge following certain management assumptions
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param subtype     Subtype to be returned: discharge or required_wat_min or frac_fulfilled
#' @param humanuse    Human use type to which river routing shall be applied (non_agriculture or committed_agriculture). Note: non_agriculture must be run prior to committed_agriculture
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#' @import mrmagpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("calcRiverSurplusDischargeAllocation", aggregate = FALSE) }
#'
#' \seealso{\code{\link{calcRiverNaturalFlows}},\code{\link{calcRiverHumanUses}},\code{\link{calcDischargeNatAndHuman}}}
#'

calcRiverSurplusDischargeAllocation <- function(selectyears="all", humanuse="non_agriculture", subtype="discharge",
                                  version="LPJmL4", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015") {
  # # # # # # # # # # #
  # # # READ IN DATA # #
  # # # # # # # # # # #
  ### Read in river structure
  # Note: river structure derived from LPJmL input (drainage) [maybe later: implement readDrainage function]
  # Information contained:
  ## upstreamcells:   all cells that are upstream of current cell (list of cells)
  ## downstreamcells: all cells that are downstream of current cell (list of cells)
  ## nextcell:        cell to which discharge of current cell flows (exactly 1 cell)
  ## endcell:         estuary cell of current cell, i.e. last cell of the river of which current cell is part of (exactly 1 cell)
  ## calcorder:       ordering of cells for calculation from upstream to downstream
  ## cells:           LPJmL cell ordering with ISO code
  ## coordinates:     coordinate data of cells
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))

  ### Read in required data
  # Minimum flow requirements determined by previous river routing: Environmental Flow Requirements + Reserved for Non-Agricultural Uses + Reserved Committed Agricultural Uses (in mio. m^3 / yr)
  required_wat_min <- calcOutput("RiverHumanUses", selectyears=selectyears, humanuse="committed_agriculture", subtype="required_wat_min", aggregate=FALSE,
                                    version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)

  # Discharge determined by previous river routing (in mio. m^3 / yr)
  discharge <- calcOutput("RiverDischargeNatAndHuman", selectyears=selectyears, humanuse=humanuse, subtype="discharge",
                                            version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year) {

  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig_ww <- calcOutput("FullIrrigationRequirement", version="LPJmL5", selectyears=selectyears, climatetype="HadGEM2_ES:rcp2p6:co2", harmonize_baseline=FALSE, time="spline", dof=4, iniyear=1995, iniarea=TRUE, irrig_requirement="withdrawal", cells="lpjcell", aggregate=FALSE)[,,c("maiz","rapeseed","puls_pro")]
  required_wat_fullirrig_ww <- pmax(required_wat_fullirrig_ww,0)
  required_wat_fullirrig_wc <- calcOutput("FullIrrigationRequirement", version="LPJmL5", selectyears=selectyears, climatetype="HadGEM2_ES:rcp2p6:co2", harmonize_baseline=FALSE, time="spline", dof=4, iniyear=1995, iniarea=TRUE, irrig_requirement="consumption", cells="lpjcell", aggregate=FALSE)[,,c("maiz","rapeseed","puls_pro")]
  required_wat_fullirrig_wc <- pmax(required_wat_fullirrig_wc,0)

  # Full irrigation water requirement depending on irrigation system in use
  if (irrigationsystem=="initialization") {
    # read in irrigation system area initialization [share of AEI by system]
    tmp               <- calcOutput("IrrigationSystem", source=irrigini, aggregate=FALSE)
    irrigation_system <- new.magpie(getCells(tmp), getYears(required_wat_fullirrig_ww), getNames(tmp))
    getYears(irrigation_system) <- getYears(required_wat_fullirrig_ww)
    for (y in getYears(required_wat_fullirrig_ww)) {
      irrigation_system[,y,] <- tmp
    }
    # full irrigation water requirements (in mio. m^3)
    required_wat_fullirrig_ww   <- dimSums(irrigation_system*required_wat_fullirrig_ww,dim=3.1)
    required_wat_fullirrig_wc   <- dimSums(irrigation_system*required_wat_fullirrig_wc,dim=3.1)
  } else {
    # whole area irrigated by one system as selected in argument "irrigationsystem"
    required_wat_fullirrig_ww <- collapseNames(required_wat_fullirrig_ww[,,irrigationsystem])
    required_wat_fullirrig_wc <- collapseNames(required_wat_fullirrig_wc[,,irrigationsystem])
  }
  # average required water for full irrigation across selected proxy crops
  required_wat_fullirrig_ww <- dimSums(required_wat_fullirrig_ww,dim=3)/length(getNames(required_wat_fullirrig_ww))
  required_wat_fullirrig_wc <- dimSums(required_wat_fullirrig_wc,dim=3)/length(getNames(required_wat_fullirrig_wc))

  # transform to array for further calculations
  required_wat_fullirrig_ww <- as.array(collapseNames(required_wat_fullirrig_ww))[,,1]
  required_wat_fullirrig_wc <- as.array(collapseNames(required_wat_fullirrig_wc))[,,1]

  # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
  meancellrank <- calcOutput("IrrigCellranking", version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=FALSE, ref_year="y2015",
                             cellrankyear=selectyears, cells="lpjcell", crops="magpie", method="meancroprank", proxycrop=c("maiz", "rapeseed", "puls_pro"), iniyear=iniyear, aggregate=FALSE)
  meancellrank <- as.array(meancellrank)[,,1]

  # Yield gain potential through irrigation of proxy crops
  irrig_yieldgainpotential <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, averaging_range=averaging_range, dof=dof,
                                         cells="lpjcell", crops="magpie", proxycrop=c("maiz", "rapeseed", "puls_pro"), monetary=thresholdtype, aggregate=FALSE)
  irrig_yieldgainpotential <- as.array(irrig_yieldgainpotential)[,,1]



  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(yearly_runoff), years = getYears(yearly_runoff), names = c(paste("on",getNames(NAg_ww),sep="."), paste("off",getNames(NAg_ww),sep=".")), fill=0)
    # bring object x to dimension of object0
    out     <- x + object0
    return(out)
  }



  ################################################
  ####### River basin discharge allocation #######
  ################################################
  # Minimum water required
  required_wat_min_allocation <- required_wat_min

  ### REPORTING: discharge before algorithm is executed
  discharge_befalgo <- discharge

  # Allocate water for full irrigation to cell with highest yield improvement through irrigation
  if (allocationrule=="optimization") {

    for (c in (1:max(meancellrank[,y],na.rm=T))){
      # available water for additional irrigation withdrawals
      avl_wat_ww <- max(discharge[c]-required_wat_min_allocation[c],0)

      # withdrawal constraint
      if (required_wat_fullirrig_ww[c,y]>0) {
        # how much withdrawals can be fulfilled by available water
        frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)

        # consumption constraint
        if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
          # available water for additional irrigation consumption (considering downstream availability)
          avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]]] - required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
          # how much consumption can be fulfilled by available water
          frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
        }

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        discharge[c(rs$downstreamcells[[c]],c)] <- discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
        # update minimum water required in cell:
        required_wat_min_allocation[c] <- required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
      }
    }
    ### REPORTING: discharge after optimization
    discharge_optimization <- discharge

  } else if (allocationrule=="upstreamfirst") {
    # Allocate full irrigation requirements to most upstream cell first (calcorder)

    for (o in 1:max(rs$calcorder)){
      cells <- which(rs$calcorder==o)

      for (c in cells){

        # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
        if (irrig_yieldgainpotential[c,y] > gainthreshold) {
          # available water for additional irrigation withdrawals
          avl_wat_ww <- max(discharge[c]-required_wat_min_allocation[c],0)

          # withdrawal constraint
          if (required_wat_fullirrig_ww[c,y]>0) {
            # how much withdrawals can be fulfilled by available water
            frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)

            # consumption constraint
            if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
              # available water for additional irrigation consumption (considering downstream availability)
              avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]]] - required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
              # how much consumption can be fulfilled by available water
              frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
            }

            # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
            discharge[c(rs$downstreamcells[[c]],c)] <- discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
            # update minimum water required in cell:
            required_wat_min_allocation[c] <- required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
          }
        }
      }
    }
    ### REPORTING: discharge after optimization
    discharge_upstreamfirst <- discharge

  } else if (allocationrule=="equality") {

    ## while loop?
    ## 1 run with meancellrank; identify list of cells that still can get water (avl wat ww > 0, downstreamcell consumption); run through these again by order of meancellrank

    for (c in (1:max(meancellrank[,y],na.rm=T))){
      # available water for additional irrigation withdrawals
      avl_wat_ww <- max(discharge[c]-required_wat_min_allocation[c],0)

      # withdrawal constraint
      if (required_wat_fullirrig_ww[c,y]>0) {
        # how much withdrawals can be fulfilled by available water
        frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)

        # consumption constraint
        if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
          # available water for additional irrigation consumption (considering downstream availability)
          avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]]] - required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
          # how much consumption can be fulfilled by available water
          frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
        }

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        discharge[c(rs$downstreamcells[[c]],c)] <- discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
        # update minimum water required in cell:
        required_wat_min_allocation[c] <- required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]

      }
    }

    # Repeat optimization algorithm several times
    # Instead of full irrigation, only up to x% (e.g.20%) are allocated to most efficient cell
    # Repeat until all river basin discharge is allocated ---> STILL MISSING

    for (c in (1:max(meancellrank[,y],na.rm=T))){
      # available water for additional irrigation withdrawals
      avl_wat_ww <- max(discharge[c]-required_wat_min_allocation[c],0)

      # withdrawal constraint
      if (required_wat_fullirrig_ww[c,y]>0) {
        # how much withdrawals can be fulfilled by available water
        frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)

        # consumption constraint
        if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
          # available water for additional irrigation consumption (considering downstream availability)
          avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]]] - required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
          # how much consumption can be fulfilled by available water
          frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
        }

        frac_fullirrig[c] <- frac_fullirrig[c]*allocationshare

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        discharge[c(rs$downstreamcells[[c]],c)] <- discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
        # update minimum water required in cell:
        required_wat_min_allocation[c] <- required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
      }
    }
  } else {
    stop("Please choose allocation rule for river basin discharge allocation algorithm")
  }

  out <- as.magpie(O_discharge, spatial=1)

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="Cellular discharge after accounting for known human uses along the river",
    isocountries=FALSE))
}
