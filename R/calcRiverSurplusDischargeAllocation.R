#' @title       calcRiverSurplusDischargeAllocation
#' @description This function distributes surplus basin discharge after the previous river routings following certain management assumptions
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
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param allocationshare Share of water to be allocated to cell (only needs to be selected in case of allocationrule=="equality")
#' @param thresholdtype   Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear          Initialization year of irrigation system
#' @param protect_scen     Land protection scenario
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverSurplusDischargeAllocation", aggregate = FALSE) }
#'

calcRiverSurplusDischargeAllocation <- function(selectyears="all", humanuse="non_agriculture", subtype="discharge",
                                  version="LPJmL4", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015",
                                  allocationrule="optimization", allocationshare=NULL, thresholdtype=TRUE, gainthreshold=10,
                                  irrigationsystem="initialization", iniyear=1995, protect_scen) {

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River Structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))

  # Minimum flow requirements determined by previous river routing: Environmental Flow Requirements + Reserved for Non-Agricultural Uses + Reserved Committed Agricultural Uses (in mio. m^3 / yr)
  required_wat_min_allocation <- collapseNames(calcOutput("RiverHumanUses", humanuse="committed_agriculture", aggregate=FALSE, selectyears=selectyears, version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)[,,"required_wat_min"])
  required_wat_min_allocation <- as.array(collapseNames(required_wat_min_allocation))[,,1]

  # Discharge determined by previous river routings (in mio. m^3 / yr)
  discharge        <- calcOutput("RiverDischargeNatAndHuman", aggregate=FALSE, selectyears=selectyears, version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
  discharge        <- as.array(collapseNames(discharge))[,,1]

  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig    <- calcOutput("FullIrrigationRequirement", version="LPJmL5", selectyears=selectyears, climatetype=climatetype, harmonize_baseline=harmonize_baseline, time=time, dof=dof, iniyear=iniyear, iniarea=TRUE, irrigationsystem=irrigationsystem, protect_scen=protect_scen, aggregate=FALSE)[,,c("maiz","rapeseed","puls_pro")]
  required_wat_fullirrig_ww <- pmax(collapseNames(required_wat_fullirrig[,,"withdrawal"]), 0)
  required_wat_fullirrig_wc <- pmax(collapseNames(required_wat_fullirrig[,,"consumption"]), 0)

  # average required water for full irrigation across selected proxy crops
  required_wat_fullirrig_ww <- dimSums(required_wat_fullirrig_ww, dim=3) / length(getNames(required_wat_fullirrig_ww))
  required_wat_fullirrig_wc <- dimSums(required_wat_fullirrig_wc, dim=3) / length(getNames(required_wat_fullirrig_wc))

  # transform to array for further calculations
  required_wat_fullirrig_ww <- as.array(collapseNames(required_wat_fullirrig_ww))[,,1]
  required_wat_fullirrig_wc <- as.array(collapseNames(required_wat_fullirrig_wc))[,,1]

  # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
  meancellrank <- calcOutput("IrrigCellranking", version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=FALSE, ref_year="y2015",
                             cellrankyear=selectyears, cells="lpjcell", method="meancroprank", proxycrop=c("maiz", "rapeseed", "puls_pro"), iniyear=iniyear, aggregate=FALSE)
  meancellrank <- as.array(collapseNames(meancellrank))[,,1]

  # Yield gain potential through irrigation of proxy crops
  irrig_yieldgainpotential <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, averaging_range=averaging_range, dof=dof,
                                         cells="lpjcell", proxycrop=c("maiz", "rapeseed", "puls_pro"), monetary=thresholdtype, aggregate=FALSE)
  irrig_yieldgainpotential <- as.array(irrig_yieldgainpotential)[,,1]

  ################################################
  ####### River basin discharge allocation #######
  ################################################
  tmp <- NULL

  for (y in getYears(meancellrank)) {

    frac_fullirrig <- array(data=0,dim=67420)

    # Allocate water for full irrigation to cell with highest yield improvement through irrigation
    if (allocationrule=="optimization") {

      for (c in (1:max(meancellrank[,y],na.rm=T))) {
        # available water for additional irrigation withdrawals
        avl_wat_ww <- max(discharge[c,y] - required_wat_min_allocation[c,y], 0)

        # withdrawal constraint
        if (required_wat_fullirrig_ww[c,y]>0) {
          # how much withdrawals can be fulfilled by available water
          frac_fullirrig[c] <- min(avl_wat_ww / required_wat_fullirrig_ww[c,y], 1)

          # consumption constraint
          if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
            # available water for additional irrigation consumption (considering downstream availability)
            avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]],y] - required_wat_min_allocation[rs$downstreamcells[[c]],y]), 0)
            # how much consumption can be fulfilled by available water
            frac_fullirrig[c]   <- min(avl_wat_wc / required_wat_fullirrig_wc[c,y], frac_fullirrig[c])
          }

          # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
          discharge[c(rs$downstreamcells[[c]],c),y] <- discharge[c(rs$downstreamcells[[c]],c),y] - required_wat_fullirrig_wc[c,y] * frac_fullirrig[c]
          # update minimum water required in cell:
          required_wat_min_allocation[c,y]          <- required_wat_min_allocation[c,y] + frac_fullirrig[c] * required_wat_fullirrig_ww[c,y]
        }
      }

    } else if (allocationrule=="upstreamfirst") {
      # Allocate full irrigation requirements to most upstream cell first (calcorder)

      for (o in 1:max(rs$calcorder)) {
        cells <- which(rs$calcorder==o)

        for (c in cells){

          # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
          if (irrig_yieldgainpotential[c,y] > gainthreshold) {
            # available water for additional irrigation withdrawals
            avl_wat_ww <- max(discharge[c,y] - required_wat_min_allocation[c,y], 0)

            # withdrawal constraint
            if (required_wat_fullirrig_ww[c,y]>0) {
              # how much withdrawals can be fulfilled by available water
              frac_fullirrig[c] <- min(avl_wat_ww / required_wat_fullirrig_ww[c,y], 1)

              # consumption constraint
              if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
                # available water for additional irrigation consumption (considering downstream availability)
                avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]],y] - required_wat_min_allocation[rs$downstreamcells[[c]],]), 0)
                # how much consumption can be fulfilled by available water
                frac_fullirrig[c]   <- min(avl_wat_wc / required_wat_fullirrig_wc[c,y], frac_fullirrig[c])
              }

              # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
              discharge[c(rs$downstreamcells[[c]],c),y] <- discharge[c(rs$downstreamcells[[c]],c),y] - required_wat_fullirrig_wc[c,y] * frac_fullirrig[c]
              # update minimum water required in cell:
              required_wat_min_allocation[c,y]          <- required_wat_min_allocation[c,y] + frac_fullirrig[c] * required_wat_fullirrig_ww[c,y]
            }
          }
        }
      }
    } else {
      stop("Please choose allocation rule for river basin discharge allocation algorithm")
    }

    discharge <- as.magpie(discharge, spatial=1)

    out <- setNames(setYears(as.magpie(discharge,spatial=1),y), "discharge")
    out <- mbind(tmp, discharge)
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="Cellular discharge after accounting for known human uses along the river",
    isocountries=FALSE))
}
