#' @title       calcRiverSurplusDischargeAllocation
#' @description This function distributes surplus basin discharge after the previous river routings following certain management assumptions
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param output output to be reported
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
#' @param protect_scen     land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection
#' @param proxycrop        historical crop mix pattern ("historical") or list of proxycrop(s)
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

calcRiverSurplusDischargeAllocation <- function(selectyears="all", output,
                                  version="LPJmL4", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015",
                                  allocationrule="optimization", allocationshare=NULL, thresholdtype=TRUE, gainthreshold=10,
                                  irrigationsystem="initialization", iniyear=1995, protect_scen, proxycrop) {

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River Structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))

  # Minimum flow requirements determined by previous river routing: Environmental Flow Requirements + Reserved for Non-Agricultural Uses + Reserved Committed Agricultural Uses (in mio. m^3 / yr)
  required_wat_min_allocation <- collapseNames(calcOutput("RiverHumanUses", humanuse="committed_agriculture", aggregate=FALSE, selectyears=selectyears, version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)[,,"required_wat_min"])

  # Discharge determined by previous river routings (in mio. m^3 / yr)
  discharge                   <- calcOutput("RiverDischargeNatAndHuman", aggregate=FALSE, selectyears=selectyears, version=version, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)

  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig      <- calcOutput("FullIrrigationRequirement", version="LPJmL5", selectyears=selectyears, climatetype=climatetype, harmonize_baseline=harmonize_baseline, time=time, dof=dof, iniyear=iniyear, iniarea=TRUE, irrigationsystem=irrigationsystem, protect_scen=protect_scen, proxycrop=proxycrop, aggregate=FALSE)
  required_wat_fullirrig_ww   <- pmax(collapseNames(required_wat_fullirrig[,,"withdrawal"]), 0)
  required_wat_fullirrig_wc   <- pmax(collapseNames(required_wat_fullirrig[,,"consumption"]), 0)

  # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
  meancellrank                <- calcOutput("IrrigCellranking", version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=FALSE, ref_year="y2015",
                                           cellrankyear=selectyears, cells="lpjcell", method="meancroprank", proxycrop=c("maiz", "rapeseed", "puls_pro"), iniyear=iniyear, aggregate=FALSE)

  # Yield gain potential through irrigation of proxy crops
  irrig_yieldgainpotential    <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, averaging_range=averaging_range, dof=dof,
                                           cells="lpjcell", proxycrop=c("maiz", "rapeseed", "puls_pro"), monetary=thresholdtype, aggregate=FALSE)

  ### Transform Objects ###
  ## Transform object dimensions
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(discharge), years = getYears(discharge), names = getNames(discharge), fill=0)
    # bring object x to dimension of object0
    out     <- object0 + x
    return(out)
  }

  discharge                   <- as.array(discharge)
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
  tmp <- NULL

  for (y in getYears(meancellrank)) {

    # Allocate water for full irrigation to cell with highest yield improvement through irrigation
    if (allocationrule=="optimization") {

      for (o in (1:max(meancellrank[,y], na.rm=T))) {

        # Cells that have (next) highest rank
        rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))
        c        <- rs$cells[meancellrank[,y]==o]

        # vector of downstreamcells of c
        down <- unlist(rs$downstreamcells[[c]])
        # vector of c in length of downstreamcells of c
        lc   <- rep(c, length(rs$downstreamcells[[c]]))
        # vector of 1 in length of downstreamcells of c
        cc   <- rep(1:length(c), length(rs$downstreamcells[[c]]))

        # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
        irriggain <- (irrig_yieldgainpotential[c,y,,drop=F] > gainthreshold)

        # available water for additional irrigation withdrawals
        avl_wat_ww[c,y,][irriggain[,,,drop=F]] <- pmax(discharge[c,y,,drop=F] - required_wat_min_allocation[c,y,,drop=F], 0)[irriggain[,,,drop=F]]

        # withdrawal constraint
        ww_constraint <-  (required_wat_fullirrig_ww[c,y,,drop=F]>0 & irriggain[,,,drop=F])

        # how much withdrawals can be fulfilled by available water
        frac_fullirrig[c,y,][ww_constraint[,,,drop=F]] <- pmin(avl_wat_ww[c,y,,drop=F][ww_constraint[,,,drop=F]] / required_wat_fullirrig_ww[c,y,,drop=F][ww_constraint[,,,drop=F]], 1)

        if (length(down)>0) {
          # consumption constraint
          wc_constraint <- (required_wat_fullirrig_wc[c,y,,drop=F]>0 & ww_constraint[,,,drop=F])

          # available water for additional irrigation consumption (considering downstream availability)
          avl_wat_wc[c,y,][wc_constraint[,,,drop=F]]     <- pmax(apply((discharge[down,y,,drop=F] - required_wat_min_allocation[down,y,,drop=F]), 3, min)[wc_constraint[,,,drop=F]], 0)
          # how much consumption can be fulfilled by available water
          frac_fullirrig[c,y,][wc_constraint[,,,drop=F]] <- pmin(avl_wat_wc[c,y,,drop=F][wc_constraint[,,,drop=F]] / required_wat_fullirrig_wc[c,y,,drop=F][wc_constraint[,,,drop=F]], frac_fullirrig[c,y,,drop=F][wc_constraint[,,,drop=F]])
        }

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        discharge[c(down,c),y,][ww_constraint[c(cc,1),,,drop=F]]    <- (discharge[c(down,c),y,,drop=F] - required_wat_fullirrig_wc[c(cc,1),y,,drop=F] * frac_fullirrig[c(cc,1),y,,drop=F])[ww_constraint[c(cc,1),,,drop=F]]
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
          avl_wat_ww[c,y,][irriggain[,,,drop=F]] <- pmax(discharge[c,y,,drop=F] - required_wat_min_allocation[c,y,,drop=F], 0)[irriggain[,,,drop=F]]

          # withdrawal constraint
          ww_constraint <- (required_wat_fullirrig_ww[c,y,,drop=F]>0 & irriggain[,,,drop=F])

          # how much withdrawals can be fulfilled by available water
          frac_fullirrig[c,y,][ww_constraint[,,,drop=F]] <- pmin(avl_wat_ww[c,y,,drop=F][ww_constraint[,,,drop=F]] / required_wat_fullirrig_ww[c,y,,drop=F][ww_constraint[,,,drop=F]], 1)

          if (length(down)>0) {
            # consumption constraint
            wc_constraint <- (required_wat_fullirrig_wc[c,y,,drop=F]>0 & ww_constraint[,,,drop=F])

            # available water for additional irrigation consumption (considering downstream availability)
            avl_wat_wc[c,y,][wc_constraint[,,,drop=F]]     <- pmax(apply((discharge[down,y,,drop=F] - required_wat_min_allocation[down,y,,drop=F]), 3, min)[wc_constraint[,,,drop=F]], 0)
            # how much consumption can be fulfilled by available water
            frac_fullirrig[c,y,][wc_constraint[,,,drop=F]] <- pmin(avl_wat_wc[c,y,,drop=F][wc_constraint[,,,drop=F]] / required_wat_fullirrig_wc[c,y,,drop=F][wc_constraint[,,,drop=F]], frac_fullirrig[c,y,,drop=F][wc_constraint[,,,drop=F]])
          }

          # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
          discharge[c(down,c),y,][ww_constraint[c(cc,1),,,drop=F]]    <- (discharge[c(down,c),y,,drop=F] - required_wat_fullirrig_wc[c(cc,1),y,,drop=F] * frac_fullirrig[c(cc,1),y,,drop=F])[ww_constraint[c(cc,1),,,drop=F]]
          # update minimum water required in cell:
          required_wat_min_allocation[c,y,][ww_constraint[,,,drop=F]] <- (required_wat_min_allocation[c,y,,drop=F] + frac_fullirrig[c,y,,drop=F] * required_wat_fullirrig_ww[c,y,,drop=F])[ww_constraint[,,,drop=F]]
        }
      }
    } else {
      stop("Please choose allocation rule for river basin discharge allocation algorithm")
    }

    if (output=="discharge") {
      # Main output for MAgPIE: water available for agricultural consumption
      out <- discharge
      dataname <- "discharge"
      description="Cellular discharge after accounting for known human uses along the river"
    } else if (output=="frac_fullirrig") {
      # Main output for MAgPIE: water available for agricultural withdrawal
      out <- frac_fullirrig
      dataname <- "frac_fullirrig"
      description="Fraction of full irrigation requirements that can be fulfilled"
    } else {
      stop("specify outputtype")
    }

    out <- setNames(setYears(as.magpie(out, spatial=1), y), nm = dataname)
    out <- mbind(tmp, out)
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
