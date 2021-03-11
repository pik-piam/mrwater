#' @title       calcRiverSurplusDischargeAllocation
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
#' \dontrun{ calcOutput("RiverSurplusDischargeAllocation", aggregate = FALSE) }
#'

calcRiverSurplusDischargeAllocation <- function(selectyears, output, climatetype, allocationrule, thresholdtype, gainthreshold, irrigationsystem, iniyear, protect_scen, proxycrop) {

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River Structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))
  # cells as numeric for surplus discharge allocation algorithm
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

  ################################################
  ####### River basin discharge allocation #######
  ################################################

  if (class(selectyears)=="numeric") {
    selectyears <- paste0("y", selectyears)
  }

  out_tmp1 <- NULL
  out_tmp2 <- NULL
  out      <- NULL

  meancellrank             <- as.array(meancellrank)[,,1]
  irrig_yieldgainpotential <- as.array(irrig_yieldgainpotential)[,,1]
  required_wat_fullirrig_ww   <- as.array(required_wat_fullirrig_ww)[,,1]
  required_wat_fullirrig_wc   <- as.array(required_wat_fullirrig_wc)[,,1]

  for (EFP in c("on", "off")) {
    for (scen in unique(gsub(".*.\\.", "", getNames(required_wat_min_allocation)))){
      for (y in selectyears){

        O_discharge    <- as.array(discharge[,y,paste(EFP,scen,sep=".")])[,1,1]
        frac_fullirrig <- array(data=0,dim=67420)
        avl_wat_ww     <- array(data=0,dim=67420)
        avl_wat_wc     <- array(data=0,dim=67420)
        O_required_wat_min_allocation <- as.array(required_wat_min_allocation[,,paste(EFP,scen,sep=".")])[,1,1]

      # Allocate water for full irrigation to cell with highest yield improvement through irrigation
      if (allocationrule=="optimization") {

        for (o in (1:max(meancellrank[,y],na.rm=T))){

          c <- rs$cells[meancellrank[,y]==o]

          if (irrig_yieldgainpotential[c,y] > gainthreshold) {

            # available water for additional irrigation withdrawals
            avl_wat_ww <- max(O_discharge[c]-O_required_wat_min_allocation[c],0)

            # withdrawal constraint
            if (required_wat_fullirrig_ww[c,y]>0) {
              # how much withdrawals can be fulfilled by available water
              frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)

              # consumption constraint
              if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
                # available water for additional irrigation consumption (considering downstream availability)
                avl_wat_wc          <- max(min(O_discharge[rs$downstreamcells[[c]]] - O_required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
                # how much consumption can be fulfilled by available water
                frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
              }

              # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
              O_discharge[c(rs$downstreamcells[[c]],c)] <- O_discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
              # update minimum water required in cell:
              O_required_wat_min_allocation[c] <- O_required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
            }
          }
        }

      } else if (allocationrule=="upstreamfirst") {
        # Allocate full irrigation requirements to most upstream cell first (calcorder)

        for (o in 1:max(rs$calcorder)){
          cells <- which(rs$calcorder==o)

          for (c in cells){

            # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
            if (irrig_yieldgainpotential[c,y] > gainthreshold) {
              # available water for additional irrigation withdrawals
              avl_wat_ww <- max(O_discharge[c]-O_required_wat_min_allocation[c],0)

              # withdrawal constraint
              if (required_wat_fullirrig_ww[c,y]>0) {
                # how much withdrawals can be fulfilled by available water
                frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)

                # consumption constraint
                if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
                  # available water for additional irrigation consumption (considering downstream availability)
                  avl_wat_wc          <- max(min(O_discharge[rs$downstreamcells[[c]]] - O_required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
                  # how much consumption can be fulfilled by available water
                  frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
                }

                # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
                O_discharge[c(rs$downstreamcells[[c]],c)] <- O_discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
                # update minimum water required in cell:
                O_required_wat_min_allocation[c] <- O_required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
              }
            }
          }
        }

      } else {
        stop("Please choose allocation rule for river basin discharge allocation algorithm")
      }

      if (output=="discharge") {
        # Main output for MAgPIE: water available for agricultural consumption
        wat_avl_irrig <- as.magpie(O_discharge, spatial=1)
        dataname <- "wat_avl_irrig_c"
        description="Discharge"
      } else if (output=="frac_fullirrig") {
        # Main output for MAgPIE: water available for agricultural withdrawal
        wat_avl_irrig <- as.magpie(frac_fullirrig, spatial=1)
        dataname <- "wat_avl_irrig_w"
        description="Available water for irrigation withdrawals per year"
      } else {
        stop("specify type of output: discharge or frac_fullirrig")
      }

      wat_avl_irrig <- setNames(setYears(as.magpie(wat_avl_irrig,spatial=1),y), dataname)
      wat_avl_irrig <- add_dimension(wat_avl_irrig, dim=3.1, add="nonag_scen", nm=scen)
      wat_avl_irrig <- add_dimension(wat_avl_irrig, dim=3.1, add="EFP", nm=EFP)
      out_tmp1      <- mbind(out_tmp1, wat_avl_irrig)
    }
      out_tmp2 <- mbind(out_tmp2, out_tmp1)
      out_tmp1 <- NULL
  }
    out      <- mbind(out, out_tmp2)
    out_tmp2 <- NULL
}

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
