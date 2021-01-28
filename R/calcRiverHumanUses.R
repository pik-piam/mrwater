#' @title calcRiverHumanUses
#' @description This function calculates natural discharge for the river routing derived from inputs from LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param humanuse    Human use type to which river routing shall be applied (non_agriculture or committed_agriculture). Note: non_agriculture must be run prior to committed_agriculture
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#'
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom madrat calcOutput
#' @import mrmagpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("calcRiverHumanUses", aggregate = FALSE) }
#'

calcRiverHumanUses <- function(selectyears="all", humanuse="non_agriculture",
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
  rs <- readRDS(system.file("extdata/riverstructure_stn.rds", package="mrwater"))

  ### Internal functions to read in data
  ## LPJmL data
  .getLPJmLData <- function(subtype, cfg) {
    x <- calcOutput("LPJmL", version="LPJmL4", selectyears=cfg$selectyears,
                    climatetype=cfg$climatetype, harmonize_baseline=cfg$harmonize_baseline, ref_year=cfg$ref_year, time=cfg$time, dof=cfg$dof, averaging_range=cfg$averaging_range,
                    subtype=subtype, aggregate=FALSE)
    x <- as.array(collapseNames(x))[,,1]
    return(x)
  }
  ## Non-agricultural water demand data
  # .getNonAgData
  ## Committed agricultural water demand data
  # .getCommAgData

  ### Required inputs for River Routing:
  ## LPJmL water data
  #!# NOTE: Only for development purposes.
  #!# In future: can drop smoothing and harmonization argument.
  #!# Water inputs should always be harmonized and smoothed before read in...
  cfg <- list(selectyears=selectyears, climatetype=climatetype,
              harmonize_baseline=harmonize_baseline, ref_year=ref_year,
              time=time, dof=dof, averaging_range=averaging_range)
  # Yearly runoff (mio. m^3 per yr) [smoothed & harmonized]
  yearly_runoff <- .getLPJmLData("runoff_lpjcell",     cfg)
  # Precipitation/Runoff on lakes and rivers from LPJmL (in mio. m^3 per year) [smoothed & harmonized]
  input_lake    <- .getLPJmLData("input_lake_lpjcell", cfg)

  # Calculate Runoff (on land and water)
  yearly_runoff <- yearly_runoff + input_lake

  ## Human uses
  # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
  NAg_ww_magpie           <- calcOutput("WaterUseNonAg", source="WATERGAP2020", selectyears=selectyears, time=time, dof=dof, averaging_range=averaging_range, waterusetype="withdrawal", seasonality="total", aggregate=FALSE)
  getCells(NAg_ww_magpie) <- paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep=".")
  NAg_ww           <- new.magpie(1:67420,getYears(NAg_ww_magpie),getNames(NAg_ww_magpie))
  NAg_ww[,,]       <- 0
  NAg_ww[paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep="."),,] <- NAg_ww_magpie[,,]
  getCells(NAg_ww) <- paste(lpj_cells_map$ISO,1:67420,sep=".")
  NAg_ww           <- as.array(collapseNames(NAg_ww))
  rm(NAg_ww_magpie)

  # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
  NAg_wc_magpie           <- calcOutput("WaterUseNonAg", source="WATERGAP2020", selectyears=selectyears, time=time, dof=dof, averaging_range=averaging_range, waterusetype="consumption", seasonality="total", aggregate=FALSE)
  getCells(NAg_wc_magpie) <- paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep=".")
  NAg_wc           <- new.magpie(1:67420,getYears(NAg_wc_magpie),getNames(NAg_wc_magpie))
  NAg_wc[,,]       <- 0
  NAg_wc[paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep="."),,] <- NAg_wc_magpie[,,]
  getCells(NAg_wc) <- paste(lpj_cells_map$ISO,1:67420,sep=".")
  NAg_wc           <- as.array(collapseNames(NAg_wc))
  rm(NAg_wc_magpie)

  # Harmonize non-agricultural consumption and withdrawals (withdrawals > consumption)
  NAg_ww <- pmax(NAg_ww, NAg_wc)
  NAg_wc <- pmax(NAg_wc, 0.01*NAg_ww)

  # Committed agricultural uses (in mio. m^3 / yr) [for initialization year]
  CAU_magpie <- calcOutput("WaterUseCommittedAg",selectyears=selectyears,cells="lpjcell",iniyear=iniyear,irrigini=paste0(unlist(str_split(irrigini, "_"))[[1]],"_lpjcell"),time=time,dof=dof,averaging_range=averaging_range,harmonize_baseline=harmonize_baseline,ref_year=ref_year,aggregate=FALSE)
  CAW_magpie <- as.array(collapseNames(dimSums(CAU_magpie[,,"withdrawal"],dim=3)))
  CAC_magpie <- as.array(collapseNames(dimSums(CAU_magpie[,,"consumption"],dim=3)))
  rm(CAU_magpie)
  CAW_magpie <- as.array(collapseNames(CAW_magpie))
  CAW_magpie <- CAW_magpie[,,1]
  CAC_magpie <- as.array(collapseNames(CAC_magpie))
  CAC_magpie <- CAC_magpie[,,1]


  for (EFP in c("on", "off")) {

    ## Inputs from previous river routings
    if (humanuse=="non_agriculture") {
      # Lake evaporation as calculated by natural flow river routing
      IO_lake_evap_new    <- collapseNames(calcOutput("RiverNaturalFlows", selectyears=selectyears, version=version, aggregate=FALSE,
                                                    climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof,
                                                    harmonize_baseline=harmonize_baseline, ref_year=ref_year)[,,"lake_evap_nat"])
      IO_lake_evap_new    <- as.array(IO_lake_evap_new)[,,1]

      # Minimum flow requirements determined by natural flow river routing: Environmental Flow Requirements (in mio. m^3 / yr) [long-term average]
      IO_required_wat_min <- calcOutput("EnvmtlFlowRequirements", selectyears=selectyears, version="LPJmL4", climatetype=climatetype, aggregate=FALSE,
                                   LFR_val=0.1, HFR_LFR_less10=0.2, HFR_LFR_10_20=0.15, HFR_LFR_20_30=0.07, HFR_LFR_more30=0.00,
                                   EFRyears=c(1980:2010))
      if (EFP=="off") {
        IO_required_wat_min[,,] <- 0
      }
      IO_required_wat_min <- as.array(collapseNames(IO_required_wat_min))[,,1]

      ## Current human uses
      # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
      currHuman_ww <- NAg_ww

      # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
      currHuman_wc <- NAg_wc

      # There are no previous human uses yet to be considered
      frac_prevHuman_fulfilled     <- currHuman_wc
      frac_prevHuman_fulfilled[,,] <- 0
      prevHuman_wc           <- currHuman_wc
      prevHuman_wc[,,]       <- 0
      prevHuman_ww           <- currHuman_ww
      prevHuman_ww[,,]       <- 0


    } else if (humanuse=="committed_agriculture") {
      # Lake evaporation and minimum flow requirements as calculated by non-agricultural human uses river routing
      #####################
      #### call itself ####
      #####################
      #IO_lake_evap_new <-
      #IO_required_wat_min <-
      #frac_prevHuman_fulfilled <-


      ## Current human uses
      # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
      currHuman_ww <- CAW_magpie

      # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
      currHuman_wc <- CAC_magpie

      ## Previous human uses
      # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
      prevHuman_ww <- NAg_ww

      # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
      prevHuman_wc <- NAg_wc


    } else {
      stop("Please specify for which type of human uses river routing shall be calculated: non_agriculture or committed_agriculture")
    }

    ####################################################
    ###### River Routing considering Human Uses ########
    ####################################################
    if (class(selectyears)=="numeric") {
      selectyears <- paste0("y",selectyears)
    }

    tmp <- NULL
    out <- NULL

    for (y in selectyears){
      for (scen in getNames(prevHuman_wc)){

        #############################
        ####### River routing #######
        #############################

        # helper variables for river routing
        inflow      <- array(data=0,dim=67420,dimnames=list(names(IO_required_wat_min[,y])))
        avl_wat_act <- array(data=0,dim=67420,dimnames=list(names(IO_required_wat_min[,y])))
        # output variable that will be filled during river routing
        frac_currHuman_fulfilled <- array(data=0,dim=67420,dimnames=list(names(IO_required_wat_min[,y])))
        discharge                <- array(data=0,dim=67420,dimnames=list(names(IO_required_wat_min[,y])))


        ### River Routing 3: Committed agricultural uses considering local EFRs and non-agricultural uses ###
        for (o in 1:max(rs$calcorder)) {
          # Note: the calcorder ensures that the upstreamcells are calculated first
          cells <- which(rs$calcorder==o)

          for (c in cells){
            # available water in cell
            avl_wat_act[c]  <- inflow[c]+yearly_runoff[c,y]-IO_lake_evap_new[c,y]

            # available water in cell not sufficient to fulfill requirements
            # -> no more water can be withdrawn
            if (avl_wat_act[c]<IO_required_wat_min[c,y]){
              # if cell has upstreamcells: upstreamcells must release missing water (cannot be consumed upstream)
              # -> reduce committed agricultural water consumption in upstream cells
              # -> locally: cannot withdraw
              if (length(rs$upstreamcells[c])>0){
                # upstream committed agricultural water consumption:
                upstream_cons <- sum(currHuman_wc[rs$upstreamcells[[c]], y, scen]*frac_currHuman_fulfilled[rs$upstreamcells[[c]]])
                if (upstream_cons>IO_required_wat_min[c,y]-avl_wat_act[c]){
                  # if upstream_cons high enough to account for difference: reduce upstream consumption respectively
                  frac_currHuman_fulfilled[rs$upstreamcells[[c]]] <- (1-(IO_required_wat_min[c,y]-avl_wat_act[c])/upstream_cons)*frac_currHuman_fulfilled[rs$upstreamcells[[c]]]
                  discharge[c] <- IO_required_wat_min[c,y] - prevHuman_wc[c,y,scen]*frac_prevHuman_fulfilled[c,y,scen]
                } else {
                  # if upstream_cons not sufficient to account for difference: no water can be used upstream
                  frac_currHuman_fulfilled[rs$upstreamcells[[c]]] <- 0
                  discharge[c] <- avl_wat_act[c]+upstream_cons - prevHuman_wc[c,y,scen]*frac_prevHuman_fulfilled[c,y,scen]
                }
              }

              # available water in cell is sufficient to fulfill requirements
              # -> further withdrawal possible
            } else {
              # Committed agricultural withdrawals
              if (currHuman_ww[c,y,scen]>0){
                ## Water withdrawal constraint:
                frac_currHuman_fulfilled[c] <- min((avl_wat_act[c]-IO_required_wat_min[c,y])/currHuman_ww[c,y,scen], 1)
              }

              ## Outflow from one cell to the next
              # (Subtract local water consumption in current cell (committed ag. & non-agricultural consumption))
              discharge[c] <- avl_wat_act[c] - currHuman_wc[c,y,scen]*frac_currHuman_fulfilled[c] - prevHuman_wc[c,y,scen]*frac_prevHuman_fulfilled[c,y,scen]
            }

            if (rs$nextcell[c]>0){
              inflow[rs$nextcell[c]] <- inflow[rs$nextcell[c]] + discharge[c]
            }
          }
        }
        # Update minimum water required in cell:
        IO_required_wat_min[,y] <- IO_required_wat_min[,y] + currHuman_ww[,y,scen]*frac_currHuman_fulfilled

      }
    }

    ## Outputs:
    tmp <- new.magpie(cells_and_regions = getCells(yearly_runoff), years=y, names=c("frac_currHuman_fulfilled", "IO_required_wat_min"))

    frac_currHuman_fulfilled <- setCells(setNames(setYears(as.magpie(frac_currHuman_fulfilled, spatial=1),y), nm="frac_currHuman_fulfilled"), getCells(yearly_runoff))
    IO_required_wat_min <- setCells(setNames(setYears(as.magpie(IO_required_wat_min,spatial=1),y), nm="IO_required_wat_min"), getCells(yearly_runoff))

    tmp[,,"frac_currHuman_fulfilled"] <- frac_currHuman_fulfilled[,,]
    tmp[,,"IO_required_wat_min"] <- IO_required_wat_min[,,]

    out <- mbind(out, tmp)
    tmp <- NULL
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="Cellular natural discharge and lake evaporation in natural river condition",
    isocountries=FALSE))

}
