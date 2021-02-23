#' @title calcWaterAllocation
#' @description This function calculates water availability for MAgPIE retrieved from LPJmL using a river routing and allocation algorithm for distribution of discharge within the river basin
#'
#' @param version     Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param selectyears Years to be returned
#' @param output      Water availability output to be returned: withdrawal or consumption
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param allocationshare Share of water to be allocated to cell (only needs to be selected in case of allocationrule=="equality")
#' @param thresholdtype   Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear          Initialization year of irrigation system
#' @param protect_scen     Land protection scenario
#' @param finalcells       Number of cells to be returned by the function (lpjcell: 67420, magpiecell: 59199)
#'
#' @import magclass
#' @import madrat
#' @import mrcommons
#' @import mrmagpie
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("WaterAllocation", aggregate = FALSE) }
#'

calcWaterAllocation <- function(selectyears="all", output="consumption", finalcells="magpiecell",
                                version="LPJmL4", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4,
                                harmonize_baseline="CRU_4", ref_year="y2015",
                                allocationrule="optimization", allocationshare=NULL, thresholdtype=TRUE, gainthreshold=10,
                                irrigationsystem="initialization", iniyear=1995, protect_scen) {

  #############################
  ####### Read in Data ########
  #############################

  ### Read in river structure
  # Note: river structure derived from LPJmL input (drainage) [maybe later: implement readDrainage function]
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))

  # Number of cells to be used for calculation
  NCELLS <- 67420

  ### LPJ-MAgPIE cell mapping
  magpie2lpj    <- magclassdata$cellbelongings$LPJ_input.Index
  lpj_cells_map <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell")

  ### Required inputs for River Routing:
  # Yearly runoff (mio. m^3 per yr) [smoothed & harmonized]
  yearly_runoff <- calcOutput("LPJmL", version="LPJmL4", selectyears=selectyears, climatetype=climatetype, subtype="runoff_lpjcell", aggregate=FALSE,
                              harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof, averaging_range=averaging_range)
  getCells(yearly_runoff) <- rs$coordinates
  yearly_runoff <- as.array(collapseNames(yearly_runoff))
  yearly_runoff <- yearly_runoff[,,1]

  # Yearly lake evapotranspiration (in mio. m^3 per year) [smoothed & harmonized]
  lake_evap     <- calcOutput("LPJmL", version="LPJmL4", selectyears=selectyears, climatetype=climatetype, subtype="evap_lake_lpjcell", aggregate=FALSE,
                              harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof, averaging_range=averaging_range)
  getCells(lake_evap) <- rs$coordinates
  lake_evap     <- as.array(collapseNames(lake_evap))
  lake_evap     <- lake_evap[,,1]
  #### LAKE EVAP PLACEHOLDER
  # lake_evap     <- yearly_runoff
  # lake_evap[]   <- 0

  # Precipitation/Runoff on lakes and rivers from LPJmL (in mio. m^3 per year) [smoothed & harmonized]
  input_lake    <- calcOutput("LPJmL", version="LPJmL4", selectyears=selectyears, climatetype=climatetype, subtype="input_lake_lpjcell", aggregate=FALSE,
                               harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof, averaging_range=averaging_range)
  getCells(input_lake) <- rs$coordinates
  input_lake    <- as.array(collapseNames(input_lake))
  input_lake    <- input_lake[,,1]
  #### LAKE EVAP PLACEHOLDER
  # input_lake     <- yearly_runoff
  # input_lake[]   <- 0

  # runoff (on land and water)
  yearly_runoff <- yearly_runoff + input_lake

  # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
  # Non-Agricultural Water Withdrawals and Consumption (in mio. m^3 / yr) [smoothed]
  wat_nonag <- calcOutput("WaterUseNonAg", source="WATERGAP2020", seasonality="total", finalcells="lpjcell", aggregate=FALSE, selectyears=selectyears, climatetype=climatetype, time=time, dof=dof, averaging_range=averaging_range, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
  wat_nonag <- wat_nonag[rs$coordinates,,]
  NAg_ww  <- as.array(collapseNames(wat_nonag[,,"withdrawal"]))
  NAg_wc  <- as.array(collapseNames(wat_nonag[,,"consumption"]))

  # Committed agricultural uses (in mio. m^3 / yr) [for initialization year]
  CAU_magpie <- calcOutput("WaterUseCommittedAg",selectyears=selectyears,iniyear=iniyear,time=time,dof=dof,averaging_range=averaging_range,harmonize_baseline=harmonize_baseline,ref_year=ref_year,aggregate=FALSE)
  CAW_magpie <- as.array(collapseNames(dimSums(CAU_magpie[,,"withdrawal"],dim=3)))
  CAC_magpie <- as.array(collapseNames(dimSums(CAU_magpie[,,"consumption"],dim=3)))
  rm(CAU_magpie)
  CAW_magpie <- as.array(collapseNames(CAW_magpie))
  CAW_magpie <- CAW_magpie[,,1]
  CAC_magpie <- as.array(collapseNames(CAC_magpie))
  CAC_magpie <- CAC_magpie[,,1]

  ### Required inputs for Allocation Algorithm:
  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig    <- calcOutput("FullIrrigationRequirement", version="LPJmL5", selectyears=selectyears, climatetype=climatetype, harmonize_baseline=harmonize_baseline, time=time, dof=dof, iniyear=iniyear, iniarea=TRUE, irrigationsystem=irrigationsystem, protect_scen=protect_scen, aggregate=FALSE)[,,c("maiz","rapeseed","puls_pro")]
  required_wat_fullirrig_ww <- collapseNames(required_wat_fullirrig[,,"withdrawal"])
  required_wat_fullirrig_wc <- collapseNames(required_wat_fullirrig[,,"consumption"])
  required_wat_fullirrig_ww <- pmax(required_wat_fullirrig_ww,0)
  required_wat_fullirrig_wc <- pmax(required_wat_fullirrig_wc,0)

  # average required water for full irrigation across selected proxy crops
  required_wat_fullirrig_ww <- dimSums(required_wat_fullirrig_ww,dim=3)/length(getNames(required_wat_fullirrig_ww))
  required_wat_fullirrig_wc <- dimSums(required_wat_fullirrig_wc,dim=3)/length(getNames(required_wat_fullirrig_wc))

  # transform to array for further calculations
  required_wat_fullirrig_ww <- as.array(collapseNames(required_wat_fullirrig_ww))[,,1]
  required_wat_fullirrig_wc <- as.array(collapseNames(required_wat_fullirrig_wc))[,,1]

  # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
  meancellrank <- calcOutput("IrrigCellranking", version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=FALSE, ref_year="y2015",
                             cellrankyear=selectyears, cells="lpjcell", method="meancroprank", proxycrop=c("maiz", "rapeseed", "puls_pro"), iniyear=iniyear, aggregate=FALSE)
  meancellrank <- as.array(meancellrank)[,,1]

  # Yield gain potential through irrigation of proxy crops
  irrig_yieldgainpotential <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, averaging_range=averaging_range, dof=dof,
                                cells="lpjcell", proxycrop=c("maiz", "rapeseed", "puls_pro"), monetary=thresholdtype, aggregate=FALSE)
  irrig_yieldgainpotential <- as.array(irrig_yieldgainpotential)[,,1]

  ############################################
  ####### Routing and Allocation Loop ########
  ############################################
  if (class(selectyears)=="numeric") {
    selectyears <- paste0("y",selectyears)
  }

  out_tmp1 <- NULL
  out_tmp2 <- NULL
  out      <- NULL

  for (EFP in c("on", "off")) {

    # Environmental Flow Requirements (share of mean annual discharge) [long-term average]
    if (EFP=="on"){
      EFR_magpie_frac <- calcOutput("EnvmtlFlowRequirementsShare", version="LPJmL4", climatetype=climatetype, aggregate=FALSE,
                               LFR_val=0.1, HFR_LFR_less10=0.2, HFR_LFR_10_20=0.15, HFR_LFR_20_30=0.07, HFR_LFR_more30=0.00,
                               EFRyears=c(1980:2010))
    } else if (EFP=="off"){
      EFR_magpie_frac <- new.magpie(1:NCELLS, fill=0)
      getCells(EFR_magpie_frac) <- rs$coordinates
    }
    EFR_magpie_frac <- as.array(collapseNames(EFR_magpie_frac))
    EFR_magpie_frac <- EFR_magpie_frac[,1,1]

    for (scen in getNames(NAg_ww)){
      for (y in selectyears){

        #############################
        ####### River routing #######
        #############################

        ## Global river routing variables
        # Naturalized discharge
        discharge_nat <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))
        inflow_nat    <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))
        lake_evap_new <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))
        # Discharge considering human uses
        discharge   <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))
        inflow      <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))
        avl_wat_act <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))
        # Water fractions reserved for certain uses
        frac_NAg_fulfilled <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))
        frac_CAg_fulfilled <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))
        frac_fullirrig     <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))
        required_wat_min   <- array(data=0,dim=NCELLS,dimnames=list(names(EFR_magpie_frac)))

        ### River Routing 1.1: Natural flows ###
        # Determine natural discharge
        for (o in 1:max(rs$calcorder)){
          # Note: the calcorder ensures that upstreamcells are calculated first
          cells <- which(rs$calcorder==o)

          for (c in cells){
            ### Natural water balance
            # lake evap that can be fulfilled (if water available: lake evaporation considered; if not: lake evap is reduced respectively):
            lake_evap_new[c] <- min(lake_evap[c,y], inflow_nat[c]+yearly_runoff[c,y])
            # natural discharge
            discharge_nat[c] <- inflow_nat[c] + yearly_runoff[c,y] - lake_evap_new[c]
            # inflow into nextcell
            if (rs$nextcell[c]>0){
              inflow_nat[rs$nextcell[c]] <- inflow_nat[rs$nextcell[c]] + discharge_nat[c]
            }
          }
        }

        # REPORTING: cellular natural discharge
        wat_av_natural    <- discharge_nat

        # EFRs (in mio. m^3 / yr)
        EFR_magpie <- EFR_magpie_frac*discharge_nat
        # Minimum availability of water in river to fulfill local EFRs
        required_wat_min <- EFR_magpie

        ### River Routing 2: Non-agricultural uses considering local EFRs ###
        for (o in 1:max(rs$calcorder)) {
          # Note: the calcorder ensures that the upstreamcells are calculated first
          cells <- which(rs$calcorder==o)

          for (c in cells){
            # available water in cell
            avl_wat_act[c]  <- inflow[c]+yearly_runoff[c,y]-lake_evap_new[c]

            # available water in cell not sufficient to fulfill requirements
            # -> no more water can be withdrawn
            if (avl_wat_act[c]<required_wat_min[c]){
              # if cell has upstreamcells: upstreamcells must release missing water (cannot be consumed upstream)
              # -> reduce non-agricultural water consumption in upstream cells
              # -> locally: cannot withdraw
              if (length(rs$upstreamcells[[c]])>0){
                # upstream non-agricultural water consumption
                upstream_cons <- sum(NAg_wc[rs$upstreamcells[[c]],y,scen]*frac_NAg_fulfilled[rs$upstreamcells[[c]]])
                if (upstream_cons>required_wat_min[c]-avl_wat_act[c]){
                  # if missing water (difference) can be fulfilled by upstream consumption: reduce upstream consumption
                  frac_NAg_fulfilled[rs$upstreamcells[[c]]] <- (1-(required_wat_min[c]-avl_wat_act[c])/upstream_cons)*frac_NAg_fulfilled[rs$upstreamcells[[c]]]
                  discharge[c] <- required_wat_min[c]
                } else {
                  # if missing water (difference) cannot be fulfilled by upstream consumption: no upstream consumption
                  frac_NAg_fulfilled[rs$upstreamcells[[c]]] <- 0
                  discharge[c] <- avl_wat_act[c]+upstream_cons
                }
              }

              # available water not sufficient & no upstream cells: no withdrawals take place
              frac_NAg_fulfilled[c] <- 0
              discharge[c]          <- avl_wat_act[c]

              # available water in cell is sufficient to fulfill requirements
              # -> further withdrawals are possible
            } else {
              # Non-agricultural withdrawals
              if (NAg_ww[c,y,scen]>0){
                ## Water withdrawal constraint:
                frac_NAg_fulfilled[c] <- min((avl_wat_act[c]-required_wat_min[c])/NAg_ww[c,y,scen], 1)
              }

              ## Outflow from one cell to the next
              # (Subtract local water consumption in current cell (non-ag. consumption))
              discharge[c] <- avl_wat_act[c] - NAg_wc[c,y,scen]*frac_NAg_fulfilled[c]
            }

            if (rs$nextcell[c]>0){
              inflow[rs$nextcell[c]] <- inflow[rs$nextcell[c]] + discharge[c]
            }
          }
        }

        # Update minimum water required in cell:
        required_wat_min <- required_wat_min + NAg_ww[,y,scen]*frac_NAg_fulfilled

        test_output_non_ag_reqwat <- required_wat_min

        # REPORTING: Non-agricultural water use
        water_use_nonag_ww <- NAg_ww[,y,scen]*frac_NAg_fulfilled
        water_use_nonag_wc <- NAg_wc[,y,scen]*frac_NAg_fulfilled

        # inflow needs to be set to 0 prior to every river routing (is recalculated by the routing)
        inflow[] <- 0

        ### River Routing 3: Committed agricultural uses considering local EFRs and non-agricultural uses ###
        for (o in 1:max(rs$calcorder)) {
          # Note: the calcorder ensures that the upstreamcells are calculated first
          cells <- which(rs$calcorder==o)

          for (c in cells){
            # available water in cell
            avl_wat_act[c]  <- inflow[c]+yearly_runoff[c,y]-lake_evap_new[c]

            # available water in cell not sufficient to fulfill requirements
            # -> no more water can be withdrawn
            if (avl_wat_act[c]<required_wat_min[c]){
              # if cell has upstreamcells: upstreamcells must release missing water (cannot be consumed upstream)
              # -> reduce committed agricultural water consumption in upstream cells
              # -> locally: cannot withdraw
              if (length(rs$upstreamcells[[c]])>0){
                # upstream committed agricultural water consumption:
                upstream_cons <- sum(CAC_magpie[rs$upstreamcells[[c]],y]*frac_CAg_fulfilled[rs$upstreamcells[[c]]])
                if (upstream_cons>required_wat_min[c]-avl_wat_act[c]){
                  # if upstream_cons high enough to account for difference: reduce upstream consumption respectively
                  frac_CAg_fulfilled[rs$upstreamcells[[c]]] <- (1-(required_wat_min[c]-avl_wat_act[c])/upstream_cons)*frac_CAg_fulfilled[rs$upstreamcells[[c]]]
                  discharge[c] <- required_wat_min[c] - NAg_wc[c,y,scen]*frac_NAg_fulfilled[c]
                } else {
                  # if upstream_cons not sufficient to account for difference: no water can be used upstream
                  frac_CAg_fulfilled[rs$upstreamcells[[c]]] <- 0
                  discharge[c] <- avl_wat_act[c]+upstream_cons - NAg_wc[c,y,scen]*frac_NAg_fulfilled[c]
                }
              }

              # available water not sufficient & no upstream cells: no withdrawals take place
              frac_CAg_fulfilled[c] <- 0
              discharge[c]          <- avl_wat_act[c] - NAg_wc[c,y,scen]*frac_NAg_fulfilled[c]

              # available water in cell is sufficient to fulfill requirements
              # -> further withdrawal possible
            } else {
              # Committed agricultural withdrawals
              if (CAW_magpie[c,y]>0){
                ## Water withdrawal constraint:
                frac_CAg_fulfilled[c] <- min((avl_wat_act[c]-required_wat_min[c])/CAW_magpie[c,y], 1)
              }

              ## Outflow from one cell to the next
              # (Subtract local water consumption in current cell (committed ag. & non-agricultural consumption))
              discharge[c] <- avl_wat_act[c] - CAC_magpie[c,y]*frac_CAg_fulfilled[c] - NAg_wc[c,y,scen]*frac_NAg_fulfilled[c]
            }

            if (rs$nextcell[c]>0){
              inflow[rs$nextcell[c]] <- inflow[rs$nextcell[c]] + discharge[c]
            }
          }
        }

        # Update minimum water required in cell:
        required_wat_min <- required_wat_min + CAW_magpie[,y]*frac_CAg_fulfilled

        ### Interim routing: Update discharge and inflow considering known non-agricultural and committed agricultural uses of river routing 3 ###
        inflow[] <- 0

        for (o in 1:max(rs$calcorder)){
          # Note: the calcorder ensures that the upstreamcells are calculated first
          cells <- which(rs$calcorder==o)

          for (c in cells){
            # available water
            avl_wat_act[c] <- inflow[c] + yearly_runoff[c,y] - lake_evap_new[c]
            # discharge
            discharge[c]   <- avl_wat_act[c] - NAg_wc[c,y,scen]*frac_NAg_fulfilled[c] - CAC_magpie[c,y]*frac_CAg_fulfilled[c]
            # inflow into nextcell
            if (rs$nextcell[c]>0){
              inflow[rs$nextcell[c]] <- inflow[rs$nextcell[c]] + discharge[c]
            }
          }
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

        #################
        #### OUTPUTS ####
        #################
        ### MAIN OUTPUT VARIABLE: water available for irrigation (consumptive agricultural use)
        wat_avl_irrig_c <- CAC_magpie[,y]*frac_CAg_fulfilled + frac_fullirrig*required_wat_fullirrig_wc[,y]
        wat_avl_irrig_w <- CAW_magpie[,y]*frac_CAg_fulfilled + frac_fullirrig*required_wat_fullirrig_ww[,y]
        wat_avl_total_human_c <- CAC_magpie[,y]*frac_CAg_fulfilled + frac_fullirrig*required_wat_fullirrig_wc[,y] + frac_NAg_fulfilled*NAg_wc[,y,scen]
        wat_avl_total_human_w <- CAW_magpie[,y]*frac_CAg_fulfilled + frac_fullirrig*required_wat_fullirrig_ww[,y] + frac_NAg_fulfilled*NAg_ww[,y,scen]
        wat_avl_nonAg_c <- frac_NAg_fulfilled*NAg_wc[,y,scen]
        wat_avl_nonAg_w <- frac_NAg_fulfilled*NAg_ww[,y,scen]
        wat_avl_commAg_c <- CAC_magpie[,y]*frac_CAg_fulfilled
        wat_avl_commAg_w <- CAW_magpie[,y]*frac_CAg_fulfilled

        if (output=="consumption") {
          # Main output for MAgPIE: water available for agricultural consumption
          wat_avl_irrig <- wat_avl_irrig_c
          dataname <- "wat_avl_irrig_c"
          description="Available water for irrigation consumption per year"
        } else if (output=="withdrawal") {
          # Main output for MAgPIE: water available for agricultural withdrawal
          wat_avl_irrig <- wat_avl_irrig_w
          dataname <- "wat_avl_irrig_w"
          description="Available water for irrigation withdrawals per year"

        ### Reporting outputs
        } else if (output=="total_consumption") {
          # Total human water consumption
          wat_avl_irrig <- wat_avl_total_human_c
          dataname <- "wat_avl_total_human_c"
          description="Total human water consumption per year"
        } else if (output=="total_withdrawal") {
          # Total human water withdrawal
          wat_avl_irrig <- wat_avl_total_human_w
          dataname <- "wat_avl_total_human_w"
          description="Total human water withdrawal per year"
        } else if (output=="nonag_consumption") {
          # Non-agricultural water consumption
          if (any(water_use_nonag_wc!=wat_avl_nonAg_c)) stop("This shouldn't be the case. Check the function again.")
          wat_avl_irrig <- wat_avl_nonAg_c
          dataname <- "wat_avl_nonAg_c"
          description="Non-agricultural water consumption per year"
        } else if (output=="nonag_withdrawal") {
          # Non-agricultural water withdrawals
          if (any(water_use_nonag_ww!=wat_avl_nonAg_w)) stop("This shouldn't be the case. Check the function again.")
          wat_avl_irrig <- wat_avl_nonAg_w
          dataname <- "wat_avl_nonAg_w"
          description="Non-agricultural water withdrawals per year"
        } else if (output=="commag_consumption") {
          # Total human water consumption
          wat_avl_irrig <- wat_avl_commAg_c
          dataname <- "wat_avl_commAg_c"
          description="Committed agricultural water consumption per year"
        } else if (output=="commag_withdrawal") {
          # Total human water withdrawal
          wat_avl_irrig <- wat_avl_commAg_w
          dataname <- "wat_avl_commAg_w"
          description="Committed agricultural water withdrawal per year"

        ### Intermediate outputs
        } else if (output=="nat_discharge") {
          # Natural discharge before human uses are considered
          wat_avl_irrig <- wat_av_natural
          dataname <- "nat_discharge"
          description="Cellular natural discharge not considering human uses"
        } else if (output=="discharge_before") {
          # Discharge after river routing, before allocation algorithm
          wat_avl_irrig <- discharge_befalgo
          dataname <- "discharge_befalgo"
          description="Cellular discharge before allocation algorithm executed"
        } else if (output=="discharge_opt") {
          # Discharge after optimization allocation algorithm
          wat_avl_irrig <- discharge_optimization
          dataname <- "discharge_optimization"
          description="Cellular discharge after optimization allocation algorithm executed"
        } else if (output=="discharge_up") {
          # Discharge after upstreamfirst allocation algorithm
          wat_avl_irrig <- discharge_upstreamfirst
          dataname <- "discharge_upstreamfirst"
          description="Cellular discharge after upstreamfirst allocation algorithm executed"
        } else if (output=="test_output_non_ag_reqwat") {
          # Discharge after upstreamfirst allocation algorithm
          wat_avl_irrig <- test_output_non_ag_reqwat
          dataname <- "test_output_non_ag_reqwat"
          description="test_output_non_ag_reqwat"
        } else {
          stop("specify type of water availability output: withdrawal or consumption")
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

  ### Correct number of cells
  if (finalcells=="lpjcell"){
    out <- out
  } else if (finalcells=="magpiecell"){
    out <- out[magclassdata$cellbelongings$LPJ_input.Index,,]
    out <- toolCell2isoCell(out)
  } else {
    stop("Cell argument not supported. Select lpjcell for 67420 cells or magpiecell for 59199 cells")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
