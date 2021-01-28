#' #' @title calcWaterAllocationAlgorithm
#' #' @description This function calculates water availability for MAgPIE retrieved from LPJmL using a river routing and allocation algorithm for distribution of discharge within the river basin
#' #'
#' #' @param version     Switch between LPJmL4 and LPJmL5
#' #' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' #' @param time            Time smoothing: average, spline or raw (default)
#' #' @param averaging_range only specify if time=="average": number of time steps to average
#' #' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' #' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' #' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' #' @param selectyears Years to be returned
#' #' @param output      Water availability output to be returned: withdrawal or consumption
#' #' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' #' @param allocationshare Share of water to be allocated to cell (only needs to be selected in case of allocationrule=="equality")
#' #' @param thresholdtype   Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' #' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' #' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' #' @param irrigini         When "initialization" selected for irrigation system: choose initialization data set for irrigation system initialization ("Jaegermeyr_lpjcell", "LPJmL_lpjcell")
#' #' @param iniyear          Initialization year of irrigation system
#' #' @param finalcells       Number of cells to be returned by the function (lpjcell: 67420, magpiecell: 59199)
#' #'
#' #' @import magclass
#' #' @import madrat
#' #' @import mrcommons
#' #' @import mrmagpie
#' #' @importFrom stringr str_split
#' #'
#' #' @return magpie object in cellular resolution
#' #' @author Felicitas Beier, Jens Heinke
#' #'
#' #' @examples
#' #' \dontrun{ calcOutput("WaterAllocationAlgorithm", aggregate = FALSE) }
#' #'
#'
#' calcWaterAllocationAlgorithm <- function(selectyears="all", output="consumption", finalcells="magpiecell",
#'                                 version="LPJmL4", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4,
#'                                 harmonize_baseline="CRU_4", ref_year="y2015",
#'                                 allocationrule="optimization", allocationshare=NULL, thresholdtype=TRUE, gainthreshold=10,
#'                                 irrigationsystem="initialization", irrigini="Jaegermeyr_lpjcell", iniyear=1995){
#'
#'   #############################
#'   ####### Read in Data ########
#'   #############################
#'
#'   ### Read in river structure
#'   # Note: river structure derived from LPJmL input (drainage) [maybe later: implement readDrainage function]
#'   # Information contained:
#'   ## upstreamcells:   all cells that are upstream of current cell (list of cells)
#'   ## downstreamcells: all cells that are downstream of current cell (list of cells)
#'   ## nextcell:        cell to which discharge of current cell flows (exactly 1 cell)
#'   ## endcell:         estuary cell of current cell, i.e. last cell of the river of which current cell is part of (exactly 1 cell)
#'   ## calcorder:       ordering of cells for calculation from upstream to downstream
#'   rs <- readRDS(system.file("extdata/riverstructure_stn.rds", package="mrwater"))
#'
#'   ### LPJ-MAgPIE cell mapping
#'   magpie2lpj    <- magclassdata$cellbelongings$LPJ_input.Index
#'   lpj_cells_map <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell")
#'
#'   ### Required inputs for River Routing:
#'   # Yearly runoff (mio. m^3 per yr) [smoothed & harmonized]
#'   yearly_runoff <- calcOutput("LPJmL", version="LPJmL4", selectyears=selectyears, climatetype=climatetype, subtype="runoff_lpjcell", aggregate=FALSE,
#'                               harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof, averaging_range=averaging_range)
#'   yearly_runoff <- as.array(collapseNames(yearly_runoff))
#'   yearly_runoff <- yearly_runoff[,,1]
#'   years <- getYears(yearly_runoff)
#'
#'   # Read in natural discharge and lake evaporation from Natural River Flow Routing
#'   natural_flows <- calcOutput("RiverNaturalFlows", version="LPJmL4", selectyears=selectyears, aggregate=FALSE,
#'                               climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
#'   discharge_nat <- as.array(collapseNames(natural_flows[,,"discharge_nat"]))[,,1]
#'   lake_evap_new <- as.array(collapseNames(natural_flows[,,"lake_evap_nat"]))[,,1]
#'
#'   # Non-Agricultural Water Withdrawals (in mio. m^3 / yr) [smoothed]
#'   NAg_ww_magpie           <- calcOutput("WaterUseNonAg", source="WATERGAP2020", selectyears=selectyears, time=time, dof=dof, averaging_range=averaging_range, waterusetype="withdrawal", seasonality="total", aggregate=FALSE)
#'   getCells(NAg_ww_magpie) <- paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep=".")
#'   NAg_ww           <- new.magpie(1:67420,getYears(NAg_ww_magpie),getNames(NAg_ww_magpie))
#'   NAg_ww[,,]       <- 0
#'   NAg_ww[paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep="."),,] <- NAg_ww_magpie[,,]
#'   getCells(NAg_ww) <- paste(lpj_cells_map$ISO,1:67420,sep=".")
#'   NAg_ww           <- as.array(collapseNames(NAg_ww))
#'   rm(NAg_ww_magpie)
#'
#'   # Non-Agricultural Water Consumption (in mio. m^3 / yr) [smoothed]
#'   NAg_wc_magpie           <- calcOutput("WaterUseNonAg", source="WATERGAP2020", selectyears=selectyears, time=time, dof=dof, averaging_range=averaging_range, waterusetype="consumption", seasonality="total", aggregate=FALSE)
#'   getCells(NAg_wc_magpie) <- paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep=".")
#'   NAg_wc           <- new.magpie(1:67420,getYears(NAg_wc_magpie),getNames(NAg_wc_magpie))
#'   NAg_wc[,,]       <- 0
#'   NAg_wc[paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep="."),,] <- NAg_wc_magpie[,,]
#'   getCells(NAg_wc) <- paste(lpj_cells_map$ISO,1:67420,sep=".")
#'   NAg_wc           <- as.array(collapseNames(NAg_wc))
#'   rm(NAg_wc_magpie)
#'
#'   # Harmonize non-agricultural consumption and withdrawals (withdrawals > consumption)
#'   NAg_ww <- pmax(NAg_ww, NAg_wc)
#'   NAg_wc <- pmax(NAg_wc, 0.01*NAg_ww)
#'
#'   # Committed agricultural uses (in mio. m^3 / yr) [for initialization year]
#'   CAU_magpie <- calcOutput("WaterUseCommittedAg",selectyears=selectyears,cells="lpjcell",iniyear=iniyear,irrigini=paste0(unlist(str_split(irrigini, "_"))[[1]],"_lpjcell"),time=time,dof=dof,averaging_range=averaging_range,harmonize_baseline=harmonize_baseline,ref_year=ref_year,aggregate=FALSE)
#'   CAW_magpie <- as.array(collapseNames(dimSums(CAU_magpie[,,"withdrawal"],dim=3)))
#'   CAC_magpie <- as.array(collapseNames(dimSums(CAU_magpie[,,"consumption"],dim=3)))
#'   rm(CAU_magpie)
#'   CAW_magpie <- as.array(collapseNames(CAW_magpie))
#'   CAW_magpie <- CAW_magpie[,,1]
#'   CAC_magpie <- as.array(collapseNames(CAC_magpie))
#'   CAC_magpie <- CAC_magpie[,,1]
#'
#'   ### Required inputs for Allocation Algorithm:
#'   # Required water for full irrigation per cell (in mio. m^3)
#'   required_wat_fullirrig_ww <- calcOutput("FullIrrigationRequirement", version="LPJmL5", selectyears=selectyears, climatetype="HadGEM2_ES:rcp2p6:co2", harmonize_baseline=FALSE, time="spline", dof=4, iniyear=1995, iniarea=TRUE, irrig_requirement="withdrawal", cells="lpjcell", aggregate=FALSE)[,,c("maiz","rapeseed","puls_pro")]
#'   required_wat_fullirrig_ww <- pmax(required_wat_fullirrig_ww,0)
#'   required_wat_fullirrig_wc <- calcOutput("FullIrrigationRequirement", version="LPJmL5", selectyears=selectyears, climatetype="HadGEM2_ES:rcp2p6:co2", harmonize_baseline=FALSE, time="spline", dof=4, iniyear=1995, iniarea=TRUE, irrig_requirement="consumption", cells="lpjcell", aggregate=FALSE)[,,c("maiz","rapeseed","puls_pro")]
#'   required_wat_fullirrig_wc <- pmax(required_wat_fullirrig_wc,0)
#'
#'   # Full irrigation water requirement depending on irrigation system in use
#'   if (irrigationsystem=="initialization") {
#'     # read in irrigation system area initialization [share of AEI by system]
#'     tmp               <- calcOutput("IrrigationSystem", source=irrigini, aggregate=FALSE)
#'     irrigation_system <- new.magpie(getCells(tmp), getYears(required_wat_fullirrig_ww), getNames(tmp))
#'     getYears(irrigation_system) <- getYears(required_wat_fullirrig_ww)
#'     for (y in getYears(required_wat_fullirrig_ww)) {
#'       irrigation_system[,y,] <- tmp
#'     }
#'     # full irrigation water requirements (in mio. m^3)
#'     required_wat_fullirrig_ww   <- dimSums(irrigation_system*required_wat_fullirrig_ww,dim=3.1)
#'     required_wat_fullirrig_wc   <- dimSums(irrigation_system*required_wat_fullirrig_wc,dim=3.1)
#'   } else {
#'     # whole area irrigated by one system as selected in argument "irrigationsystem"
#'     required_wat_fullirrig_ww <- collapseNames(required_wat_fullirrig_ww[,,irrigationsystem])
#'     required_wat_fullirrig_wc <- collapseNames(required_wat_fullirrig_wc[,,irrigationsystem])
#'   }
#'   # average required water for full irrigation across selected proxy crops
#'   required_wat_fullirrig_ww <- dimSums(required_wat_fullirrig_ww,dim=3)/length(getNames(required_wat_fullirrig_ww))
#'   required_wat_fullirrig_wc <- dimSums(required_wat_fullirrig_wc,dim=3)/length(getNames(required_wat_fullirrig_wc))
#'
#'   # transform to array for further calculations
#'   required_wat_fullirrig_ww <- as.array(collapseNames(required_wat_fullirrig_ww))[,,1]
#'   required_wat_fullirrig_wc <- as.array(collapseNames(required_wat_fullirrig_wc))[,,1]
#'
#'   # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
#'   meancellrank <- calcOutput("IrrigCellranking", version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=FALSE, ref_year="y2015",
#'                              cellrankyear=selectyears, cells="lpjcell", crops="magpie", method="meancroprank", proxycrop=c("maiz", "rapeseed", "puls_pro"), iniyear=iniyear, aggregate=FALSE)
#'   meancellrank <- as.array(meancellrank)[,,1]
#'
#'   # Yield gain potential through irrigation of proxy crops
#'   irrig_yieldgainpotential <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, averaging_range=averaging_range, dof=dof,
#'                                 cells="lpjcell", crops="magpie", proxycrop=c("maiz", "rapeseed", "puls_pro"), monetary=thresholdtype, aggregate=FALSE)
#'   irrig_yieldgainpotential <- as.array(irrig_yieldgainpotential)[,,1]
#'
#'   ############################################
#'   ####### Routing and Allocation Loop ########
#'   ############################################
#'   if (class(selectyears)=="numeric") {
#'     selectyears <- paste0("y",selectyears)
#'   }
#'
#'   out_tmp1 <- NULL
#'   out_tmp2 <- NULL
#'   out      <- NULL
#'
#'   for (EFP in c("on", "off")) {
#'
#'     # Environmental Flow Requirements (in mio. m^3 / yr) [long-term average]
#'     EFR_magpie <- calcOutput("EnvmtlFlowRequirements", selectyears=selectyears, version="LPJmL4", climatetype=climatetype, aggregate=FALSE,
#'                              LFR_val=0.1, HFR_LFR_less10=0.2, HFR_LFR_10_20=0.15, HFR_LFR_20_30=0.07, HFR_LFR_more30=0.00,
#'                              EFRyears=c(1980:2010))
#'
#'     if (EFP=="off") {
#'       EFR_magpie[,,] <- 0
#'     }
#'     EFR_magpie <- as.array(collapseNames(EFR_magpie))[,,1]
#'
#'
#'     ## INPUTS:
#'     # required_wat_min (from River Routing)
#'     # discharge (from River Routing)
#'     # meancellrank (calcCellRanking)
#'     # required_wat_fullirrig_ww
#'     # required_wat_fullirrig_wc
#'     # irrig_yield_gain_potential (calcYiledGainPotential)
#'
#'
#'     for (scen in getNames(NAg_ww)){
#'       for (y in selectyears){
#'
#'         ################################################
#'         ####### River basin discharge allocation #######
#'         ################################################
#'         # Minimum water required
#'         required_wat_min_allocation <- required_wat_min
#'
#'         ### REPORTING: discharge before algorithm is executed
#'         discharge_befalgo <- discharge
#'
#'         # Allocate water for full irrigation to cell with highest yield improvement through irrigation
#'         if (allocationrule=="optimization") {
#'
#'           for (c in (1:max(meancellrank[,y],na.rm=T))){
#'             # available water for additional irrigation withdrawals
#'             avl_wat_ww <- max(discharge[c]-required_wat_min_allocation[c],0)
#'
#'             # withdrawal constraint
#'             if (required_wat_fullirrig_ww[c,y]>0) {
#'               # how much withdrawals can be fulfilled by available water
#'               frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)
#'
#'               # consumption constraint
#'               if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
#'                 # available water for additional irrigation consumption (considering downstream availability)
#'                 avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]]] - required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
#'                 # how much consumption can be fulfilled by available water
#'                 frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
#'               }
#'
#'               # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
#'               discharge[c(rs$downstreamcells[[c]],c)] <- discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
#'               # update minimum water required in cell:
#'               required_wat_min_allocation[c] <- required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
#'             }
#'           }
#'           ### REPORTING: discharge after optimization
#'           discharge_optimization <- discharge
#'
#'         } else if (allocationrule=="upstreamfirst") {
#'           # Allocate full irrigation requirements to most upstream cell first (calcorder)
#'
#'           for (o in 1:max(rs$calcorder)){
#'             cells <- which(rs$calcorder==o)
#'
#'             for (c in cells){
#'
#'               # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
#'               if (irrig_yieldgainpotential[c,y] > gainthreshold) {
#'                 # available water for additional irrigation withdrawals
#'                 avl_wat_ww <- max(discharge[c]-required_wat_min_allocation[c],0)
#'
#'                 # withdrawal constraint
#'                 if (required_wat_fullirrig_ww[c,y]>0) {
#'                   # how much withdrawals can be fulfilled by available water
#'                   frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)
#'
#'                   # consumption constraint
#'                   if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
#'                     # available water for additional irrigation consumption (considering downstream availability)
#'                     avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]]] - required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
#'                     # how much consumption can be fulfilled by available water
#'                     frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
#'                   }
#'
#'                   # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
#'                   discharge[c(rs$downstreamcells[[c]],c)] <- discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
#'                   # update minimum water required in cell:
#'                   required_wat_min_allocation[c] <- required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
#'                 }
#'               }
#'             }
#'           }
#'           ### REPORTING: discharge after optimization
#'           discharge_upstreamfirst <- discharge
#'
#'         } else if (allocationrule=="equality") {
#'
#'           ## while loop?
#'           ## 1 run with meancellrank; identify list of cells that still can get water (avl wat ww > 0, downstreamcell consumption); run through these again by order of meancellrank
#'
#'           for (c in (1:max(meancellrank[,y],na.rm=T))){
#'             # available water for additional irrigation withdrawals
#'             avl_wat_ww <- max(discharge[c]-required_wat_min_allocation[c],0)
#'
#'             # withdrawal constraint
#'             if (required_wat_fullirrig_ww[c,y]>0) {
#'               # how much withdrawals can be fulfilled by available water
#'               frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)
#'
#'               # consumption constraint
#'               if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
#'                 # available water for additional irrigation consumption (considering downstream availability)
#'                 avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]]] - required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
#'                 # how much consumption can be fulfilled by available water
#'                 frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
#'               }
#'
#'               # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
#'               discharge[c(rs$downstreamcells[[c]],c)] <- discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
#'               # update minimum water required in cell:
#'               required_wat_min_allocation[c] <- required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
#'
#'             }
#'           }
#'
#'           # Repeat optimization algorithm several times
#'           # Instead of full irrigation, only up to x% (e.g.20%) are allocated to most efficient cell
#'           # Repeat until all river basin discharge is allocated ---> STILL MISSING
#'
#'           for (c in (1:max(meancellrank[,y],na.rm=T))){
#'             # available water for additional irrigation withdrawals
#'             avl_wat_ww <- max(discharge[c]-required_wat_min_allocation[c],0)
#'
#'             # withdrawal constraint
#'             if (required_wat_fullirrig_ww[c,y]>0) {
#'               # how much withdrawals can be fulfilled by available water
#'               frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)
#'
#'               # consumption constraint
#'               if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
#'                 # available water for additional irrigation consumption (considering downstream availability)
#'                 avl_wat_wc          <- max(min(discharge[rs$downstreamcells[[c]]] - required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
#'                 # how much consumption can be fulfilled by available water
#'                 frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
#'               }
#'
#'               frac_fullirrig[c] <- frac_fullirrig[c]*allocationshare
#'
#'               # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
#'               discharge[c(rs$downstreamcells[[c]],c)] <- discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
#'               # update minimum water required in cell:
#'               required_wat_min_allocation[c] <- required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
#'             }
#'           }
#'         } else {
#'           stop("Please choose allocation rule for river basin discharge allocation algorithm")
#'         }
#'
#'         #################
#'         #### OUTPUTS ####
#'         #################
#'         ### MAIN OUTPUT VARIABLE: water available for irrigation (consumptive agricultural use)
#'         wat_avl_irrig_c <- CAC_magpie[,y]*frac_CAg_fulfilled + frac_fullirrig*required_wat_fullirrig_wc[,y]
#'         wat_avl_irrig_w <- CAW_magpie[,y]*frac_CAg_fulfilled + frac_fullirrig*required_wat_fullirrig_ww[,y]
#'         wat_avl_total_human_c <- CAC_magpie[,y]*frac_CAg_fulfilled + frac_fullirrig*required_wat_fullirrig_wc[,y] + frac_NAg_fulfilled*NAg_wc[,y,scen]
#'         wat_avl_total_human_w <- CAW_magpie[,y]*frac_CAg_fulfilled + frac_fullirrig*required_wat_fullirrig_ww[,y] + frac_NAg_fulfilled*NAg_ww[,y,scen]
#'         wat_avl_nonAg_c <- frac_NAg_fulfilled*NAg_wc[,y,scen]
#'         wat_avl_nonAg_w <- frac_NAg_fulfilled*NAg_ww[,y,scen]
#'         wat_avl_commAg_c <- CAC_magpie[,y]*frac_CAg_fulfilled
#'         wat_avl_commAg_w <- CAW_magpie[,y]*frac_CAg_fulfilled
#'
#'         if (output=="consumption") {
#'           # Main output for MAgPIE: water available for agricultural consumption
#'           wat_avl_irrig <- wat_avl_irrig_c
#'           dataname <- "wat_avl_irrig_c"
#'           description="Available water for irrigation consumption per year"
#'         } else if (output=="withdrawal") {
#'           # Main output for MAgPIE: water available for agricultural withdrawal
#'           wat_avl_irrig <- wat_avl_irrig_w
#'           dataname <- "wat_avl_irrig_w"
#'           description="Available water for irrigation withdrawals per year"
#'
#'         ### Reporting outputs
#'         } else if (output=="total_consumption") {
#'           # Total human water consumption
#'           wat_avl_irrig <- wat_avl_total_human_c
#'           dataname <- "wat_avl_total_human_c"
#'           description="Total human water consumption per year"
#'         } else if (output=="total_withdrawal") {
#'           # Total human water withdrawal
#'           wat_avl_irrig <- wat_avl_total_human_w
#'           dataname <- "wat_avl_total_human_w"
#'           description="Total human water withdrawal per year"
#'         } else if (output=="nonag_consumption") {
#'           # Non-agricultural water consumption
#'           if (any(water_use_nonag_wc!=wat_avl_nonAg_c)) stop("This shouldn't be the case. Check the function again.")
#'           wat_avl_irrig <- wat_avl_nonAg_c
#'           dataname <- "wat_avl_nonAg_c"
#'           description="Non-agricultural water consumption per year"
#'         } else if (output=="nonag_withdrawal") {
#'           # Non-agricultural water withdrawals
#'           if (any(water_use_nonag_ww!=wat_avl_nonAg_w)) stop("This shouldn't be the case. Check the function again.")
#'           wat_avl_irrig <- wat_avl_nonAg_w
#'           dataname <- "wat_avl_nonAg_w"
#'           description="Non-agricultural water withdrawals per year"
#'
#'         ### Intermediate outputs
#'         } else if (output=="discharge_before") {
#'           # Discharge after river routing, before allocation algorithm
#'           wat_avl_irrig <- discharge_befalgo
#'           dataname <- "discharge_befalgo"
#'           description="Cellular discharge before allocation algorithm executed"
#'         } else if (output=="discharge_opt") {
#'           # Discharge after optimization allocation algorithm
#'           wat_avl_irrig <- discharge_optimization
#'           dataname <- "discharge_optimization"
#'           description="Cellular discharge after optimization allocation algorithm executed"
#'         } else if (output=="discharge_up") {
#'           # Discharge after upstreamfirst allocation algorithm
#'           wat_avl_irrig <- discharge_upstreamfirst
#'           dataname <- "discharge_upstreamfirst"
#'           description="Cellular discharge after upstreamfirst allocation algorithm executed"
#'         } else {
#'           stop("specify type of water availability output: withdrawal or consumption")
#'         }
#'
#'         wat_avl_irrig <- setNames(setYears(as.magpie(wat_avl_irrig,spatial=1),y), dataname)
#'         wat_avl_irrig <- add_dimension(wat_avl_irrig, dim=3.1, add="nonag_scen", nm=scen)
#'         wat_avl_irrig <- add_dimension(wat_avl_irrig, dim=3.1, add="EFP", nm=EFP)
#'         out_tmp1      <- mbind(out_tmp1, wat_avl_irrig)
#'       }
#'       out_tmp2 <- mbind(out_tmp2, out_tmp1)
#'       out_tmp1 <- NULL
#'     }
#'     out      <- mbind(out, out_tmp2)
#'     out_tmp2 <- NULL
#'   }
#'
#'
#'   ## OUTPUTS:
#'   # discharge
#'   # avl_water
#'   # frac_fullirrig
#'
#'   ### Correct number of cells
#'   if (finalcells=="lpjcell"){
#'     out <- out
#'   } else if (finalcells=="magpiecell"){
#'     out <- out[magclassdata$cellbelongings$LPJ_input.Index,,]
#'     out <- toolCell2isoCell(out)
#'   } else {
#'     stop("Cell argument not supported. Select lpjcell for 67420 cells or magpiecell for 59199 cells")
#'   }
#'
#'   return(list(
#'     x=out,
#'     weight=NULL,
#'     unit="mio. m^3",
#'     description=description,
#'     isocountries=FALSE))
#' }
