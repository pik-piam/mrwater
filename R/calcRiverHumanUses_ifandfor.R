#' @title calcRiverHumanUses_ifandfor
#' @description This function calculates natural discharge for the river routing derived from inputs from LPJmL
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param subtype     Subtype to be returned: discharge or required_wat_min or frac_fulfilled
#' @param humanuse    Human use type to which river routing shall be applied (non_agriculture or committed_agriculture). Note: non_agriculture must be run prior to committed_agriculture
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param conservationstatus Conservation status or management objective according to Smakthin EFR method: "fair", "good", "natural". Details: The strictness of the conservation status affects the LFRs (low flow requirements, baseflow that needs to be maintained in the river)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums add_dimension
#' @importFrom stringr str_split
#' @import mrmagpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("RiverHumanUses_ifandfor", aggregate = FALSE) }
#'

calcRiverHumanUses_ifandfor <- function(selectyears, humanuse, subtype, climatetype, conservationstatus) {
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

  ### Required inputs for River Routing:
  yearly_runoff <- collapseNames(calcOutput("YearlyRunoff", aggregate=FALSE, selectyears=selectyears, climatetype=climatetype))

  # Non-Agricultural Water Withdrawals and Consumption (in mio. m^3 / yr) [smoothed]
  # Non-Agricultural Water Withdrawals and Consumption (in mio. m^3 / yr) [smoothed]
  wat_nonag <- calcOutput("WaterUseNonAg", source="WATERGAP2020", seasonality="total", aggregate=FALSE, selectyears=selectyears, climatetype=NULL, lpjml=NULL, harmon_base_time="average")
  I_NAg_ww  <- collapseNames(wat_nonag[,,"withdrawal"])
  I_NAg_wc  <- collapseNames(wat_nonag[,,"consumption"])

  # Lake evaporation as calculated by natural flow river routing
  lake_evap_new <- collapseNames(calcOutput("RiverNaturalFlows", selectyears=selectyears, climatetype=climatetype, aggregate=FALSE)[,,"lake_evap_nat"])

  # bring all inputs to correct object size and transform to array for faster calculation
  lake_evap_new <- as.array(lake_evap_new)[,,1]
  NAg_wc        <- as.array(I_NAg_wc)
  NAg_ww        <- as.array(I_NAg_ww)
  yearly_runoff <- as.array(yearly_runoff)[,,1]


  ####################################################
  ###### River Routing considering Human Uses ########
  ####################################################

  if (class(selectyears)=="numeric") {
    selectyears <- paste0("y",selectyears)
  }

  out_tmp1 <- NULL
  out_tmp2 <- NULL
  out      <- NULL

  for (EFP in c("on", "off")) {

    required_wat_min <- calcOutput("EnvmtlFlowRequirements", selectyears=selectyears, climatetype=climatetype, conservationstatus, aggregate=FALSE)
    required_wat_min <- as.array(required_wat_min)[,,1]

    if (EFP=="off"){
      required_wat_min[,] <- 0
    }

    for (scen in getNames(NAg_ww)) {
      for (y in selectyears) {

        # Discharge considering human uses
        discharge   <- array(data=0,dim=67420)
        inflow      <- array(data=0,dim=67420)
        avl_wat_act <- array(data=0,dim=67420)
        # Water fractions reserved for certain uses
        frac_NAg_fulfilled <- array(data=0,dim=67420)

        ### River Routing 2: Non-agricultural uses considering local EFRs ###
        for (o in 1:max(rs$calcorder)) {
          # Note: the calcorder ensures that the upstreamcells are calculated first
          cells <- which(rs$calcorder==o)

          for (c in cells){

          #  if (c==2806) stop("check loop from here")

            # available water in cell
            avl_wat_act[c]  <- inflow[c]+yearly_runoff[c,y]-lake_evap_new[c,y]

            # available water in cell not sufficient to fulfill requirements
            # -> no more water can be withdrawn
            if (avl_wat_act[c]<required_wat_min[c,y]) {
              # if cell has upstreamcells: upstreamcells must release missing water (cannot be consumed upstream)
              # -> reduce non-agricultural water consumption in upstream cells
              # -> locally: cannot withdraw
              if (length(rs$upstreamcells[[c]])>0) {
                # upstream non-agricultural water consumption
                upstream_cons <- sum(NAg_wc[rs$upstreamcells[[c]],y,scen]*frac_NAg_fulfilled[rs$upstreamcells[[c]]])
                if (upstream_cons>required_wat_min[c,y]-avl_wat_act[c]) {
                  # if missing water (difference) can be fulfilled by upstream consumption: reduce upstream consumption
                  frac_NAg_fulfilled[rs$upstreamcells[[c]]] <- (1-(required_wat_min[c,y]-avl_wat_act[c])/upstream_cons)*frac_NAg_fulfilled[rs$upstreamcells[[c]]]
                  discharge[c] <- required_wat_min[c,y]
                } else {
                  # if missing water (difference) cannot be fulfilled by upstream consumption: no upstream consumption
                  frac_NAg_fulfilled[rs$upstreamcells[[c]]] <- 0
                  discharge[c] <- avl_wat_act[c]+upstream_cons
                }
              }

              # available water not sufficient & no upstream cells: no withdrawals take place
              discharge[c]          <- avl_wat_act[c]

              # available water in cell is sufficient to fulfill requirements
              # -> further withdrawals are possible
            } else {
              # Non-agricultural withdrawals
              if (NAg_ww[c,y,scen]>0) {
                ## Water withdrawal constraint:
                frac_NAg_fulfilled[c] <- min((avl_wat_act[c]-required_wat_min[c,y])/NAg_ww[c,y,scen], 1)
              }

              ## Outflow from one cell to the next
              # (Subtract local water consumption in current cell (non-ag. consumption))
              discharge[c] <- avl_wat_act[c] - NAg_wc[c,y,scen]*frac_NAg_fulfilled[c]
            }

            if (rs$nextcell[c]>0) {
              inflow[rs$nextcell[c]] <- inflow[rs$nextcell[c]] + discharge[c]
            }
          }
        }

        # Update minimum water required in cell:
        required_wat_min[,y] <- required_wat_min[,y] + NAg_ww[,y,scen]*frac_NAg_fulfilled


      if (subtype=="required_wat_min") {
        wat_avl_irrig <- required_wat_min[,y]
        dataname <- "required_wat_min"
        description="required_wat_min"
      } else if (subtype=="frac_fulfilled") {
        wat_avl_irrig <- frac_NAg_fulfilled
        dataname <- "frac_NAg_fulfilled"
        description="frac_NAg_fulfilled"
      } else {
        stop("Please specify subtype that should be returned by this function: discharge, required_wat_min or frac_fulfilled")
      }

      wat_avl_irrig <- setNames(setYears(as.magpie(wat_avl_irrig, spatial=1),y), dataname)
      getCells(wat_avl_irrig) <- rs$cells
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
    description="test output",
    isocountries=FALSE))
}
