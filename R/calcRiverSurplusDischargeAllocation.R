#' @title       calcRiverSurplusDischargeAllocation
#' @description This function distributes surplus basin discharge after the previous river routings following certain management assumptions
#'
#' @param selectyears Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param output output to be reported
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod   EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param variabilitythreshold Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod      method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype   Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear          Initialization year of irrigation system
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param proxycrop        historical crop mix pattern ("historical") or list of proxycrop(s)
#' @param FAOyieldcalib TRUE (LPJmL yields scaled with current FAO yield) or FALSE (LPJmL yield potentials)
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

calcRiverSurplusDischargeAllocation <- function(selectyears, output, climatetype, EFRmethod, variabilitythreshold, rankmethod, allocationrule, thresholdtype, gainthreshold, irrigationsystem, iniyear, avlland_scen, proxycrop, FAOyieldcalib) {

  # Check
  if (!is.na(as.list(strsplit(avlland_scen, split=":"))[[1]][2]) && iniyear != as.numeric(as.list(strsplit(avlland_scen, split=":"))[[1]][2])) stop("Initialization year in calcRiverSurplusDischargeAllocation does not match: iniyear and avlland_scen should have same initialization year")

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River Structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))
  # numeric cell numbers in order of rs object
  rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

  # Inputs from previous river routing
  tmp <- calcOutput("RiverHumanUses", humanuse="committed_agriculture", climatetype=climatetype, EFRmethod=EFRmethod, selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)
  # Minimum flow requirements: Environmental Flow Requirements + Reserved for Non-Agricultural Uses + Reserved Committed Agricultural Uses (in mio. m^3 / yr)
  required_wat_min_allocation <- collapseNames(tmp[,,"required_wat_min"])
  # Already committed water withdrawals
  com_ww                      <- collapseNames(tmp[,,"currHuman_ww"])

  # Discharge determined by previous river routings (in mio. m^3 / yr)
  discharge                   <- calcOutput("RiverDischargeNatAndHuman", selectyears=selectyears, iniyear=iniyear, climatetype=climatetype, EFRmethod=EFRmethod, aggregate=FALSE)

  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig      <- calcOutput("FullIrrigationRequirement", selectyears=selectyears, climatetype=climatetype, comagyear=iniyear, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, aggregate=FALSE)
  required_wat_fullirrig_ww   <- pmax(collapseNames(required_wat_fullirrig[,,"withdrawal"]), 0)
  required_wat_fullirrig_wc   <- pmax(collapseNames(required_wat_fullirrig[,,"consumption"]), 0)

  # Yield gain potential through irrigation of proxy crops
  irrig_yieldgainpotential    <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears, proxycrop=proxycrop, monetary=thresholdtype, iniyear=iniyear, FAOyieldcalib=FAOyieldcalib, aggregate=FALSE)

  # Discharge that is inaccessible to human uses (mio m^3)
  inaccessible_discharge      <- calcOutput("DischargeInaccessibile", selectyears=selectyears, climatetype=climatetype, variabilitythreshold=variabilitythreshold, aggregate=FALSE)

  if (allocationrule=="optimization") {
    # Retrieve arguments
    fullpotential <- as.logical(strsplit(rankmethod, ":")[[1]][2])

    # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
    glocellrank  <- calcOutput("IrrigCellranking", climatetype=climatetype, cellrankyear=selectyears, method=rankmethod, proxycrop=proxycrop, iniyear=iniyear, FAOyieldcalib=FAOyieldcalib, aggregate=FALSE)
    glocellrank  <- as.array(glocellrank)[,,1]

    # Share of full irrigation water requirements to be allocated for each round of the allocation algorithm
    allocationshare           <- 1 / (length(glocellrank[,1])/67420)
    required_wat_fullirrig_ww <- required_wat_fullirrig_ww * allocationshare
    required_wat_fullirrig_wc <- required_wat_fullirrig_wc * allocationshare
  }

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
  com_ww                      <- as.array(com_ww)
  required_wat_min_allocation <- as.array(required_wat_min_allocation)
  required_wat_fullirrig_ww   <- as.array(.transformObject(required_wat_fullirrig_ww))
  required_wat_fullirrig_wc   <- as.array(.transformObject(required_wat_fullirrig_wc))
  irrig_yieldgainpotential    <- as.array(.transformObject(irrig_yieldgainpotential))
  frac_fullirrig              <- as.array(.transformObject(0))
  avl_wat_ww                  <- as.array(.transformObject(0))
  avl_wat_wc                  <- as.array(.transformObject(0))
  inaccessible_discharge      <- as.array(.transformObject(inaccessible_discharge))

  ################################################
  ####### River basin discharge allocation #######
  ################################################
  out <- NULL
  if (class(selectyears)=="numeric") {
    selectyears <- paste0("y", selectyears)
  }

  for (y in selectyears) {

    # Allocate water for full irrigation to cell with highest yield improvement through irrigation
    if (allocationrule=="optimization") {

      for (o in (1:max(glocellrank[,y], na.rm=T))) {

        # Extract the cell number (depending on type of cellranking)
        if (!fullpotential) {
          c <- rs$cells[rs$coordinates==paste(strsplit(gsub(".*_", "", names(which(glocellrank[,y]==o))), "\\.")[[1]][1], strsplit(gsub(".*_", "", names(which(glocellrank[,y]==o))), "\\.")[[1]][2], sep=".")]
        } else {
          c <- rs$cells[glocellrank[,y]==o]
        }

        # vector of downstreamcells of c
        down <- unlist(rs$downstreamcells[[c]])
        # vector of c in length of downstreamcells of c
        lc   <- rep(c, length(rs$downstreamcells[[c]]))
        # vector of 1s in length of downstreamcells of c
        cc   <- rep(1:length(c), length(rs$downstreamcells[[c]]))

        # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
        irriggain <- (irrig_yieldgainpotential[c,y,,drop=F] > gainthreshold)

        # available water for additional irrigation withdrawals
        # (Avl. water = Discharge - Inaccessible Discharge - Previously Committed for Human Consumption)
        # (Inaccessible Discharge is either reserved for the environment: EFRs (required_wat_min_allocation - com_ww) or inaccessible due to variability (inaccessible_discharge) needs to be reserved)
        avl_wat_ww[c,y,][irriggain[,,,drop=F]] <- pmax(IO_discharge[c,y,,drop=F] - pmax(required_wat_min_allocation[c,y,,drop=F] - com_ww[c,y,,drop=F], inaccessible_discharge[c,y,,drop=F]) - com_ww[c,y,,drop=F], 0)[irriggain[,,,drop=F]]

        # withdrawal constraint
        ww_constraint   <- (required_wat_fullirrig_ww[c,y,,drop=F]>0 & irriggain[,,,drop=F])

        # how much withdrawals can be fulfilled by available water
        frac_fullirrig[c,y,][ww_constraint[,,,drop=F]] <- pmin(avl_wat_ww[c,y,,drop=F][ww_constraint[,,,drop=F]] / required_wat_fullirrig_ww[c,y,,drop=F][ww_constraint[,,,drop=F]], 1)

        if (length(down)>0) {
          # consumption constraint
          wc_constraint <- (required_wat_fullirrig_wc[c,y,,drop=F]>0 & ww_constraint[,,,drop=F])

          # available water for additional irrigation consumption (considering downstream availability)
          # (downstream availability is constrained by EFRs and inaccessible discharge just as local withdrawal constraint above)
          avl_wat_wc[c,y,][wc_constraint[,,,drop=F]]     <- pmax(apply((IO_discharge[down,y,,drop=F] - pmax(required_wat_min_allocation[down,y,,drop=F] - com_ww[down,y,,drop=F], inaccessible_discharge[down,y,,drop=F]) - com_ww[down,y,,drop=F]), MARGIN=3, min), 0)[wc_constraint[,,,drop=F]]

          # how much consumption can be fulfilled by available water
          frac_fullirrig[c,y,][wc_constraint[,,,drop=F]] <- pmin(avl_wat_wc[c,y,,drop=F][wc_constraint[,,,drop=F]] / required_wat_fullirrig_wc[c,y,,drop=F][wc_constraint[,,,drop=F]], frac_fullirrig[c,y,,drop=F][wc_constraint[,,,drop=F]])
        }

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        IO_discharge[c(down,c),y,][ww_constraint[c(cc,1),,,drop=F]] <- (IO_discharge[c(down,c),y,,drop=F] - required_wat_fullirrig_wc[c(lc,c),y,,drop=F] * frac_fullirrig[c(lc,c),y,,drop=F])[ww_constraint[c(cc,1),,,drop=F]]
        # update minimum water required and previously committed water withdrawal in cell:
        required_wat_min_allocation[c,y,][ww_constraint[,,,drop=F]] <- (required_wat_min_allocation[c,y,,drop=F] + frac_fullirrig[c,y,,drop=F] * required_wat_fullirrig_ww[c,y,,drop=F])[ww_constraint[,,,drop=F]]
        com_ww[c,y,][ww_constraint[,,,drop=F]]                      <- (com_ww[c,y,,drop=F] + frac_fullirrig[c,y,,drop=F] * required_wat_fullirrig_ww[c,y,,drop=F])[ww_constraint[,,,drop=F]]
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
          # (Avl. water = Discharge - Inaccessible Discharge - Previously Committed for Human Consumption)
          # (Inaccessible Discharge is either reserved for the environment: EFRs (required_wat_min_allocation - com_ww) or inaccessible due to variability (inaccessible_discharge) needs to be reserved)
          avl_wat_ww[c,y,][irriggain[,,,drop=F]] <- pmax(IO_discharge[c,y,,drop=F] - pmax(required_wat_min_allocation[c,y,,drop=F] - com_ww[c,y,,drop=F], inaccessible_discharge[c,y,,drop=F]) - com_ww[c,y,,drop=F], 0)[irriggain[,,,drop=F]]

          # withdrawal constraint
          ww_constraint <- (required_wat_fullirrig_ww[c,y,,drop=F]>0 & irriggain[,,,drop=F])

          # how much withdrawals can be fulfilled by available water
          frac_fullirrig[c,y,][ww_constraint[,,,drop=F]] <- pmin(avl_wat_ww[c,y,,drop=F][ww_constraint[,,,drop=F]] / required_wat_fullirrig_ww[c,y,,drop=F][ww_constraint[,,,drop=F]], 1)

          if (length(down)>0) {
            # consumption constraint
            wc_constraint <- (required_wat_fullirrig_wc[c,y,,drop=F]>0 & ww_constraint[,,,drop=F])

            # available water for additional irrigation consumption (considering downstream availability)
            # (downstream availability is constrained by EFRs and inaccessible discharge just as local withdrawal constraint above)
            avl_wat_wc[c,y,][wc_constraint[,,,drop=F]]     <- pmax(apply((IO_discharge[down,y,,drop=F] - pmax(required_wat_min_allocation[down,y,,drop=F] - com_ww[down,y,,drop=F], inaccessible_discharge[down,y,,drop=F]) - com_ww[down,y,,drop=F]), MARGIN=3, min), 0)[wc_constraint[,,,drop=F]]

            # how much consumption can be fulfilled by available water
            frac_fullirrig[c,y,][wc_constraint[,,,drop=F]] <- pmin(avl_wat_wc[c,y,,drop=F][wc_constraint[,,,drop=F]] / required_wat_fullirrig_wc[c,y,,drop=F][wc_constraint[,,,drop=F]], frac_fullirrig[c,y,,drop=F][wc_constraint[,,,drop=F]])
          }

          # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
          IO_discharge[c(down,c),y,][ww_constraint[c(cc,1),,,drop=F]] <- (IO_discharge[c(down,c),y,,drop=F] - required_wat_fullirrig_wc[c(lc,c),y,,drop=F] * frac_fullirrig[c(lc,c),y,,drop=F])[ww_constraint[c(cc,1),,,drop=F]]
          # update minimum water required in cell:
          required_wat_min_allocation[c,y,][ww_constraint[,,,drop=F]] <- (required_wat_min_allocation[c,y,,drop=F] + frac_fullirrig[c,y,,drop=F] * required_wat_fullirrig_ww[c,y,,drop=F])[ww_constraint[,,,drop=F]]
          com_ww[c,y,][ww_constraint[,,,drop=F]]                      <- (com_ww[c,y,,drop=F] + frac_fullirrig[c,y,,drop=F] * required_wat_fullirrig_ww[c,y,,drop=F])[ww_constraint[,,,drop=F]]
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
