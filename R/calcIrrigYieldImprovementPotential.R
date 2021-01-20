#' @title calcIrrigYieldImprovementPotential
#' @description This function calculates the yield improvement potential of irrigation for different crops
#'
#' @param climatetype switch between different climate scenarios (default: "CRU_4") of calcYields function
#' @param selectyears years to be returned by the function
#' @param time            time smoothing of calcYields function: average, spline (default) or raw
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param monetary        yield improvement potential in tDM (FALSE, default) or priced yield improvement potential in USD05 (TRUE)
#' @param iniyear         year to be used when monetary activated
#' @param harmonize_baseline harmonization in calcYields function: FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year onwards)
#' @param ref_year           reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param cells       switch between "lpjcell" (67420) and "magpiecell" (59199)
#' @param crops       switch between "magpie" and "lpjml" (default) crops
#' @param proxycrop   proxycrop(s) selected for crop mix specific calculations (default: "all")
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigYieldImprovementPotential", aggregate=FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import mrmagpie

calcIrrigYieldImprovementPotential <- function(climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, monetary=FALSE, iniyear=1995,
                                          harmonize_baseline=FALSE, ref_year=NULL, selectyears=seq(1995, 2095,by=5), cells="magpiecell", crops="lpjml", proxycrop="all"){

  # read in yields [in tons/ha]
  yields <- calcOutput("Yields", lpjml=c(natveg="LPJml4", crop="LPJmL5"), climatetype=climatetype, selectyears=selectyears, crops=crops,
                       time=time, dof=dof, averaging_range=averaging_range, harmonize_baseline=harmonize_baseline, ref_year=ref_year, aggregate=FALSE)

  # yield gap (irrigated vs. rainfed) [in tons/ha]
  tmp    <- collapseNames(yields[,,"irrigated"])-collapseNames(yields[,,"rainfed"])
  # (Note: irrigation may lead to shift in growing period -> tmp can have negative values; also: under N-stress, irrigation may lead to lower yields, the latter is only relevant for limited-N-LPJmL version, default: unlimited N)

  # cellular dimension
  if (cells=="magpiecell") {
    yield_gain <- tmp
  } else if (cells=="lpjcell") {
    lpj_cells_map  <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell")
    getCells(tmp)  <- paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep=".")
    yield_gain     <- new.magpie(1:67420,getYears(tmp),getNames(tmp))
    yield_gain[,,] <- 0
    yield_gain[paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep="."),,] <- tmp[,,]
    getCells(yield_gain) <- paste(lpj_cells_map$ISO,1:67420,sep=".")
  } else {
    stop("Cells argument not supported. Please select lpjcell for 67420 cells or magpiecell for 59199 cells")
  }

  if (monetary){
    # Read in crop output price in initialization (USD05/tDM)
    p <- calcOutput("IniFoodPrice", datasource="FAO", years=NULL, aggregate=FALSE, year=iniyear)

    if (crops=="lpjml"){
      # map prices to LPJmL crops
      LPJ2MAG <- toolGetMapping( "MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")
      LPJ2MAG <- data.frame(magpiecrops=LPJ2MAG$MAgPIE, lpjmlcrops=LPJ2MAG$LPJmL, lpjml5crops=LPJ2MAG$LPJmL5)
      # delete duplicates
      LPJ2MAG <- LPJ2MAG[!(LPJ2MAG$magpiecrops=="oilpalm"   & LPJ2MAG$lpjmlcrops=="groundnut"),]
      LPJ2MAG <- LPJ2MAG[!(LPJ2MAG$magpiecrops=="cottn_pro" & LPJ2MAG$lpjmlcrops=="groundnut"),]
      LPJ2MAG <- LPJ2MAG[!(LPJ2MAG$magpiecrops=="others" & LPJ2MAG$lpjmlcrops=="maize"),]
      LPJ2MAG <- LPJ2MAG[!(LPJ2MAG$magpiecrops=="foddr"  & LPJ2MAG$lpjmlcrops=="maize"),]
      LPJ2MAG <- LPJ2MAG[!(LPJ2MAG$magpiecrops=="potato" & LPJ2MAG$lpjmlcrops=="tero"),]

      p <- as.data.frame(p)
      p <- data.frame(magpiecrops=p$Data1, price=p$Value)
      p <- merge(LPJ2MAG, p, "magpiecrops")
      p <- data.frame(data=p$lpjmlcrops, Value=p$price)
      p <- as.magpie(p)
    }

    # Calculate monetary yield gain (in USD05/ha)
    yield_gain  <- yield_gain[,,intersect(getNames(yield_gain), getNames(p))] * p[,,intersect(getNames(yield_gain), getNames(p))]
    unit        <- "USD05 per ha"
  } else {
    unit        <- "tons per ha"
  }

  # Selected crops
  if (proxycrop=="all") {
    description <- "Yield improvement potential by irrigation for different crop types"
  } else {
    # select proxy crops
    yield_gain  <- yield_gain[,,proxycrop]
    # average over proxy crops
    yield_gain  <- dimSums(yield_gain,dim=3)/length(getNames(yield_gain))
    description <- "Average yield improvement potential for selection of crop types"
  }

  # Check for NAs
  if(any(is.na(yield_gain))){
    stop("Function YieldImprovementPotential produced NAs")
  }

  return(list(
    x=yield_gain,
    weight=NULL,
    unit=unit,
    description=description,
    isocountries=FALSE))
}
