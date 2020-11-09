#' @title calcYields
#' @description This function extracts yields from LPJmL to MAgPIE
#'
#' @param version Switch between LPJmL4 and LPJmL5
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param harmonize_baseline FALSE (default) nothing happens, if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year just specify for harmonize_baseline != FALSE : Reference year
#' @param time spline or average, if spline specify dof, if average specify averaging range
#' @param dof degrees of freedom for spline
#' @param isimip_subtype isimip 3b yields subtype
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("YieldsReplaceISIMIP", aggregate = FALSE) }
#'

calcYieldsReplaceISIMIP <- function(version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", dof=4,
                       harmonize_baseline="CRU_4", ref_year="y2015",
                       isimip_subtype="ISIMIP3b:yields.EPIC-IIASA_ukesm1-0-ll_ssp585_default"){

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit=1e+12)
  on.exit(options(magclass_sizeLimit=sizelimit))

  yields <- calcOutput("Yields", version=version, climatetype=climatetype, time=time,
             harmonize_baseline=harmonize_baseline, ref_year=ref_year, aggregate = FALSE)

  to_rep <- calcOutput("ISIMIPYields", time=time, subtype=isimip_subtype, aggregate=FALSE)
gc()
  common_yrs <- intersect(getYears(yields),getYears(to_rep))
  common_vars <- intersect(getNames(yields),getNames(to_rep))
  yields <- as.array(yields); to_rep <- as.array(to_rep)
  yields[,common_yrs,common_vars] <- to_rep[,common_yrs,common_vars]
  yields <- as.magpie(yields); to_rep <- as.magpie(to_rep)


  crop_area_weight     <- yields
  crop_area_weight[,,] <- 1

  return(list(
    x=yields,
    weight=crop_area_weight,
    unit="t per ha",
    description="Yields in tons per hectar for different crop types.",
    isocountries=FALSE))
}
