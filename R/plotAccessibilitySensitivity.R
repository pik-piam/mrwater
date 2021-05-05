#' @title       plotAccessibilitySensitivity
#' @description plot of irrigatable area supply curve
#'
#' @param x_axis_range     range of x-axis (gainthreshold) to be depicted on the curve
#' @param output           output type to be displayed: irrigated area "area" or available water volume "wat_ag_ww" "wat_ag_wc" "wat_tot_ww" "wat_tot_wc"
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param FAOyieldcalib    TRUE (LPJmL yields scaled with current FAO yield) or FALSE (LPJmL yield potentials)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param proxycrop        proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotAccessibilitySensitivity(x_axis_range=c(0,1000), scenario="on.ssp2") }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums
#' @importFrom ggplot2 ggplot geom_line geom_point aes_string ggtitle xlab ylab theme_bw

plotAccessibilitySensitivity <- function(x_axis_range, scenario, output, lpjml, selectyears, climatetype, EFRmethod, gainthreshold, rankmethod, FAOyieldcalib, allocationrule, thresholdtype, irrigationsystem, avlland_scen, proxycrop, potential_wat=TRUE) {

  if (length(selectyears)>1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  iniyear      <- as.numeric(as.list(strsplit(avlland_scen, split=":"))[[1]][2])

  if (output=="area") {
    x <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, selectyears=selectyears, climatetype=climatetype, variabilitythreshold=0, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=potential_wat, aggregate=FALSE)[,,"irrigatable"])
    description <- "Irrigatable Area"
    unit        <- "Irrigatable Area (Mha)"
  } else {
    x <- collapseNames(calcOutput("WaterPotUse",     lpjml=lpjml, selectyears=selectyears, climatetype=climatetype, variabilitythreshold=0, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, avlland_scen=avlland_scen, proxycrop=proxycrop, aggregate=FALSE)[,,output])
    description <- paste0("Potential Water Use (", output, ")")
    unit        <- "Accessible Water (mio. m^3)"
  }
  x <- as.data.frame(dimSums(x, dim=1))

  supply_curve <- data.frame(EFP=x$Data1, Scen=x$Data2, GT0=x$Value)

  if (x_axis_range[1]==0) {
    x_axis_range <- x_axis_range[-1]
  }

  for (variabilitythreshold in x_axis_range) {
    if (output=="area") {
      x <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, selectyears=selectyears, climatetype=climatetype, variabilitythreshold=variabilitythreshold, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=potential_wat, aggregate=FALSE)[,,"irrigatable"])
    } else {
      x <- collapseNames(calcOutput("WaterPotUse",     lpjml=lpjml, selectyears=selectyears, climatetype=climatetype, variabilitythreshold=variabilitythreshold, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, avlland_scen=avlland_scen, proxycrop=proxycrop, aggregate=FALSE)[,,output])
    }
    x <- as.data.frame(dimSums(x, dim=1))

    tmp              <- data.frame(EFP=x$Data1, Scen=x$Data2, Value=x$Value)
    names(tmp)[3]    <- paste0("GT",gainthreshold)
    supply_curve     <- merge(supply_curve, tmp)
  }

  supply_curve        <- data.frame(t(data.frame(Scen=paste(supply_curve$EFP, supply_curve$Scen, sep="."),supply_curve[-c(1,2)])))
  names(supply_curve) <- supply_curve[1,]
  supply_curve        <- supply_curve[-1,]
  supply_curve        <- data.frame(GT=as.numeric(gsub("GT", "", rownames(supply_curve))), supply_curve)
  supply_curve        <- as.data.frame(lapply(supply_curve, as.numeric))

  out <- ggplot(data=supply_curve, aes_string(x="GT")) +
                geom_line(aes_string(y=paste("on", scenario, sep=".")), color="darkred") + geom_point(aes_string(y=paste("on", scenario, sep="."))) +
                geom_line(aes_string(y=paste("off", scenario, sep=".")), color="darkblue", linetype="twodash") + geom_point(aes_string(y=paste("off", scenario, sep="."))) +
                theme_bw() +
                ggtitle(paste0(description, " Supply Curve for FAOyieldcalib = ", FAOyieldcalib, " on ", avlland_scen)) + xlab(paste0("Accessability (Variabilitythreshold Q", variabilitythreshold,")")) + ylab(unit)

  return(out)
}
