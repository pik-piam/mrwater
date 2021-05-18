#' @title       reportEconOfIrrig
#' @description reports irrigatable area depending on costs paid for irrigation
#'
#' @param region           regional resolution (can be country iso-code, region name and respective mapping "EUR:H12", "GLO" for global)
#' @param output           output to be displayed: irrigated area "IrrigArea" or available water volume "wat_ag_ww" "wat_ag_wc"
#' @param GT_range         range of x-axis (gainthreshold) to be depicted on the curve
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param FAOyieldcalib    TRUE (LPJmL yields scaled with current FAO yield) or FALSE (LPJmL yield potentials)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param proxycrop        proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ reportEconOfIrrig(GT_range=seq(0, 10000, by=100), scenario="ssp2") }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom stringr str_split
#'
#' @export

reportEconOfIrrig <- function(region="GLO", output, GT_range, scenario, lpjml, selectyears, climatetype, EFRmethod, accessibilityrule, rankmethod, FAOyieldcalib, allocationrule, thresholdtype, irrigationsystem, avlland_scen, proxycrop, potential_wat=TRUE, com_ag) {

  if (length(selectyears)>1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  iniyear <- as.numeric(as.list(strsplit(avlland_scen, split=":"))[[1]][2])

  if (output=="IrrigArea") {
    x <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=0, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=potential_wat, com_ag=com_ag, aggregate=FALSE)[,,"irrigatable"])
    d <- "Irrigatable Area"
    u <- "Irrigatable Area (Mha)"
  } else {
    x <- collapseNames(calcOutput("WaterPotUse",      lpjml=lpjml, gainthreshold=0, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, iniyear=iniyear, avlland_scen=avlland_scen, proxycrop=proxycrop, com_ag=com_ag, aggregate=FALSE)[,,output])
    # transform from mio. m^3 to km^3:
    # (1 km^3 = 1e+09 m^3)
    # (1 mio. = 1e+06)
    x <- x / 1000
    d <- paste0("Water Use Potential")
    u <- paste0("Potential water use", output, "(km^3)")
  }
  if (region=="GLO") {
    x <- as.data.frame(dimSums(x, dim=1))

  } else {
    map    <- str_split(region, ":")[[1]][2]
    region <- str_split(region, ":")[[1]][1]

    # aggregate to iso-countries
    mapping        <- toolGetMappingCoord2Country()
    mapping$coords <- paste(mapping$coords, mapping$iso, sep=".")
    x <- toolAggregate(x, rel=mapping, from="coords", to="iso", dim=1)
    x <- toolCountryFill(x, fill=0) # Note: "ABW" "AND" "ATA" "BES" "BLM" "BVT" "GIB" "LIE" "MAC" "MAF" "MCO" "SMR" "SXM" "VAT" "VGB" missing in LPJmL cells

    # aggregate to regions
    if (!is.na(map) && map=="H12") {
      regmap        <- toolGetMapping("regionmappingH12.csv")
      names(regmap) <- c("Country", "iso", "reg")
      x             <- toolAggregate(x, rel=regmap, from="iso", to="reg", dim=1)
    } else if (!is.na(map) && map!="H12") {
      stop("Selected regionmapping is not yet available. Please select region and respective mapping via region argument: e.g. EUR:H12")
    }

    x <- as.data.frame(dimSums(x[region,,], dim=1))
  }

  df <- data.frame(EFP=x$Data1, Scen=x$Data2, GT0=x$Value, stringsAsFactors = FALSE)

  if (GT_range[1]==0) {
    GT_range <- GT_range[-1]
  }

  for (gainthreshold in GT_range) {

    if (output=="IrrigArea") {
      x  <- collapseNames(calcOutput("IrrigatableArea", lpjml=lpjml, gainthreshold=gainthreshold, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=potential_wat, com_ag=com_ag, aggregate=FALSE)[,,"irrigatable"])
    } else {
      x <- collapseNames(calcOutput("WaterPotUse",      lpjml=lpjml, gainthreshold=gainthreshold, selectyears=selectyears, climatetype=climatetype, accessibilityrule=accessibilityrule, EFRmethod=EFRmethod, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, irrigationsystem=irrigationsystem, iniyear=iniyear, avlland_scen=avlland_scen, proxycrop=proxycrop, com_ag=com_ag, aggregate=FALSE)[,,output])
      x <- x / 1000
    }

    if (region=="GLO") {
      x <- as.data.frame(dimSums(x, dim=1))

    } else {
      map    <- str_split(region, ":")[[1]][2]
      region <- str_split(region, ":")[[1]][1]

      # aggregate to iso-countries
      mapping        <- toolGetMappingCoord2Country()
      mapping$coords <- paste(mapping$coords, mapping$iso, sep=".")
      x <- toolAggregate(x, rel=mapping, from="coords", to="iso", dim=1)
      x <- toolCountryFill(x, fill=0) # Note: "ABW" "AND" "ATA" "BES" "BLM" "BVT" "GIB" "LIE" "MAC" "MAF" "MCO" "SMR" "SXM" "VAT" "VGB" missing in LPJmL cells

      # aggregate to regions
      if (!is.na(map) && map=="H12") {
        regmap        <- toolGetMapping("regionmappingH12.csv")
        names(regmap) <- c("Country", "iso", "reg")
        x             <- toolAggregate(x, rel=regmap, from="iso", to="reg", dim=1)
      } else if (!is.na(map) && map!="H12") {
        stop("Selected regionmapping is not yet available. Please select region and respective mapping via region argument: e.g. EUR:H12")
      }

      x <- as.data.frame(dimSums(x[region,,], dim=1))
    }

    tmp              <- data.frame(EFP=x$Data1, Scen=x$Data2, Value=x$Value)
    names(tmp)[3]    <- paste0("GT",gainthreshold)
    df     <- merge(df, tmp)
  }

  df        <- data.frame(t(data.frame(Scen=paste(output, df$EFP, df$Scen, sep="."), df[-c(1,2)])), stringsAsFactors = FALSE)
  names(df) <- as.character(unlist(df[1,]))
  df        <- df[-1,]
  df        <- data.frame(GT=as.numeric(gsub("GT", "", rownames(df))), df, stringsAsFactors = FALSE)
  df        <- as.data.frame(lapply(df, as.numeric))

  return(list(data=df, description=d, unit=u))
}
