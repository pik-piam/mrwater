#' @title       plotCurveIrrigAreaDemand
#' @description plot minimum monetary yield gain per ha achieved
#'              on related potentially irrigated area
#'
#' @param region           regional resolution (can be country iso-code, region name and respective mapping "EUR:H12", "GLO" for global)
#' @param y_axis_range     range of y-axis (monetary irrigation gain) to be depicted on the curve
#' @param scenario         combination of EFP and non-agricultural water use scenario
#'                         to be displayed in plot separated by "."
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param iniyear          initialization year
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param cropmix          cropmix for which irrigation yield improvement is calculated
#'                         can be selection of proxycrop(s) for calculation of average yield gain
#'                         or hist_irrig or hist_total for historical cropmix
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
#' @param display          specifies whether current cropland, potential cropland
#'                         or both should be displayed in the graph
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotCurveIrrigAreaDemand(y_axis_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom stats reshape
#' @importFrom ggplot2 ggplot geom_line geom_vline geom_point aes_string ggtitle xlab ylab theme_bw geom_text xlim ylim
#'
#' @export

plotCurveIrrigAreaDemand <- function(y_axis_range, region = "GLO", scenario,
                                     lpjml, selectyears, iniyear, climatetype, EFRmethod,
                                     accessibilityrule, rankmethod, yieldcalib, allocationrule,
                                     thresholdtype, irrigationsystem, cropmix, potential_wat = TRUE,
                                     com_ag, multicropping, display) {

  ## Main data: with water constraint
  # on current cropland
  inputdata <- calcOutput("EconOfIrrig", GT_range = y_axis_range, scenario = gsub(".*\\.", "", scenario), season = "single", region = region, output = "IrrigArea",
                       lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                       EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = FALSE,
                       allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                       avlland_scen = "currCropland:2010", cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)
  inputdata <- as.data.frame(inputdata)

  if (multicropping) {
    tmp <- calcOutput("EconOfIrrig", GT_range = y_axis_range, scenario = gsub(".*\\.", "", scenario), season = "double", region = region, output = "IrrigArea",
                            lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                            EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = FALSE,
                            allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                            avlland_scen = "currCropland:2010", cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)
    tmp <- as.data.frame(tmp)

    tmp2 <- calcOutput("EconOfIrrig", GT_range = y_axis_range, scenario = gsub(".*\\.", "", scenario), season = "triple", region = region, output = "IrrigArea",
                      lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                      EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = FALSE,
                      allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                      avlland_scen = "currCropland:2010", cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)
    tmp2 <- as.data.frame(tmp2)


    df <- data.frame(GT = as.numeric(as.character(inputdata$Data1)),
                     scen = as.character(inputdata$Data2),
                     IrrigArea1 = inputdata$Value,
                     IrrigArea2 = tmp$Value,
                     IrrigArea3 = tmp2$Value,
                     stringsAsFactors = FALSE)

  } else {
    df <- data.frame(GT = as.numeric(as.character(inputdata$Data1)),
                     scen = as.character(inputdata$Data2),
                     IrrigArea = inputdata$Value, stringsAsFactors = FALSE)
  }

  df            <- reshape(df, idvar = "GT", timevar = "scen", direction = "wide")
  names(df)[-1] <- paste(names(df)[-1], "CurrCropland", sep = ".")

  # on potential cropland
  inputdata <- calcOutput("EconOfIrrig", GT_range = y_axis_range, scenario = gsub(".*\\.", "", scenario), season = "single", region = region, output = "IrrigArea",
                          lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                          EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = FALSE,
                          allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                          avlland_scen = "potIrrig_HalfEarth:2010", cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)
  inputdata <- as.data.frame(inputdata)

  if (multicropping) {
    tmp <- calcOutput("EconOfIrrig", GT_range = y_axis_range, scenario = gsub(".*\\.", "", scenario), season = "double", region = region, output = "IrrigArea",
                      lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                      EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = FALSE,
                      allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                      avlland_scen = "potIrrig_HalfEarth:2010", cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)
    tmp <- as.data.frame(tmp)

    tmp2 <- calcOutput("EconOfIrrig", GT_range = y_axis_range, scenario = gsub(".*\\.", "", scenario), season = "triple", region = region, output = "IrrigArea",
                       lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                       EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = FALSE,
                       allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                       avlland_scen = "potIrrig_HalfEarth:2010", cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)
    tmp2 <- as.data.frame(tmp2)


    df <- data.frame(GT = as.numeric(as.character(inputdata$Data1)),
                     scen = as.character(inputdata$Data2),
                     IrrigArea1 = inputdata$Value,
                     IrrigArea2 = tmp$Value,
                     IrrigArea3 = tmp2$Value,
                     stringsAsFactors = FALSE)
  } else {
    tmp <- data.frame(GT = as.numeric(as.character(inputdata$Data1)),
                     scen = as.character(inputdata$Data2),
                     IrrigArea = inputdata$Value, stringsAsFactors = FALSE)
  }

  tmp            <- reshape(tmp, idvar = "GT", timevar = "scen", direction = "wide")
  names(tmp)[-1] <- paste(names(tmp)[-1], "PotCropland", sep = ".")

  df             <- merge(df, tmp)

  ## Reference data: without water constraint
  # current cropland
  inputdata <- calcOutput("YieldgainArea", region = region, GT_range = y_axis_range, lpjml = lpjml,
                          selectyears = selectyears, climatetype = climatetype, EFRmethod = EFRmethod,
                          yieldcalib = yieldcalib, thresholdtype = thresholdtype, avlland_scen = "currCropland:2010",
                          cropmix = cropmix, multicropping = multicropping, aggregate = FALSE)
  inputdata <- as.data.frame(inputdata)

  if (multicropping) {
    tmp <- data.frame(GT = as.numeric(as.character(levels(inputdata$Data1[inputdata$Data2 == 1]))),
                      YieldGainArea1 = inputdata$Value[inputdata$Data2 == "addMC0"],
                      YieldGainArea2 = inputdata$Value[inputdata$Data2 == "addMC1"],
                      YieldGainArea3 = inputdata$Value[inputdata$Data2 == "addMC2"],
                      stringsAsFactors = FALSE)
  } else {
    tmp <- data.frame(GT = as.numeric(as.character(inputdata$Data1)),
                      YieldGainArea = inputdata$Value, stringsAsFactors = FALSE)
  }

  names(tmp)[-1] <- paste(names(tmp)[-1], "CurrCropland", sep = ".")
  df             <- merge(df, tmp)

  # potential cropland
  inputdata  <- calcOutput("YieldgainArea", region = region, GT_range = y_axis_range, lpjml = lpjml,
                              selectyears = selectyears, climatetype = climatetype, EFRmethod = EFRmethod,
                              yieldcalib = yieldcalib, thresholdtype = thresholdtype, avlland_scen = "potIrrig_HalfEarth:2010",
                              cropmix = cropmix, multicropping = multicropping, aggregate = FALSE)
  inputdata <- as.data.frame(inputdata)

  if (multicropping) {
    tmp <- data.frame(GT = as.numeric(as.character(levels(inputdata$Data1[inputdata$Data2 == 1]))),
                      YieldGainArea1 = inputdata$Value[inputdata$Data2 == "addMC0"],
                      YieldGainArea2 = inputdata$Value[inputdata$Data2 == "addMC1"],
                      YieldGainArea3 = inputdata$Value[inputdata$Data2 == "addMC2"],
                      stringsAsFactors = FALSE)
  } else {
    tmp <- data.frame(GT = as.numeric(as.character(inputdata$Data1)),
                      YieldGainArea = inputdata$Value, stringsAsFactors = FALSE)
  }

  names(tmp)[-1] <- paste(names(tmp)[-1], "PotCropland", sep = ".")
  df <- merge(df, tmp)

  ## Reference points
  # Area that can be irrigated with committed agricultural uses
  current_fulfilled <- collapseNames(calcOutput("IrrigatableArea", gainthreshold = 0,
                                      selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
                                      accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                      avlland_scen = "currCropland:2010", cropmix = cropmix, multicropping = multicropping,
                                      potential_wat = FALSE, com_ag = TRUE, aggregate = FALSE)[, , "irrigatable"][, , "single"])
  # Area that is irrigated according to LUH
  current_LUH <- dimSums(calcOutput("Croparea", years = iniyear, sectoral = "kcr",
                                    cells = "lpjcell", physical = TRUE, cellular = TRUE,
                                    irrigation = TRUE, aggregate = FALSE)[, , "irrigated"], dim = 3)
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  map                             <- toolGetMappingCoord2Country()
  getCells(current_LUH)           <- paste(map$coords, map$iso, sep = ".")
  names(dimnames(current_LUH))[1] <- "x.y.iso"
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

  # sum up over regional dimension
  current_fulfilled <- toolRegionSums(x = current_fulfilled, region = region)
  current_LUH       <- toolRegionSums(x = current_LUH,       region = region)

  if (multicropping) {

    if (display == "currCropland") {

      out <- ggplot(data = df, aes_string(y = "GT")) +
        geom_line(aes_string(x = paste("IrrigArea1", gsub("\\..*", "", scenario), "CurrCropland", sep = ".")), color = "darkgreen", size = 1.1) + geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "CurrCropland", sep = "."))) +
        geom_line(aes_string(x = paste("IrrigArea2", gsub("\\..*", "", scenario), "CurrCropland", sep = ".")), color = "darkblue", size = 1.1) + geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "CurrCropland", sep = "."))) +
        geom_line(aes_string(x = paste("IrrigArea3", gsub("\\..*", "", scenario), "CurrCropland", sep = ".")), color = "black", size = 1.1) + geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "CurrCropland", sep = "."))) +

        geom_line(aes_string(x = paste("YieldGainArea1", "CurrCropland", sep = ".")), color = "darkgreen", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea1", "CurrCropland", sep = "."))) +
        geom_line(aes_string(x = paste("YieldGainArea2", "CurrCropland", sep = ".")), color = "darkblue", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea2", "CurrCropland", sep = "."))) +
        geom_line(aes_string(x = paste("YieldGainArea3", "CurrCropland", sep = ".")), color = "black", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea3", "CurrCropland", sep = "."))) +

        theme_bw() +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1400, by = 100)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("on", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "blue", size = 2) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("off", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "red", size = 2) +
        geom_point(x = as.numeric(current_LUH), y = 0, color = "black", size = 2) +
        ggtitle("Irrigation Area Demand Curve on current Cropland") + ylab("Monetary yield gain (USD/ha)") + xlab("Potentially irrigated area (Mha)")


    } else if (display == "potCropland") {

      out <- ggplot(data = df, aes_string(y = "GT")) +
        geom_line(aes_string(x = paste("IrrigArea1", gsub("\\..*", "", scenario), "PotCropland", sep = ".")), color = "darkgreen", size = 1.1) + geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "CurrCropland", sep = "."))) +
        geom_line(aes_string(x = paste("IrrigArea2", gsub("\\..*", "", scenario), "PotCropland", sep = ".")), color = "darkblue", size = 1.1) + geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "CurrCropland", sep = "."))) +
        geom_line(aes_string(x = paste("IrrigArea3", gsub("\\..*", "", scenario), "PotCropland", sep = ".")), color = "black", size = 1.1) + geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "CurrCropland", sep = "."))) +


        geom_line(aes_string(x = paste("YieldGainArea1", "PotCropland", sep = ".")), color = "darkgreen", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea1", "PotCropland", sep = "."))) +
        geom_line(aes_string(x = paste("YieldGainArea2", "PotCropland", sep = ".")), color = "darkblue", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea2", "PotCropland", sep = "."))) +
        geom_line(aes_string(x = paste("YieldGainArea3", "PotCropland", sep = ".")), color = "black", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea3", "PotCropland", sep = "."))) +


        theme_bw() +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 4000, by = 100)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("on", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "blue", size = 2) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("off", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "red", size = 2) +
        geom_point(x = as.numeric(current_LUH), y = 0, color = "black", size = 2) +
        ggtitle("Irrigation Area Demand Curve on Potential Cropland") + ylab("Monetary yield gain (USD/ha)") + xlab("Potentially irrigated area (Mha)")

    } else {
      stop("Please choose if current (currCropland), potential (potCropland)
           should be displayed in the graph")
    }


  } else {

    if (display == "currCropland") {

      out <- ggplot(data = df, aes_string(y = "GT")) +
        geom_line(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "CurrCropland", sep = ".")), color = "darkgreen", size = 1.1) +
        geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "CurrCropland", sep = "."))) +
        geom_line(aes_string(x = paste("YieldGainArea", "CurrCropland", sep = ".")), color = "darkgreen", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea", "CurrCropland", sep = "."))) +
        theme_bw() +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1400, by = 100)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("on", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "blue", size = 2) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("off", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "red", size = 2) +
        geom_point(x = as.numeric(current_LUH), y = 0, color = "black", size = 2) +
        ggtitle("Irrigation Area Demand Curve on current Cropland") + ylab("Monetary yield gain (USD/ha)") + xlab("Potentially irrigated area (Mha)")


    } else if (display == "potCropland") {

      out <- ggplot(data = df, aes_string(y = "GT")) +
        geom_line(aes_string(x = paste("IrrigArea",  gsub("\\..*", "", scenario), "PotCropland", sep = ".")), color = "darkred", size = 1.1) +
        geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "PotCropland", sep = "."))) +
        geom_line(aes_string(x = paste("YieldGainArea", "PotCropland", sep = ".")), color = "darkred", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea", "PotCropland", sep = "."))) +
        theme_bw() +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 4000, by = 100)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("on", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "blue", size = 2) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("off", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "red", size = 2) +
        geom_point(x = as.numeric(current_LUH), y = 0, color = "black", size = 2) +
        ggtitle("Irrigation Area Demand Curve on Potential Cropland") + ylab("Monetary yield gain (USD/ha)") + xlab("Potentially irrigated area (Mha)")


    } else if (display == "curr+potCropland") {

      out <- ggplot(data = df, aes_string(y = "GT")) +
        geom_line(aes_string(x = paste("IrrigArea",  gsub("\\..*", "", scenario), "CurrCropland", sep = ".")), color = "darkgreen", size = 1.1) +
        geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "CurrCropland", sep = "."))) +
        geom_line(aes_string(x = paste("IrrigArea",  gsub("\\..*", "", scenario), "PotCropland", sep = ".")), color = "darkred", size = 1.1) +
        geom_point(aes_string(x = paste("IrrigArea", gsub("\\..*", "", scenario), "PotCropland", sep = "."))) +
        geom_line(aes_string(x = paste("YieldGainArea", "CurrCropland", sep = ".")), color = "darkgreen", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea", "CurrCropland", sep = "."))) +
        geom_line(aes_string(x = paste("YieldGainArea", "PotCropland", sep = ".")), color = "darkred", linetype = "twodash", size = 1.1) +
        geom_point(aes_string(x = paste("YieldGainArea", "PotCropland", sep = "."))) +
        theme_bw() +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 4000, by = 100)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("on", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "blue", size = 2) +
        geom_point(x = as.numeric(current_fulfilled[, , paste("off", gsub(".*\\.", "", scenario), sep = ".")]), y = 0, color = "red", size = 2) +
        geom_point(x = as.numeric(current_LUH), y = 0, color = "black", size = 2) +
        ggtitle("Irrigation Area Demand Curve") + ylab("Monetary yield gain (USD/ha)") + xlab("Potentially irrigated area (Mha)")

    } else {
      stop("Please choose if current (currCropland), potential (potCropland) or
         both (curr+potCropland) should be displayed in the graph")
    }

  }

  return(out)
}
