#' @title       plotScatterIrrigArea
#' @description scatterplot of projected irrigated areas on current cropland and actually irrigated areas reported by LUH
#'
#' @param region           GLO or one of the H12 regions
#' @param scenario         EFP and non-agricultural water use scenarios separate by "." (e.g. "on.ssp2")
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold    Gainthreshold for allocation algorithm
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotScatterIrrigArea()
#' }
#'
#' @importFrom luplot plotmap2
#' @importFrom magclass collapseNames
#' @importFrom stats lm na.omit
#' @importFrom ggplot2 ggplot geom_point theme scale_color_manual guide_legend guides geom_smooth geom_text coord_equal geom_abline
#'
#' @export

plotScatterIrrigArea <- function(region, scenario, lpjml, selectyears, climatetype,
                                 EFRmethod, accessibilityrule, rankmethod, yieldcalib,
                                 allocationrule, gainthreshold, thresholdtype,
                                 irrigationsystem, avlland_scen, cropmix, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  # retrieve function arguments
  iniyear   <- as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])

  croparea  <- calcOutput("Croparea", years = selectyears, sectoral = "kcr", cells = "lpjcell",
                           physical = TRUE, cellular = TRUE, irrigation = TRUE, aggregate = FALSE)
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  map                          <- toolGetMappingCoord2Country()
  getCells(croparea)           <- paste(map$coords, map$iso, sep = ".")
  names(dimnames(croparea))[1] <- "x.y.iso"
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  irrigarea <- dimSums(croparea[, , "irrigated"], dim = 3)
  croparea  <- dimSums(croparea, dim = 3)

  irrigatableArea <- collapseNames(calcOutput("IrrigatableArea", selectyears = selectyears,
                                              climatetype = climatetype, lpjml = lpjml,
                                              gainthreshold = gainthreshold, rankmethod = rankmethod, yieldcalib = yieldcalib,
                                              allocationrule = allocationrule,  thresholdtype = thresholdtype,
                                              irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                                              cropmix = cropmix, potential_wat = TRUE,
                                              accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                              com_ag = FALSE, multicropping = multicropping, aggregate = FALSE)[, , scenario][, , "irrigatable"])
  # sum over seasons (harvested area throughout the year)
  irrigatableArea <- dimSums(irrigatableArea, dim = "season")

  ### Reference data ###
  # yield gain > threshold
  potIrrigGain <- calcOutput("IrrigYieldImprovementPotential", selectyears = selectyears,
                              lpjml = lpjml, climatetype = climatetype, iniyear = iniyear,
                              cropmix = cropmix, unit = thresholdtype, yieldcalib = yieldcalib,
                              multicropping = multicropping, aggregate = FALSE)
  potIrrigGain <- dimSums(potIrrigGain, dim = 3)

  # fraction of fullirrigation requirement that can be fulfilled
  watAvlAg  <- collapseNames(calcOutput("RiverSurplusDischargeAllocation",
                                         output = "potIrrigWat", selectyears = selectyears,
                                         lpjml = lpjml, climatetype = climatetype,
                                         EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
                                         rankmethod = rankmethod, yieldcalib = yieldcalib,
                                         allocationrule = allocationrule, thresholdtype = thresholdtype,
                                         gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                                         iniyear = iniyear, avlland_scen = avlland_scen,
                                         cropmix = cropmix, com_ag = FALSE,
                                         multicropping = multicropping, aggregate = FALSE)[, , scenario])

  fullirrigReq  <- calcOutput("FullIrrigationRequirement", selectyears = selectyears,
                               lpjml = lpjml, climatetype = climatetype, comagyear = NULL,
                               irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                               cropmix = cropmix, multicropping = multicropping, aggregate = FALSE)

  fracfullirrig <- watAvlAg / fullirrigReq
  fracfullirrig[fullirrigReq == 0] <- 0
  fracfullirrig <- dimSums(pmin(collapseNames(fracfullirrig[,,"consumption"]), collapseNames(fracfullirrig[,,"withdrawal"])), dim = 3)

  # regionmapping
  mapping        <- toolGetMappingCoord2Country()
  mapping$coords <- paste(mapping$coords, mapping$iso, sep = ".")
  regmap         <- toolGetMapping("regionmappingH12.csv")
  names(regmap)  <- c("Country", "iso", "reg")

  mapping <- merge(regmap, mapping)

  df <- data.frame(coord = mapping$coords,
                   irrigarea = as.data.frame(irrigarea[mapping$coords, , ])$Value,
                   irrigatableArea = as.data.frame(irrigatableArea[mapping$coords, , ])$Value,
                   fracfullirrig = as.data.frame(fracfullirrig[mapping$coords, , ])$Value,
                   gainpotential = as.data.frame(potIrrigGain[mapping$coords, , ])$Value,
                   region = mapping$reg)
  df1 <- df2 <- df3 <- df
  df1$irrigatableArea[df1$fracfullirrig <= 0] <- NA
  df2$irrigatableArea[df2$fracfullirrig <= 0] <- NA
  df3$irrigatableArea[df3$fracfullirrig > 0]  <- NA
  df1$irrigatableArea[df1$gainpotential < gainthreshold] <- NA
  df2$irrigatableArea[df2$gainpotential > gainthreshold] <- NA
  df3$irrigatableArea[df3$gainpotential < gainthreshold] <- NA

  if (region != "GLO") {
    df  <- df[df$region == region, ]
    modelstat <- lm(irrigatableArea ~ irrigarea, data = df, na.action = na.omit)
    rsquared  <- round(summary(modelstat)$r.squared, digits = 3)

    df1  <- df1[df1$region == region, ]
    p1 <- ggplot(df1, aes(x = irrigarea, y = irrigatableArea)) +
      geom_point(size = 0.1, color = "black", na.rm = TRUE) +
      geom_abline(intercept = c(0, 0), slope = 1, color = "grey") +
      # geom_smooth(method = lm, na.rm = TRUE) +
      coord_equal(xlim = c(0, 0.3), ylim = c(0, 0.3)) +
      xlab("Actually Irrigated Area according to LUH (in Mha)") +
      ylab(paste0("Projected Irrigated Area according to Algorithm on ", avlland_scen)) +
      theme_bw() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white")) +
      ggtitle(paste0("Rsquared: ", rsquared))

    df2  <- df2[df2$region == region, ]
    p2 <- ggplot(df2, aes(x = irrigarea, y = irrigatableArea)) +
      geom_point(size = 0.1, color = "green", na.rm = TRUE) +
      coord_equal(xlim = c(0, 0.3), ylim = c(0, 0.3)) +
      xlab("Actually Irrigated Area according to LUH (in Mha)") +
      ylab(paste0("Projected Irrigated Area according to Algorithm on ", avlland_scen)) +
      theme_bw() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white")) +
      ggtitle(paste0("Rsquared: ", rsquared))

    df3  <- df3[df3$region == region, ]
    p3 <- ggplot(df3, aes(x = irrigarea, y = irrigatableArea)) +
      geom_point(size = 0.1, color = "red", na.rm = TRUE) +
      coord_equal(xlim = c(0, 0.3), ylim = c(0, 0.3)) +
      xlab("Actually Irrigated Area according to LUH (in Mha)") +
      ylab(paste0("Projected Irrigated Area according to Algorithm on ", avlland_scen)) +
      theme_bw() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white")) +
      ggtitle(paste0("Rsquared: ", rsquared))

    out <- cowplot::ggdraw() + cowplot::draw_plot(p1) + cowplot::draw_plot(p3) + cowplot::draw_plot(p2)


  } else {
    # modelstat <- lm(irrigatableArea ~ irrigarea, data = df)
    # rsquared  <- round(summary(modelstat)$r.squared, digits = 3)
    # out <- ggplot(df, aes(x = irrigarea, y = irrigatableArea, color = region)) +
    #   geom_point(size = 0.05, na.rm = TRUE) +
    #   geom_smooth(method = lm, na.rm = TRUE) +
    #   coord_equal(xlim = c(0, 0.3), ylim = c(0, 0.3)) +
    #   xlab("Actually Irrigated Area according to LUH (in Mha)") +
    #   ylab(paste0("Projected Irrigated Area according to Algorithm on ", avlland_scen)) +
    #   scale_color_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928")) +
    #   guides(colour = guide_legend(override.aes = list(size = 5))) +
    #   theme_bw() +
    #   ggtitle(paste0("Rsquared: ", rsquared, " for region: ", region))

    modelstat <- lm(irrigatableArea ~ irrigarea, data = df, na.action = na.omit)
    rsquared  <- round(summary(modelstat)$r.squared, digits = 3)

    p1 <- ggplot(df1, aes(x = irrigarea, y = irrigatableArea)) +
      geom_point(size = 0.1, color = "black", na.rm = TRUE) +
      geom_abline(intercept = c(0, 0), slope = 1, color = "grey") +
      # geom_smooth(method = lm, na.rm = TRUE) +
      coord_equal(xlim = c(0, 0.3), ylim = c(0, 0.3)) +
      xlab("Actually Irrigated Area according to LUH (in Mha)") +
      ylab(paste0("Projected Irrigated Area according to Algorithm on ", avlland_scen)) +
      theme_bw() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white")) +
      ggtitle(paste0("Rsquared: ", rsquared))

    p2 <- ggplot(df2, aes(x = irrigarea, y = irrigatableArea)) +
      geom_point(size = 0.1, color = "green", na.rm = TRUE) +
      coord_equal(xlim = c(0, 0.3), ylim = c(0, 0.3)) +
      xlab("Actually Irrigated Area according to LUH (in Mha)") +
      ylab(paste0("Projected Irrigated Area according to Algorithm on ", avlland_scen)) +
      theme_bw() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white")) +
      ggtitle(paste0("Rsquared: ", rsquared))

    p3 <- ggplot(df3, aes(x = irrigarea, y = irrigatableArea)) +
      geom_point(size = 0.1, color = "red", na.rm = TRUE) +
      coord_equal(xlim = c(0, 0.3), ylim = c(0, 0.3)) +
      xlab("Actually Irrigated Area according to LUH (in Mha)") +
      ylab(paste0("Projected Irrigated Area according to Algorithm on ", avlland_scen)) +
      theme_bw() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white")) +
      ggtitle(paste0("Rsquared: ", rsquared))

    out <- cowplot::ggdraw() + cowplot::draw_plot(p1) + cowplot::draw_plot(p3) + cowplot::draw_plot(p2)

  }

  return(out)
}
