#' @title       calcRevenue
#' @description calculates revenue on selected area under selected management
#'
#' @param management        management in terms of irrigation and multiple cropping practices
#'                          "actual": irrigation and multiple cropping as reported by the
#'                                    Landuse Toolbox;
#'                          "single_rainfed"; "single_irrigated";
#'                          "multiple_rainfed"; "multiple_irrigated"
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param lpjml             LPJmL version used
#' @param selectyears       Years for which irrigatable area is calculated
#' @param iniyear           Initialization year for initial croparea
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          USD_ha (USD per hectare) for relative area return, or
#'                          USD_m3 (USD per cubic meter) for relative volumetric return;
#'                          USD for absolute return (total profit);
#'                          Price aggregation:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices
#'                          and boolean indicating fullpotential (TRUE, i.e. cell
#'                          receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                          receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (in USD per hectare)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param comAg             If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("Revenue", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames

calcRevenue <- function(management, landScen,
                        lpjml, climatetype, selectyears, iniyear,
                        efrMethod, accessibilityrule,
                        rankmethod, yieldcalib, allocationrule, gainthreshold,
                        irrigationsystem, cropmix, comAg,
                        transDist) {

  #########################
  ### Extract Arguments ###
  #########################
  priceAgg <- unlist(strsplit(rankmethod, split = ":"))[2]

  if (grepl(pattern = "single", x = management)) {
    m1 <- m2 <- FALSE
  } else {
    m1 <- "TRUE:potential:endogenous"
  }
  if (grepl(pattern = "actual", x = management)) {
    m2   <- "TRUE:actual:irrig_crop"
    land <- "currIrrig:NULL"
  } else {
    land <- landScen
  }

  ######################
  ### Read in Inputs ###
  ######################
  # read in cellular lpjml yields (in USD/ha)
  yields <- calcOutput("YieldsValued",
                       lpjml = lpjml, climatetype = climatetype,
                       iniyear = iniyear, selectyears = selectyears,
                       yieldcalib = yieldcalib,
                       priceAgg = priceAgg,
                       multicropping = m1,
                       aggregate = FALSE)
  # read in area for which revenue shall be calculated (in Mha)
  pia <- collapseNames(calcOutput("IrrigAreaPotential", cropAggregation = FALSE,
                                  lpjml = lpjml, climatetype = climatetype,
                                  selectyears = selectyears, iniyear = iniyear,
                                  efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib,
                                  allocationrule = allocationrule, gainthreshold = gainthreshold,
                                  irrigationsystem = irrigationsystem, landScen = land,
                                  cropmix = cropmix, comAg = comAg,
                                  multicropping = m2, transDist = transDist,
                                  aggregate = FALSE))

  ####################
  ### Calculations ###
  ####################
  # Revenue (in mio. USD)
  if (management == "actual") {

    # Single cropping yields
    yieldsSingle <- calcOutput("YieldsValued",
                               lpjml = lpjml, climatetype = climatetype,
                               iniyear = iniyear, selectyears = selectyears,
                               yieldcalib = yieldcalib,
                               priceAgg = priceAgg,
                               multicropping = FALSE,
                               aggregate = FALSE)
    deltaYields <- yields - yieldsSingle
    deltaYields[deltaYields < 0] <- 0

    # Cropping intensity
    ci <- calcOutput("MulticroppingIntensity",
                     scenario = strsplit(m2, split = ":")[[1]][3],
                     selectyears = selectyears, sectoral = "kcr",
                     lpjml = lpjml, climatetype = climatetype,
                     aggregate = FALSE)
    ci <- dimOrder(ci, c(2, 1), dim = 3)
    ci <- ci[, , getItems(yields, dim = 3)]
    # Share of area that is multicropped
    shrMC <- (ci - 1)

    # Potentially irrigated area under actual conditions (in Mha)
    pia <- add_dimension(pia, dim = 3.4, nm = c("irrigated", "rainfed"), add = "irrigation")
    pia[, , "rainfed"] <- 0

    currCroparea <- new.magpie(cells_and_regions = getItems(pia, dim = 1),
                               years = getItems(pia, dim = 2),
                               names = getItems(pia, dim = 3),
                               fill = 0)
    getSets(currCroparea) <- getSets(pia)

    # Croparea as reported by chosen data source (in Mha)
    tmp <- calcOutput("CropareaAdjusted", iniyear = iniyear, aggregate = FALSE)

    # Areas reported to be irrigated
    currCroparea[, , "irrigated"] <- tmp[, , "irrigated"]
    # Mismatch of areas reported to be irrigated and those potentially irrigated
    diff <- collapseNames(currCroparea[, , "irrigated"] - pia[, , "irrigated"])

    if (any(round(diff, digits = 2) < 0)) {
      stop("There is more PIA than currently irrigated area in the currIrrig scenario.
            Please check calcRevenue and its upstream functions for potential bug!")
    }

    if (grepl("currIrrig", landScen)) {
      # Only currently irrigated land is evaluated
      shrMC[, , "rainfed"] <- 0

      # All areas that cannot be irrigated following (according to the PIA) receive
      # the rainfed yield for the calculation of the revenue
      pia[, , "rainfed"] <- diff
    } else {
      # Rainfed areas as reported currently
      currCroparea[, , "rainfed"] <- tmp[, , "rainfed"]
      # All areas that cannot be irrigated following (according to the PIA) receive
      # the rainfed yield for the calculation of the revenue
      pia[, , "rainfed"] <- currCroparea[, , "rainfed"] + diff
    }

    # Cellular Revenue (in mio. USD): Crop-specific Potentially Irrigated Area (Mha) x
    #                                 crop yields (USD/ha)
    out <- collapseNames(dimSums(pia * yieldsSingle + pia * deltaYields * shrMC,
                                 dim = c("crop", "irrigation")))

  } else {
    # Cellular Revenue (in mio. USD): Crop-specific Potentially Irrigated Area (Mha) x
    #                                 crop yields (USD/ha)
    out <- collapseNames(dimSums(pia * collapseNames(yields[, , strsplit(management, split = "_")[[1]][2]]),
                                 dim = "crop"))
  }

  # check for NAs and negative values
  if (any(is.na(out))) {
    stop("calcRevenue produced NA values")
  }
  if (any(out < 0)) {
    stop("calcRevenue produced negative values")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. USD",
              description  = "Potential revenue achieved on selected area
                              under chosen management",
              isocountries = FALSE))
}
