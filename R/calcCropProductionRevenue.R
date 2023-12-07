#' @title       calcCropProductionRevenue
#' @description calculates irrigated and rainfed production quantities or revenue
#'              on given crop areas under selected management scenario.
#'              It can return quantities in terms of the sum of crop biomass produced in tDM
#'              or production revenue in terms of monetary revenue achieved in USD
#'
#' @param outputtype        "biomass": returns the production quantity in terms of
#'                                     crop biomass (in tDM)
#'                          "revenue": returns the revenue in terms of price x quantity
#'                                     of crop production (in USD)
#' @param scenario          EFP and non-agricultural water use scenario separated with a "."
#'                          (e.g. "on.ssp2")
#' @param management        management in terms of irrigation and multiple cropping practices
#'                          consisting of multiple cropping scenario ("single", actMC", "potMC")
#'                          and ("potential", "counterfactual")
#'                          separated by ":"
#'                          (e.g. "single:potential", "actMC:counterfactual")
#'                          Multiple cropping practices:
#'                          "single": single cropping assumed everywhere
#'                          "actMC": multiple cropping as reported by LandInG data
#'                          "potMC": multiple cropping practiced everywhere where it is possible
#'                          Irrigation assumption:
#'                          "potential": irrigation practiced where possible according to
#'                                       irrigation potentials or practiced according to
#'                                       area information (see param `area`)
#'                          "counterfactual": production and revenue for counterfactual
#'                                            case that irrigated areas were managed under
#'                                            rainfed conditions
#' @param area              area scenario
#'                          "actual": current cropland and currently irrigated areas
#'                          or potentially irrigated areas following land availability
#'                          defined in landScen scenario: consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, or one of the scenarios available
#'                             in calcConservationPriorities,
#'                             e.g., 30by20, BH, BH_IFL, PBL_HalfEarth,
#'                             or NA for no protection).
#'                          For case of no land protection select "NA" in second part of argument
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
#'                          "hist_rainf" for historical rainfed cropmix,
#'                          or selection of proxycrops)
#' @param comAg             If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#' @param fossilGW          If TRUE: non-renewable groundwater can be used.
#'                          If FALSE: non-renewable groundwater cannot be used.
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("CropProductionRevenue", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames

calcCropProductionRevenue <- function(outputtype, scenario, management, area,
                                      lpjml, climatetype, selectyears, iniyear,
                                      efrMethod, accessibilityrule,
                                      rankmethod, yieldcalib, allocationrule, gainthreshold,
                                      irrigationsystem, cropmix, comAg,
                                      transDist, fossilGW) {
  #########################
  ### Extract Arguments ###
  #########################
  priceAgg <- unlist(strsplit(rankmethod, split = ":"))[2]

  if (grepl(pattern = "single", x = management)) {
    m1 <- m2 <- FALSE
  } else {
    m1 <- m2 <- "TRUE:potential:endogenous"
  }
  if (grepl(pattern = "actMC", x = management)) {
    m2   <- "TRUE:actual:irrig_crop"
  }

  # extract land scenario information
  landScen <- area

  ####################
  ### Read in data ###
  ####################
  ### Area ###
  if (area == "actual") {
    # Current cropland
    tmp <- calcOutput("CropareaAdjusted", iniyear = iniyear,
                      aggregate = FALSE)
    # Crop-specific total (rainfed + irrigated) cropareas (in Mha)
    cropareaTotal <- dimSums(tmp, dim = "irrigation")
    # Crop-specific irrigated areas (in Mha)
    cropareaIrrig <- collapseNames(tmp[, , "irrigated"])

    # In "actual" scenario, only currently irrigated areas are available
    # for irrigation
    landScen <- "currIrrig:NA"

  } else {
    # Crop-specific irrigated and rainfed cropareas (in Mha)
    # depending on chosen land and management scenario
    # Note: no committed agricultural areas subtracted because full area is needed here
    # For (rainfed) areas to match under committed ag. scenario, hist_total needs to be selected as cropmix
    # for total crop areas
    if (comAg) {
      cmix <- "hist_total"
    } else {
      cmix <- cropmix
    }
    if (grepl("currIrrig", landScen)) {
      cropmix <- cmix <- "hist_irrig"
    }
    # all of available croparea (in Mha)
    cropareaTotal <- calcOutput("CropAreaPotIrrig",
                                cropmix = cmix,
                                landScen = landScen,
                                selectyears = selectyears, iniyear = iniyear,
                                comagyear = NULL,
                                aggregate = FALSE)

    # Crop-specific (potentially) irrigated areas (in Mha)
    # depending on chosen land, management, and water limitation scenario
    cropareaIrrig <- collapseNames(calcOutput("PotIrrigAreas", cropAggregation = FALSE,
                                              cropmix = cropmix, landScen = landScen,
                                              transDist = transDist, fossilGW = fossilGW,
                                              multicropping = m2,
                                              lpjml = lpjml, climatetype = climatetype,
                                              selectyears = selectyears, iniyear = iniyear,
                                              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                                              rankmethod = rankmethod, yieldcalib = yieldcalib,
                                              allocationrule = allocationrule, gainthreshold = gainthreshold,
                                              irrigationsystem = irrigationsystem,
                                              comAg = comAg, aggregate = FALSE)[, , scenario])
  }

  ### Yields ###
  # Crop-specific rainfed and irrigated yields
  # depending on chosen output type (biomass or revenue), they are given in tDM/ha or USD/ha
  if (outputtype == "biomass") {
    # Yields in tDM/ha
    yields <- calcOutput("YieldsAdjusted",
                         lpjml = lpjml, climatetype = climatetype,
                         iniyear = iniyear, selectyears = selectyears,
                         yieldcalib = yieldcalib,
                         multicropping = m1, aggregate = FALSE)
    # Single cropping yields (in tDM/ha)
    yieldsSingle <- calcOutput("YieldsAdjusted",
                               lpjml = lpjml, climatetype = climatetype,
                               iniyear = iniyear, selectyears = selectyears,
                               yieldcalib = yieldcalib,
                               multicropping = FALSE, aggregate = FALSE)

    unit        <- "mio. tDM"
    description <- paste0("Crop- and irrigation-specific ",
                          "biomass production on selected area ",
                          "under chosen management scenario.")

  } else if (outputtype == "revenue") {
    # Yields (in USD/ha)
    yields <- calcOutput("YieldsValued",
                         lpjml = lpjml, climatetype = climatetype,
                         iniyear = iniyear, selectyears = selectyears,
                         yieldcalib = yieldcalib,
                         priceAgg = priceAgg,
                         multicropping = m1,
                         aggregate = FALSE)
    # Single cropping yields (in USD/ha)
    yieldsSingle <- calcOutput("YieldsValued",
                               lpjml = lpjml, climatetype = climatetype,
                               iniyear = iniyear, selectyears = selectyears,
                               yieldcalib = yieldcalib,
                               priceAgg = priceAgg,
                               multicropping = FALSE,
                               aggregate = FALSE)

    unit        <- "mio. USD"
    description <- paste0("Crop- and irrigation-specific ",
                          "production revenue on selected area ",
                          "under chosen management scenario.")

  } else {
    stop("Please choose `outputtype` in mrwater::calcCropProductionRevenue:
         `'biomass'` for biomass production quantity in tDM or
         `'revenue'` for associated revenue with production quantity in USD.")
  }

  # reorder third dimension (switch irrigation and crop)
  yields       <- dimOrder(yields, c(2, 1), dim = 3)
  yieldsSingle <- dimOrder(yieldsSingle, c(2, 1), dim = 3)

  # difference between multiple cropped and single yields
  deltaYields <- yields - yieldsSingle
  if (any(round(deltaYields, digits = 6) < 0)) {
    stop("Multiple cropping yields are smaller than single cropping yields.
    Please check whether this can be correct!")
  } else {
    # correct negatives that occur for numerical reasons
    deltaYields[deltaYields < 0] <- 0
  }

  ### Share Multicropped ###
  if (grepl(pattern = "actMC", x = management)) {
    # For actMC scenario (actual multiple cropping as reported by the LandInG data set)
    # Current Cropping intensity (CI) according to LandInG data set
    ci <- calcOutput("MulticroppingIntensity",
                     scenario = strsplit(m2, split = ":")[[1]][3],
                     selectyears = selectyears, sectoral = "kcr",
                     aggregate = FALSE)
    # reorder third dimension (switch irrigation and crop)
    ci <- dimOrder(ci, c(2, 1), dim = 3)
    ci <- ci[, , getItems(yields, dim = 3)]

    # Share of area that is multicropped
    shrMC <- (ci - 1)

  } else if (grepl(pattern = "potMC", x = management)) {
    # Share of irrigated area that can be multiple cropped
    shrMC <- calcOutput("PotMulticroppingShare", scenario = scenario,
                        lpjml = lpjml, climatetype = climatetype,
                        selectyears = selectyears, iniyear = iniyear,
                        efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                        allocationrule = allocationrule, gainthreshold = gainthreshold,
                        irrigationsystem = irrigationsystem, landScen = landScen,
                        cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
                        multicropping = m2, transDist = transDist,
                        aggregate = FALSE)

  } else if (grepl(pattern = "single", x = management)) {
    # for single: no multiplecropping
    shrMC <- new.magpie(cells_and_regions = getItems(cropareaIrrig, dim = 1),
                        years = selectyears,
                        names = getItems(yields, dim = 3),
                        sets = c("x", "y", "iso", "year", "irrigation", "crop"),
                        fill = 0)
  }

  ####################
  ### Preparations ###
  ####################
  production <- new.magpie(cells_and_regions = getItems(cropareaIrrig, dim = 1),
                           years = selectyears,
                           names = getItems(yields, dim = 3),
                           sets = c("x", "y", "iso", "year", "irrigation", "crop"),
                           fill = NA)

  ####################
  ### Calculations ###
  ####################
  ### Area ###
  # Rainfed cropland
  cropareaRainfed <- cropareaTotal - cropareaIrrig

  # Check for negative rainfed cropareas
  if (any(round(cropareaRainfed, digits = 6) < 0)) {
    warning(paste0("In mrwater::calcCropProductionRevenue: rainfed croparea became negative.
                   This should not be the case and indicates a data mismatch
                   between total cropland and irrigated croparea.
                   Please check for the following settings: ",
                   "outputtype: ", outputtype,
                   "management: ", management,
                   "area: ", area,
                   "allocationrule: ", allocationrule,
                   "cropmix: ", cropmix,
                   "comAg: ", comAg,
                   "transDist: ", transDist,
                   "fossilGW: ", fossilGW))
  }
  # remove negatives due to rounding imprecision
  cropareaRainfed[cropareaRainfed < 0] <- 0

  ### Yields ###
  # rainfed yield
  rfYld      <- collapseNames(yieldsSingle[, , "rainfed"])
  deltaYldrf <- collapseNames(deltaYields[, , "rainfed"])
  if (grepl("counterfactual", management)) {
    # counterfactual case where irrigated areas are managed under rainfed conditions
    irYld <- collapseNames(yieldsSingle[, , "rainfed"])
    deltaYldir <- collapseNames(deltaYields[, , "rainfed"])
  } else {
    # irrigated yield
    irYld <- collapseNames(yieldsSingle[, , "irrigated"])
    deltaYldir <- collapseNames(deltaYields[, , "irrigated"])
  }

  # Calculate production (quantity x yield) or revenue (quantity x yield x price)
  # Note: the price information is already part of "valued yields"
  production[, , "rainfed"] <- cropareaRainfed * rfYld +
    cropareaRainfed * shrMC[, , "rainfed"] * deltaYldrf
  production[, , "irrigated"] <- cropareaIrrig * irYld +
    cropareaIrrig * shrMC[, , "irrigated"] * deltaYldir

  # Check for NAs and negative values
  if (any(is.na(production))) {
    stop("mrwater::calcCropProductionRevenue produced NA values")
  }
  if (any(round(production, digits = 3) < 0)) {
    stop("mrwater::calcCropProductionRevenue produced negative values")
  }

  return(list(x            = production,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
