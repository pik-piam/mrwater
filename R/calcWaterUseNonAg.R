#' @title calcWaterUseNonAg
#' @description This function extracts non-agricultural water demand
#'
#' @param selectyears        Years to be returned
#' @param datasource         Data source to be used (e.g. WATERGAP2020)
#' @param lpjml              Defines LPJmL version for crop/grass and natveg specific inputs
#' @param seasonality        grper (default): non-agricultural water demand in growing period per year;
#'                           total: non-agricultural water demand throughout the year
#' @param climatetype        Switch between different climate scenarios for calcGrowingPeriod
#' @param harmonType         Type of time smoothing:
#'                           average (average over 8-year time span around baseline year) or
#'                           spline (time smoothing using spline method with 4 degrees of freedom) or
#'                           NULL (no smoothing)
#' @param usetype            water use types (domestic, industry, electricity)
#'                           and option to return withdrawals or consumption
#'                           separated by ":" (e.g. "all:withdrawal")
#'                           options for first argument:
#'                           "total" (returns the sum over different
#'                           water use types) or
#'                           "all" ( returns all water use types
#'                           (domestic, industry, electricity))
#'                           options for second argument: "all", "withdrawal", "consumption"
#'
#' @param cells              Number of cells to be returned
#'                           (select "magpiecell" for 59199 cells or "lpjcell" for 67420 cells)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterUseNonAg", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput readSource toolTimeAverage toolTimeSpline toolFillYears
#' @importFrom magclass new.magpie getYears getCells getSets setYears dimOrder
#' @importFrom mstools toolCell2isoCell toolCoord2Isocell toolGetMappingCoord2Country toolHarmonize2Baseline
#' @importFrom magpiesets addLocation findset

calcWaterUseNonAg <- function(selectyears = seq(1995, 2100, by = 5), cells = "lpjcell", # nolint: cyclocomp_linter
                              datasource = "WATCH_ISIMIP_WATERGAP", usetype = "all",
                              seasonality = "grper", harmonType = "average",
                              lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                                        crop = "ggcmi_phase3_nchecks_9ca735cb"),
                              climatetype = "GSWP3-W5E5:historical") {
  # Set up size limit
  local_options(magclass_sizeLimit = 1e+12)

  # Extract arguments
  waterusetype    <- strsplit(usetype, split = ":")[[1]][1]
  abstractiontype <- strsplit(usetype, split = ":")[[1]][2]

  # Cell mapping
  map         <- toolGetMappingCoord2Country()
  selectcells <- map$coords

  ### Old Non-Agricultural Waterdemand data (old default): ###
  if (datasource == "WATCH_ISIMIP_WATERGAP") {
    # Read in nonagricultural water demand:
    watdemNonAg <- readSource("WATERGAP", convert = "onlycorrect", subtype = datasource)
    # iso cell names
    watdemNonAg <- toolCell2isoCell(watdemNonAg)
    getSets(watdemNonAg) <- c("iso", "cell", "year", "scenario", "use")
    # Add year 2100
    watdemNonAg <- toolFillYears(watdemNonAg,
                                 seq(getYears(watdemNonAg, as.integer = TRUE)[1], 2100, by = 5))

  } else if (datasource == "ISIMIP") {
    # ISIMIP non-agricultural water use data (multi-model mean from H08, WaterGAP and PCR-GLOBWB)
    # Since this data is only available until 2050, the values should are kept constant from 2050 onwards.
    # Industry in ISIMIP includes both electricity & domestic
    watdemISIMIPhist   <- readSource("ISIMIPinputs",
                                     subtype = "ISIMIP3b:water:histsoc.waterabstraction",
                                     convert = "onlycorrect")
    watdemISIMIPfuture <- readSource("ISIMIPinputs",
                                     subtype = "ISIMIP3b:water:2015soc.waterabstraction",
                                     convert = "onlycorrect")

    # Subset data to 67420 cells
    watdemISIMIPhist   <- watdemISIMIPhist[selectcells, , ]
    watdemISIMIPfuture <- watdemISIMIPfuture[selectcells, , ]

    # Combine historical and future ISIMIP data:
    watdemISIMIP <- mbind(watdemISIMIPhist, watdemISIMIPfuture)

    # Ref_year: 2010 because both ISIMIP historical (available until 2014) and WATERGAP (available from 2005)
    baseyear  <- "y2010"
    yearsHist <- getYears(watdemISIMIPhist)

    # Time smoothing
    if (harmonType == "average" || harmonType == "spline") {
      watdemISIMIP   <- toolSmooth(watdemISIMIP, method = harmonType)
    } else if (is.null(harmonType) || harmonType == "raw") {
      watdemISIMIP   <- watdemISIMIP
    } else {
      stop("Please select time smoothing method (spline or average)
           for case of harmonization")
    }

    # Reduce size (cut historical years)
    watdemNonAg <- watdemISIMIP[, setdiff(getYears(watdemISIMIP), paste0("y", c(1901:1964))), ]

  } else if (datasource == "WATERGAP2020") {
    # Read in WATERGAP non-agricultural water abstractions:
    watdemWATERGAP <- readSource("WATERGAP", subtype = "WATERGAP2020", convert = "onlycorrect")
    # Default cell order
    watdemNonAg    <- watdemWATERGAP[selectcells, , ]

    if (harmonType == "average" || harmonType == "spline") {
      watdemNonAg   <- toolSmooth(watdemNonAg, method = harmonType)
    } else if (is.null(harmonType) || harmonType == "raw") {
      watdemNonAg   <- watdemNonAg
    } else {
      stop("Please select time smoothing method (spline or average)
           for case of harmonization")
    }

  } else if (datasource == "WATERGAP_ISIMIP") {

    if (harmonType == "average" || harmonType == "spline") {
      # Read in ISIMIP non-agricultural water abstractions:
      watdemISIMIP   <- calcOutput("WaterUseNonAg", datasource = "ISIMIP", cells = "lpjcell",
                                   selectyears = "all", seasonality = "total", usetype = "all:all",
                                   harmonType = harmonType, lpjml = lpjml, climatetype = climatetype,
                                   aggregate = FALSE)

      # Read in ISIMIP non-agricultural water abstractions:
      watdemWATERGAP <- calcOutput("WaterUseNonAg", datasource = "WATERGAP2020", cells = "lpjcell",
                                   selectyears = "all", seasonality = "total", usetype = "all:all",
                                   harmonType = harmonType, lpjml = lpjml, climatetype = climatetype,
                                   aggregate = FALSE)
    } else {
      stop("Please select time smoothing method (spline or average)
           for case of harmonization")
    }

    ### Harmonize WATERGAP and ISIMIP data (WATERGAP trends scaled to ISIMIP historical data)
    # Ref_year: 2010 because both ISIMIP historical (available until 2014) and WATERGAP (available from 2005)
    baseyear        <- "y2010"
    yearsHarmonized <- paste0("y", seq(2010, 2020))
    yearsWATERGAP   <- getYears(watdemWATERGAP)
    yISIMIP         <- as.numeric(gsub("y", "", getYears(watdemISIMIP)))
    yBase           <- as.numeric(gsub("y", "", baseyear))
    yearsHist       <- setdiff(getYears(watdemISIMIP),
                               getYears(watdemISIMIP)[yISIMIP > yBase])

    # Note: ISIMIP industry data = manufacturing + electricity
    # Store WATERGAP share of manufacturing and electricity of industry
    watdemIndustry   <- collapseNames(watdemWATERGAP[, , "manufacturing"]) +
      collapseNames(watdemWATERGAP[, , "electricity"])
    shrManufacturing <- ifelse(watdemIndustry > 0,
                               watdemWATERGAP[, , "manufacturing"] / watdemIndustry,
                               0)
    shrManufacturing <- dimOrder(shrManufacturing, perm = c(1, 3, 2), dim = 3)
    shrElectricity   <- ifelse(watdemIndustry > 0,
                               watdemWATERGAP[, , "electricity"] / watdemIndustry,
                               0)
    shrElectricity   <- dimOrder(shrElectricity, perm = c(1, 3, 2), dim = 3)

    # Harmonization routine (split for memory reasons)
    tmpWATERGAP <- new.magpie(cells_and_regions = getCells(watdemWATERGAP),
                              years = yearsWATERGAP,
                              names = getNames(watdemISIMIP))
    getSets(tmpWATERGAP) <- c("x", "y", "iso", "year", "use", "wtype")

    scenarios  <- getNames(watdemWATERGAP, dim = "scenario")
    listMAgPIE <- vector(mode = "list", length = length(scenarios))
    i          <- 0
    for (scenario in scenarios) {
      # WATERGAP data
      tmpWATERGAP[, , "industry"] <- watdemIndustry[, , scenario]
      tmpWATERGAP[, , "domestic"] <- collapseNames(watdemWATERGAP[, , "domestic"][, , scenario])

      # Harmonize WATERGAP data to ISIMIP baseline for baseyear
      harmonizedWATERGAP <- toolHarmonize2Baseline(x = tmpWATERGAP, base = watdemISIMIP,
                                                   ref_year = baseyear, method = "additive", hard_cut = FALSE)
      harmonizedWATERGAP <- setNames(harmonizedWATERGAP, nm = paste(scenario, getNames(harmonizedWATERGAP), sep = "."))
      getSets(harmonizedWATERGAP) <- c("x", "y", "iso", "year", "scenario", "use", "wtype")

      # Store harmonized data in final object
      tmp <- vector(mode = "list", length = 3)
      tmp[[1]] <- harmonizedWATERGAP[, yearsWATERGAP, "domestic"]
      tmp[[2]] <- shrManufacturing[, yearsWATERGAP, scenario] *
        collapseNames(harmonizedWATERGAP[, yearsWATERGAP, "industry"])
      tmp[[3]] <- shrElectricity[, yearsWATERGAP, scenario] *
        collapseNames(harmonizedWATERGAP[, yearsWATERGAP, "industry"])

      # Store MAgPIE objects in list
      i <- i + 1
      listMAgPIE[[i]] <- mbind(tmp)

    }

    # Combine to one object
    watdemWATERGAP <- mbind(listMAgPIE)
    rm(listMAgPIE, tmp, tmpWATERGAP, harmonizedWATERGAP, watdemIndustry)

    # Harmonize future to last year of common scenario time frame
    yWATERGAP   <- as.numeric(gsub("y", "", yearsWATERGAP))
    yHarmonized <- as.numeric(gsub("y", "", yearsHarmonized[length(yearsHarmonized)]))
    yearsFUTURE <- yearsWATERGAP[yWATERGAP >= yHarmonized]

    tmp <- vector(mode = "list", length = 3)
    i   <- 0
    for (scenario in scenarios) {

      i <- i + 1
      tmp[[i]] <- toolHarmonize2Baseline(x = collapseNames(watdemWATERGAP[, yearsFUTURE, scenario]),
                                         base = collapseNames(watdemWATERGAP[, yearsFUTURE, "ssp2"]),
                                         ref_year = yearsHarmonized[length(yearsHarmonized)],
                                         method = "additive",
                                         hard_cut = FALSE)
      getNames(tmp[[i]]) <- paste(scenario, getNames(tmp[[i]]), sep = ".")
      getSets(tmp[[i]])  <- c("x", "y", "iso", "year", "scenario", "use", "wtype")

    }
    watFUTURE <- mbind(tmp)

    watdemWATERGAP[, yearsFUTURE, ] <- watFUTURE
    rm(tmp, watFUTURE)

    # Data follows common scenario (WATERGAP SSP2) until 2020, then scenarios diverge;
    # Note: ISIMIP stays constant for future
    watdemWATERGAP[, yearsHarmonized, ] <- collapseNames(watdemWATERGAP[, yearsHarmonized, "ssp2"])


    # Reduce number of years (for memory reasons)
    if (all(selectyears != "all")) {

      if (is.numeric(selectyears)) {
        selectyears <- paste0("y", selectyears)
      }

      yearsHist     <- intersect(yearsHist, selectyears)
      yearsWATERGAP <- intersect(yearsWATERGAP, selectyears)

      if (length(yearsWATERGAP) != 0) {
        watdemWATERGAP <- watdemWATERGAP[, yearsWATERGAP, ]
      }
    }

    # historical data provided by ISIMIP (splitting of manufacturing and
    # electricity share based on WATERGAP SSP2 baseyear share)
    tmp                <- vector(mode = "list", length = 3)
    tmp[[1]]           <- watdemISIMIP[, yearsHist, "domestic"]
    tmp[[2]]           <- collapseNames(watdemISIMIP[, yearsHist, "industry"]) *
      collapseNames(setYears(shrManufacturing[, baseyear, "ssp2"], NULL))
    getNames(tmp[[2]]) <- paste("manufacturing", getNames(tmp[[2]]), sep = ".")
    tmp[[3]]           <- collapseNames(watdemISIMIP[, yearsHist, "industry"]) *
      collapseNames(setYears(shrElectricity[, baseyear, "ssp2"], NULL))
    getNames(tmp[[3]]) <- paste("electricity", getNames(tmp[[3]]), sep = ".")

    watdemNonAg <- mbind(tmp)

    # scenario ISIMIP data (split up by water use with WATERGAP scenario manufacturing and
    # electricity shares)
    tmp                <- vector(mode = "list", length = 3)
    tmp[[1]]           <- watdemISIMIP[, yearsWATERGAP, "domestic"]
    tmp[[2]]           <- collapseNames(watdemISIMIP[, yearsWATERGAP, "industry"]) *
      collapseNames(shrManufacturing[, yearsWATERGAP, "ssp2"])
    getNames(tmp[[2]]) <- paste("manufacturing", getNames(tmp[[2]]), sep = ".")
    tmp[[3]]           <- collapseNames(watdemISIMIP[, yearsWATERGAP, "industry"]) *
      collapseNames(shrElectricity[, yearsWATERGAP, "ssp2"])
    getNames(tmp[[3]]) <- paste("electricity", getNames(tmp[[3]]), sep = ".")

    tmp         <- mbind(tmp)
    yearsNoOverlap <- setdiff(getYears(tmp), getYears(watdemNonAg))
    if (!identical(yearsNoOverlap, character(0))) {
      watdemNonAg <- mbind(watdemNonAg, tmp[, setdiff(getYears(tmp), getYears(watdemNonAg)), ])
    }
    watdemNonAg <- add_dimension(watdemNonAg, dim = 3.1, add = "scenario", nm = "ISIMIP")
    rm(watdemISIMIP)

    if (length(yearsHist) != 0) {
      # historical data provided by ISIMIP (same for all scenarios)
      watdemWATERGAP <- add_columns(watdemWATERGAP,
                                    dim = 2,
                                    addnm = setdiff(yearsHist, yearsWATERGAP),
                                    fill = NA)
      watdemWATERGAP <- watdemWATERGAP[, sort(as.numeric(gsub("y", "", getYears(watdemWATERGAP)))), ]

      watdemWATERGAP[, yearsHist, ] <- collapseNames(watdemNonAg[, yearsHist, ])
    }
    rm(shrElectricity, shrManufacturing)

    # Merge to final object
    watdemNonAg <- mbind(watdemNonAg, watdemWATERGAP)
    rm(watdemWATERGAP)

  }

  ###########################################
  ############ Function Output  #############
  ###########################################

  if (datasource != "WATCH_ISIMIP_WATERGAP") {
    # Correct mismatches of withdrawal and consumption (withdrawals > consumption)
    watdemNonAg[, , "withdrawal"]  <- pmax(watdemNonAg[, , "withdrawal"], watdemNonAg[, , "consumption"])
    watdemNonAg[, , "consumption"] <- pmax(watdemNonAg[, , "consumption"], 0.01 * watdemNonAg[, , "withdrawal"])
  }

  ### Non-agricultural water demands in Growing Period
  if (seasonality == "grper") {
    ### Note: Seasonality "grper" will be deleted when we switch to new mrwater preprocessing

    # Get growing days per month
    growDays <- calcOutput("GrowingPeriod", aggregate = FALSE, cells = "lpjcell",
                           lpjml = lpjml, climatetype = climatetype, yield_ratio = 0.1)

    # Growing days per year
    growDays <- dimSums(growDays, dim = 3)

    # Adjust years
    yearsWatdem <- getYears(watdemNonAg)
    yearsGrper  <- getYears(growDays)
    if (length(yearsWatdem) >= length(yearsGrper)) {
      years <- yearsGrper
    } else {
      years <- yearsWatdem
    }
    rm(yearsGrper, yearsWatdem)

    # Calculate non-agricultural water demand in growing period
    out         <- watdemNonAg[, years, ] * growDays[, years, ] / 365
    description <- "Non-agricultural water demand (manufacturing, electricity, domestic) in growing period"

  } else if (seasonality == "total") {
    ### Total non-agricultural water demands per year
    out         <- watdemNonAg
    description <- "Total non-agricultural water demand (manufacturing, electricity, domestic)"

  } else {
    stop("Specify seasonality! grper or total")
  }

  # Sum up over all non-agricultural water uses (domestic, manufacturing, electricity)
  if (waterusetype == "total") {
    out <- dimSums(out, dim = "use")
  }

  # Report withdrawal or consumption only
  if (!is.na(abstractiontype) &&
        (grepl(abstractiontype, "consumption") || grepl(abstractiontype, "withdrawal"))) {
    out <- collapseNames(out[, , abstractiontype])
  }

  # Number of cells to be returned
  if (cells == "magpiecell") {
    out <- toolCoord2Isocell(out)
  } else if (cells == "lpjcell") {
    # Correct cell naming
    out <- out[selectcells, , ]
    getItems(out, dim = 1, raw = TRUE) <- paste(map$coords, map$iso, sep = ".")
    getSets(out, fulldim = FALSE)[1] <- "x.y.iso"
  }

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA watdemNonAg")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
