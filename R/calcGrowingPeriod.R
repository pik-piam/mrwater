#' @title calcGrowingPeriod
#' @description This function determines a mean sowing date and a mean growing period for each cell
#' in order to determine when irrigation can take place.
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @param stage Degree of processing: raw, smoothed, harmonized, harmonized2020
#' @param yield_ratio threshold for cell yield over global average. crops in cells below threshold will be ignored
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("GrowingPeriod", aggregate = FALSE)
#' }
#'
#' @importFrom madrat toolGetMapping toolAggregate
#' @importFrom magclass collapseNames getItems new.magpie getYears dimSums magpie_expand
#' @importFrom mrcommons toolHarmonize2Baseline toolSmooth toolLPJmLVersion
#'
#' @export

calcGrowingPeriod <- function(lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop = "ggcmi_phase3_nchecks_9ca735cb"),
                              climatetype = "GSWP3-W5E5:historical", stage = "harmonized2020", yield_ratio = 0.1) {

  cfg <- toolLPJmLVersion(version = lpjml["natveg"], climatetype = climatetype)

  if (stage %in% c("raw", "smoothed")) {

    ####################################################################################
    # Goal: calculate mean sowing date and growing period
    # Step 1 Take care of inconsistencies (harvest date or sowing date ==0 etc)
    #        and convert to magpie crop functional types
    # Step 2 Remove wintercrops from both calculations for the northern hemisphere: sowd>180, hard<sowd
    # Step 3 Remove crops that have an irrigated yield below 10% of global average
    #        (total cell area as aggregation weight for global value)
    # Step 4 Calculate growing period with the remaining crops
    # Step 5 Remove sowd1 for sowing date calculation
    # Step 6 Calculate mean sowing date
    # Step 7 Set the sowd to 1 and growing period to 365 where dams are present and
    #        where they are NA (reflecting that all crops have been eliminated)
    # Step 8 Calculate the growing days per month for each cell and each year

    ####################################################################################

    ####################################################################################
    # Read sowing and harvest date input (new for LPJmL5)
    ####################################################################################

    LPJ2MAG      <- toolGetMapping("MAgPIE_LPJmL.csv",
                                    type = "sectoral",
                                    where = "mappingfolder")

    # Read yields first
    yields <- toolCoord2Isocell(collapseNames(calcOutput("LPJmL_new", version = lpjml["crop"],
                                                          climatetype = climatetype, subtype = "harvest",
                                                          stage = "raw", aggregate = FALSE)[, , "irrigated"]))

    # Load Sowing dates from LPJmL (use just rainfed dates since they do not differ for irrigated and rainfed)
    sowd   <- toolCoord2Isocell(collapseNames(calcOutput("LPJmL_new", version = lpjml["crop"],
                                                          climatetype = climatetype,  subtype = "sdate",
                                                          stage = "raw", aggregate = FALSE)[, , "rainfed"]))
    hard   <- toolCoord2Isocell(collapseNames(calcOutput("LPJmL_new", version = lpjml["crop"],
                                                          climatetype = climatetype,  subtype = "hdate",
                                                          stage = "raw", aggregate = FALSE)[, , "rainfed"]))

    goodCrops <- LPJ2MAG$MAgPIE[which(LPJ2MAG$LPJmL %in% getItems(sowd, dim = 3))]
    badCrops  <- LPJ2MAG$MAgPIE[which(!LPJ2MAG$LPJmL %in% getItems(sowd, dim = 3))]

    sowd   <- toolAggregate(sowd, rel = LPJ2MAG,
                            from = "LPJmL", to = "MAgPIE",
                            dim = 3.1, partrel = TRUE)
    hard   <- toolAggregate(hard, rel = LPJ2MAG,
                            from = "LPJmL", to = "MAgPIE",
                            dim = 3.1, partrel = TRUE)
    yields <- toolAggregate(yields, rel = LPJ2MAG,
                            from = "LPJmL", to = "MAgPIE",
                            dim = 3.1, partrel = TRUE)

    if (length(badCrops) > 0) vcat(2, "No information on the growing period found for those crops: ",
                                    paste(unique(badCrops), collapse = ", "))

    #####################################################################################

    ####################################################################################
    # Step 1 Take care of inconsistencies (hard==0 etc)
    ####################################################################################

    # Set sowd to 1 and hard to 365 where either sowd or hard are 0
    sowd[which(hard == 0 | sowd == 0)] <- 1
    hard[which(hard == 0 | sowd == 0)] <- 365

    # Set hard to sowd-1 where sowd and hard are equal
    hard[which(hard == sowd & sowd > 1)]  <- sowd[which(hard == sowd & sowd > 1)] - 1 ### WHY???
    hard[which(hard == sowd & sowd == 1)] <- 365

    ####################################################################################

    ####################################################################################
    # Step 2 remove crops that have an irrigated yield below 10% of global average
    #        (total cell area as aggregation weight)
    ####################################################################################

    area   <- dimSums(calcOutput("LUH2v2", cellular = TRUE,
                                 aggregate = FALSE, years = "y1995"),
                      dim = 3)
    yields <- collapseNames(yields[, , goodCrops])

    cell2GLO     <- array(c(getItems(yields, dim = 1),
                          rep("GLO", 59199)), dim = c(59199, 2))
    glo_yields   <- toolAggregate(yields, cell2GLO, weight = setYears(area, NULL))
    ratio_yields <- yields / glo_yields

    rm_lowyield   <- yields
    rm_lowyield[] <- 1
    rm_lowyield[ratio_yields < 0.1] <- NA

    rm(ratio_yields, yields, area, glo_yields)

    ####################################################################################

    ####################################################################################
    # Step 3 remove wintercrops from both calculations for the northern hemisphere: sowd>180, hard>365
    ####################################################################################

    cells_northern_hemisphere <- which(magpie_coord[, 2] > 0)
    rm_wintercrops            <- new.magpie(cells_and_regions = getCells(sowd),
                                            years = getYears(sowd),
                                            names = getNames(sowd),
                                            sets = c("region", "year", "crop"),
                                            fill = 1)

    # define all crops sowed after 180 days and where sowing date is after harvest date as wintercrops
    rm_wintercrops[cells_northern_hemisphere, , ] <-
      ifelse(sowd[cells_northern_hemisphere, , ] > 180 &
               hard[cells_northern_hemisphere, , ] < sowd[cells_northern_hemisphere, , ],
             NA, 1)

    ####################################################################################

    ####################################################################################
    # Step 4 Calculate mean growing period with the remaining crops
    ####################################################################################

    # calculate growing period as difference of sowing date to harvest date
    # sowd <- sowd[,years,]
    # hard <- hard[,years,]
    grow_period <- hard - sowd
    grow_period[which(hard < sowd)] <- 365 + grow_period[which(hard < sowd)]

    # calculate the mean after removing the before determined winter- and low yielding crops
    # rm_wintercrops <- rm_wintercrops[,years,]
    # rm_lowyield    <- rm_lowyield[,years,]

    n_crops          <- dimSums(rm_wintercrops * rm_lowyield, dim = 3, na.rm = TRUE)
    meanGrper <- dimSums(grow_period * rm_wintercrops * rm_lowyield, dim = 3, na.rm = TRUE) / n_crops
    meanGrper[is.infinite(meanGrper)] <- NA

    #############################################################################

    ####################################################################################
    # Step 5 remove sowd1 for sowing date calculation
    ####################################################################################

    rm_sowd1          <- sowd
    rm_sowd1[]        <- 1
    rm_sowd1[sowd == 1] <- NA

    ####################################################################################

    ####################################################################################
    # Step 6 Calculate mean sowing date
    ####################################################################################

    n_crops   <- dimSums(rm_wintercrops * rm_lowyield * rm_sowd1, dim = 3, na.rm = TRUE)
    meanSowd <- dimSums(grow_period * rm_wintercrops * rm_lowyield * rm_sowd1, dim = 3, na.rm = TRUE) / n_crops
    meanSowd[is.infinite(meanSowd)] <- NA

    rm(rm_wintercrops, rm_lowyield, rm_sowd1)
    rm(sowd, hard, grow_period)
    ####################################################################################

    ####################################################################################
    # Step 7 Set the sowd to 1 and growing period to 365 where dams are present and where they are NA
    #        (reflecting that all crops have been eliminated).
    ####################################################################################

    dams <- readSource("Dams", convert = "onlycorrect")

    for (t in getYears(meanSowd)) {
      meanSowd[which(dams == 1), t]        <- 1
      meanGrper[which(dams == 1), t] <- 365
    }

    meanSowd[is.na(meanSowd)]               <- 1
    meanGrper[is.na(meanGrper)] <- 365
    meanSowd         <- round(meanSowd)
    meanGrper  <- round(meanGrper)

    ####################################################################################

    ####################################################################################
    # Step 8 Calculate the growing days per month for each cell and each year
    ####################################################################################

    month        <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    monthLength  <- c(31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31)
    names(monthLength) <- month

    # Determine which day belongs to which month
    days_months        <- 1:365
    names(days_months) <- 1:365

    before <- 0
    for (i in 1:length(monthLength)) {
      days_months[(before + 1):(before + monthLength[i])]        <- i
      names(days_months)[(before + 1):(before + monthLength[i])] <- names(monthLength)[i]
      before <- before + monthLength[i]
    }

    # mag object for the growing days per month
    grow_days_per_month <- new.magpie(cells_and_regions = getCells(meanSowd),
                                      years = getYears(meanSowd),
                                      names = month,
                                      fill = 0)

    # determine the harvest day, take care if it is greater than 365
    meanHard <- (meanSowd + meanGrper - 1) %% 365
    meanHard[meanHard == 0] <- 365

    meanHard <- as.array(meanHard)
    meanSowd <- as.array(meanSowd)
    grow_days_per_month <- as.array(grow_days_per_month)

    # Loop over the months to set the number of days that the growing period lasts in each month
    for (t in getYears(meanSowd)) {

      # goodcells are cells in which harvest date is after sowing date,
      # i.e. the cropping period does not cross the beginning of the year
      goodcells  <- ifelse(meanHard[, t, ] >= meanSowd[, t, ], 1, 0)
      badcells   <- ifelse(meanHard[, t, ] >= meanSowd[, t, ], 0, 1)

      for (month in 1:12) {
        lastMonthday  <- which(days_months == month)[length(which(days_months == month))]
        firstMonthday <- which(days_months == month)[1]
        test_harvest_goodcells <- as.array(meanHard[, t, ] - firstMonthday + 1)
        days_in_this_month_goodcells <- as.array(lastMonthday - meanSowd[, t, ] + 1)
        days_in_this_month_goodcells[days_in_this_month_goodcells < 0] <- 0 # Month before sowing date
        days_in_this_month_goodcells[days_in_this_month_goodcells > monthLength[month]] <- monthLength[month] # Month is completely after sowing date
        days_in_this_month_goodcells[test_harvest_goodcells < 0] <- 0 # Month lies after harvest date
        days_in_this_month_goodcells[test_harvest_goodcells > 0 & test_harvest_goodcells < monthLength[month]] <- days_in_this_month_goodcells[test_harvest_goodcells > 0 & test_harvest_goodcells < monthLength[month]] - (lastMonthday - meanHard[test_harvest_goodcells > 0 & test_harvest_goodcells < monthLength[month], t, ]) # Harvest date lies in the month. cut off the end of the month after harvest date
        days_in_this_month_goodcells <- days_in_this_month_goodcells <- days_in_this_month_goodcells * goodcells
        days_in_this_month_badcells_firstyear <- as.array(lastMonthday - meanSowd[, t, ] + 1)
        days_in_this_month_badcells_firstyear[days_in_this_month_badcells_firstyear < 0] <- 0 # Month before sowing date
        days_in_this_month_badcells_firstyear[days_in_this_month_badcells_firstyear > monthLength[month]] <- monthLength[month] # Month is completely after sowing date
        days_in_this_month_badcells_secondyear <- as.array(meanHard[, t, ] - firstMonthday + 1)
        days_in_this_month_badcells_secondyear[days_in_this_month_badcells_secondyear < 0] <- 0 # Month lies completely after harvest day
        days_in_this_month_badcells_secondyear[days_in_this_month_badcells_secondyear > monthLength[month]] <- monthLength[month] # Month lies completely before harvest day
        days_in_this_month_badcells <- (days_in_this_month_badcells_firstyear + days_in_this_month_badcells_secondyear) * badcells

        grow_days_per_month[, t, month] <- days_in_this_month_goodcells + days_in_this_month_badcells
      }
    }

    out <- as.magpie(grow_days_per_month)

    if (any(is.na(out))) {
      stop("calcGrowingPeriod produced NAs")
    }

    if (stage == "smoothed") {
      # Time smoothing
      out <- toolSmooth(out)
      # replace values above days of a month with days of the month & negative values with 0
      month        <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
      monthLength <- c(31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31)
      names(monthLength) <- month
      out[out > as.magpie(monthLength)] <- magpie_expand(as.magpie(monthLength), out)[out > as.magpie(monthLength)]
      out[out < 0] <- 0

    }

  } else if (stage == "harmonized") {

    # load smoothed data
    baseline <- calcOutput("GrowingPeriod", lpjml = lpjml, climatetype = cfg$baseline_hist,
                           stage = "smoothed", yield_ratio = yield_ratio, aggregate = FALSE)

    if (climatetype == cfg$baseline_hist) {

      out <- baseline

    } else {

      x        <- calcOutput("GrowingPeriod", lpjml = lpjml, climatetype = climatetype,
                             stage = "smoothed", yield_ratio = yield_ratio, aggregate = FALSE)
      # Harmonize to baseline
      out <- toolHarmonize2Baseline(x = x, base = baseline, ref_year = cfg$ref_year_hist)
    }

  } else if (stage == "harmonized2020") {

    # read in historical data for subtype
    baseline2020    <- calcOutput("GrowingPeriod", lpjml = lpjml, climatetype = cfg$baseline_gcm,
                                  stage = "harmonized", yield_ratio = yield_ratio, aggregate = FALSE)

    if (climatetype == cfg$baseline_gcm) {
      out <- baseline2020

    } else {

      x        <- calcOutput("GrowingPeriod", lpjml = lpjml, climatetype = climatetype,
                             stage = "smoothed", yield_ratio = yield_ratio, aggregate = FALSE)
      out <- toolHarmonize2Baseline(x, baseline2020, ref_year = cfg$ref_year_gcm)
    }

  } else {
    stop("Stage argument not supported!")
 }

  # replace values above days of a month with days of the month & negative values with 0
  month        <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  monthLength <- c(31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31)
  names(monthLength) <- month
  out[out > as.magpie(monthLength)] <- magpie_expand(as.magpie(monthLength), out)[out > as.magpie(monthLength)]
  out[out < 0] <- 0

  return(list(x            = out,
              weight       = NULL,
              unit         = "days",
              description  = "Growing days per month",
              isocountries = FALSE))

}
