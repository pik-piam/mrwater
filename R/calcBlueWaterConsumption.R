#' @title       calcBlueWaterConsumption
#' @description This function calculates consumptive blue water use for the whole year based on
#'              LPJmL blue water consumption of crops and the difference between rainfed and irrigated
#'              evapotranspiration of grass
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Climate model or historical baseline "GSWP3-W5E5:historical"
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("BlueWaterConsumption", aggregate = FALSE)
#' }
#'
#' @importFrom magclass collapseNames collapseDim getYears getCells getNames new.magpie add_dimension
#' @importFrom madrat calcOutput toolAggregate toolGetMapping
#' @importFrom mrcommons toolCell2isoCell

calcBlueWaterConsumption <- function(selectyears, lpjml, climatetype, multicropping) {

  ##############################
  ######## Read in data ########
  ##############################
  # Monthly grass evapotranspiration (ET) under irrigation
  grassET_ir <- calcOutput("LPJmL_new", subtype = "met_grass_ir",
                           version = lpjml["crop"], climatetype = climatetype,
                           stage = "raw", years = selectyears,
                           aggregate = FALSE)
  # ToDo change stage!!! (only raw until I have full LPJmL runs!!!)
  #### @KRISTINE: stage: smoothed or harmonized2020???

  # Monthly grass ET under rainfed conditions
  grassET_rf <- calcOutput("LPJmL_new", subtype = "met_grass_rf",
                           version = lpjml["crop"], climatetype = climatetype,
                           stage = "raw", years = selectyears,
                           aggregate = FALSE)
  # ToDo change stage!!! (only raw until I have full LPJmL runs!!!)

  # Irrigated grass ET in rainfed/irrigated growing period of crop
  cft_et_grass_ir <- calcOutput("LPJmL_new", subtype = "cft_et_grass_ir",
                           version = lpjml["crop"], climatetype = climatetype,
                           stage = "raw", years = selectyears,
                           aggregate = FALSE)
  # ToDo change stage!!! (only raw until I have full LPJmL runs!!!)

  # Rainfed grass ET in rainfed/irrigated growing period of crop
  cft_et_grass_rf <- calcOutput("LPJmL_new", subtype = "cft_et_grass_rf",
                                version = lpjml["crop"], climatetype = climatetype,
                                stage = "raw", years = selectyears,
                                aggregate = FALSE)
  # ToDo change stage!!! (only raw until I have full LPJmL runs!!!)



  ##############################
  ######## Calculations ########
  ##############################
  # Annual grass ET
  grassET_ir <- dimSums(grassET_ir, dim = 3)
  grassET_rf <- dimSums(grassET_rf, dim = 3)

  # Delta ET (irrigated ET - rainfed ET) as proxy for blue water consumption of grass
  delta_grassET_annual <- grassET_ir - grassET_rf

# temporary
  out <- delta_grassET_annual

#
#
#   ### Water requirements ###
#
#
#   # Delta AET as proxy for blue water consumption
#   # growing period grass ET
#   delta_cft_et_grass <- data.cft_et_grass[,,"ir"] - data.cft_et_grass[,,"rf"]
#   # annual grass ET
#   delta_aet_grass    <- data.aet_grass[,,"ir"] - data.aet_grass[,,"rf"]
#
#   jpeg(filename = paste0(outputdir, "/ScatterIrrigatedET_", cft, ".jpeg"))
#   plot(x = delta_cft_et_grass, y = data.cft_bcons,
#        pch = 19, cex = 0.2,
#        xlab = "Delta ET grass (irr ET - rf ET)", ylab = paste0("Blue water consumption of ", cft),
#        main = paste0("RSE: ", round(summary(lm(as.vector(data.cft_bcons) ~ as.vector(delta_cft_et_grass) + 0))$sigma, digits = 2)),
#        sub = paste0("Adj. R-squared: ", round(summary(lm(as.vector(data.cft_bcons) ~ as.vector(delta_cft_et_grass) + 0))$adj.r.squared, digits = 2)))
#   abline(lm(as.vector(data.cft_bcons) ~ as.vector(delta_cft_et_grass) + 0),
#          col = "blue", main = "fitted")
#   dev.off()
#
#   # Calculate crop water use
#   cft_et_grass_2nd <- ifelse(data.cft_gpp_grass_ir[,,"ir"]>100 & data.agpp_grass[,,"ir"]/data.cft_gpp_grass_ir[,,"ir"]>2,
#                              0.75 * (delta_aet_grass - delta_cft_et_grass),
#                              0)
#
#   et_grass_to_cft_bcons <- lm(y ~ x + 0, data = data.frame(y = as.vector(data.cft_bcons),
#                                                            x = as.vector(delta_cft_et_grass)))$coefficients[1]
#
#   cft_bcons_2nd <- et_grass_to_cft_bcons * cft_et_grass_2nd
#
#   cft_bcons_1st   <- data.cft_bcons
#   cft_bcons_total <- cft_bcons_1st + cft_bcons_2nd
#
#
#   jpeg(filename = paste0(outputdir, "/BWCmain_", cft, ".jpeg"))
#   image.plot(cft_bcons_1st,
#              main = paste0("Blue water consumption (main season) for ", cft),
#              legend.lab = "liter/m^2")
#   dev.off()
#
#   jpeg(filename = paste0(outputdir, "/BWCtotal_", cft, ".jpeg"))
#   image.plot(cft_bcons_total,
#              main = paste0("Blue water consumption (whole year) for ", cft),
#              legend.lab = "liter/m^2")
#   dev.off()
#
#
#
#
#
#   ## Evapotranspiration ##
#   subtype <- "LPJmL4_for_MAgPIE_44ac93de:GSWP3-W5E5:historical:met_grass_ir"
#   mgpp_et_ir <- readSource("LPJmL_new", subtype = subtype, convert = FALSE)
#
#   subtype <- "LPJmL4_for_MAgPIE_44ac93de:GSWP3-W5E5:historical:met_grass_rf"
#   mgpp_et_rf <- readSource("LPJmL_new", subtype = subtype, convert = FALSE)
#
#
#   subtype <- "LPJmL4_for_MAgPIE_44ac93de:GSWP3-W5E5:historical:cft_et_grass_ir"
#   cft_et_grass_ir <- readSource("LPJmL_new", subtype = subtype, convert = FALSE)
#
#   subtype <- "LPJmL4_for_MAgPIE_44ac93de:GSWP3-W5E5:historical:cft_et_grass_rf"
#   cft_et_grass_rf <- readSource("LPJmL_new", subtype = subtype, convert = FALSE)
#
#
#
#
#
#
#   ### Mappings
#   lpj2mag <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")
#
#   ### Read in blue water consumption for irrigated crops (in m^3 per ha per yr):
#   bwc <- collapseNames(setYears(calcOutput("LPJmL_new", subtype = "cwater_b",
#                                             version = lpjml["crop"], climatetype = climatetype, stage = "smoothed",
#                                             aggregate = FALSE, years = selectyears),
#                                 selectyears))
#   names(dimnames(bwc))[3] <- "crop"
#   years                   <- getYears(bwc)
#   cropnames               <- getNames(bwc)
#   systemnames             <- c("drip", "sprinkler", "surface")
#
#   # Seasonality dimension
#   bwc   <- add_dimension(bwc, dim = 3.2, add = "season", nm = c("first", "second"))
#
#   if (multicropping) {
#
#     # Reduce to areas where multicropping is relevant based on Multiple Cropping Zones
#     mc <- calcOutput("MultipleCroppingZones", layers = 2, aggregate = FALSE)
#     mc <- collapseNames(mc[, , "irrigated"])
#
#     # Irrigation water requirements of main-season ("first") and off-season ("second"):
#     ratio <- calcOutput("MultipleCroppingWatRatio", selectyears = selectyears,
#                         lpjml = lpjml, climatetype = climatetype, aggregate = FALSE)
#     bwc[, , "second"] <- bwc[, , "second"] * ratio * mc
#
#   } else {
#
#     bwc[, , "second"] <- 0
#
#   }
#
#   ### Field efficiencies from Jägermeyr et al. (global values) [placeholder!]
#   #### Use field efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####
#   ### Alternatively: use regional efficiencies from Sauer et al. (2010), Table 5,
#   fieldEff                <- new.magpie(cells_and_regions =  getCells(bwc),
#                                         years = years,
#                                         names = sort(paste(systemnames, rep(cropnames, 3), sep = ".")),
#                                         sets = c("x.y.iso", "year", "system.crop"))
#   fieldEff[, , "drip"]      <- 0.88 # Sauer: 0.8-0.93
#   fieldEff[, , "sprinkler"] <- 0.78 # Sauer: 0.6-0.86
#   fieldEff[, , "surface"]   <- 0.52 # Sauer: 0.25-0.5
#   #### Use field efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####
#
#   ### Conveyance efficiency proxy [placeholder]
#   #### Use conveyance efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####
#   convEff                <- new.magpie(cells_and_regions = getCells(bwc),
#                                        years = years,
#                                        names = sort(paste(systemnames, rep(cropnames, 3), sep = ".")),
#                                        sets = c("x.y.iso", "year", "system.crop"))
#   convEff[, , "drip"]      <- 0.95 # Note: same as in LPJmL (see Schaphoff 2018 p. 1395)
#   convEff[, , "sprinkler"] <- 0.95 # Note: same as in LPJmL (see Schaphoff 2018 p. 1395)
#   convEff[, , "surface"]   <- 0.7
#   #### Use conveyance efficiency from LPJmL here (by system, by crop, on 0.5 degree) [Does it vary by year?] ####
#
#   ##############################
#   ######## Calculations ########
#   ##############################
#
#   # Calculate project efficiency from given field and conveyance efficiencies
#   projectEff <- fieldEff * convEff
#
#   # Water withdrawal = crop water consumption + field losses + conveyance losses
#   watWW      <- bwc / projectEff
#
#   # Conveyance loss (from river to field)
#   convLoss   <- watWW * (1 - convEff)
#
#   # consumptive irrigation water = consumptive plant transpiration + evaporative conveyance loss
#   # (Note: According to Rost et al. (2008) 50% of conveyance loss are evaporative)
#   # ["Half of conveyance losses are assumed to occur due to evaporation from open
#   # water bodies and the remainder is added to the return flow as drainage." (Schaphoff 2018)]
#   watWC      <- bwc + 0.5 * convLoss
#
#   # Output: irrigation water requirements (consumption and withdrawals)
#   irrigReq   <- new.magpie(cells_and_regions = getCells(watWC),
#                            years = getYears(watWC),
#                            names = getNames(watWC),
#                            sets = c("x.y.iso", "year", "crop.season.system"))
#   irrigReq <- add_dimension(irrigReq, dim = 3.4, add = "irrig_type", nm = c("consumption", "withdrawal"))
#   irrigReq[, , "consumption"] <- watWC
#   irrigReq[, , "withdrawal"]  <- watWW
#
#   # Aggregate to MAgPIE crops
#   irrigReq  <- toolAggregate(irrigReq, lpj2mag, from = "LPJmL", to = "MAgPIE",
#                              dim = "crop", partrel = TRUE)
#
#   # Check for NAs and negative values
#   if (any(is.na(irrigReq))) {
#     stop("produced NA irrigation water requirements")
#   }
#   if (any(irrigReq < 0)) {
#     stop("produced negative irrigation water requirements")
#   }

  return(list(x            = out,
              weight       = NULL,
              unit         = "m^3 per ha per yr",
              description  = "Irrigation water requirements for irrigation for
                              different crop types in different seasons
                              under different irrigation systems",
              isocountries = FALSE))
}
