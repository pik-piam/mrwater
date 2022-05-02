#' @title       calcIrrigSystemShr
#' @description This function returns the share of the
#'              irrigation area under a specific irrigation system type
#'              (surface, sprinkler, drip)
#'              per crop type
#'
#' @param iniyear Year to be used for irrigation system share initialization
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigSystemShr", aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource calcOutput toolGetMapping
#' @importFrom magclass getItems add_columns where dimSums

calcIrrigSystemShr <- function(iniyear) {

  ####################
  ### READ IN DATA ###
  ####################
  # irrigated croparea per country
  irrigArea <- dimSums(collapseNames(calcOutput("CropareaAdjusted", iniyear = iniyear,
                                        aggregate = FALSE)[, , "irrigated"]),
                      dim = c("x", "y"))
  countrylist <- getItems(irrigArea, dim = 1)
  years       <- getYears(irrigArea)

  # crop suitability for different irrigation systems
  irrigSuit <- setYears(readSource("Jaegermeyr2015", subtype = "systemSuitability"),
                        years)

  # irrigation system share by country
  irrigShr  <- setYears(readSource("Jaegermeyr2015", subtype = "systemShare")[countrylist, , ],
                        years)

  ######################
  ### TRANSFORM DATA ###
  ######################
  # from LPJmL crops to MAgPIE crops
  map <- toolGetMapping("MAgPIE_LPJmL.csv",
                          type = "sectoral", where = "mappingfolder")
  getItems(irrigSuit, dim = "crop") <- gsub("pastures", "mgrass", getItems(irrigSuit, dim = "crop"))
  # add betr and begr
  irrigSuit <- add_columns(irrigSuit, addnm = c("betr", "begr"),
                           dim = "crop", fill = 1)
  irrigSuit[, , "begr"] <- irrigSuit[, , "mgrass"]
  # remove LPJmL "others" category
  irrigSuit <- irrigSuit[, , "others", invert = TRUE]

  irrigSuit <- toolAggregate(irrigSuit, rel = map,
                            from = "LPJmL", to = "MAgPIE", dim = "crop")

  # remove pasture (not irrigated in MAgPIE)
  irrigSuit <- irrigSuit[, , "pasture", invert = TRUE]

  # Adjust MAgPIE crop types that are not part of original suitability data:
  irrigSuit[, , "others"] <- 1
  irrigSuit[, , "oilpalm"] <- 1
  irrigSuit[, , "potato"] <- 1
  irrigSuit[, , "cottn_pro"] <- 1
  # MAgPIE cassava_sp contains: Sweet potatoes; Cassava; Yautia (cocoyam);
  # Taro (cocoyam); Yams; Roots and tubers; Bananas; Plantains
  irrigSuit[, , "cassav_sp"] <- 1
  ### ToDo: use FAO country-share of cassava in cassav_sp category and assign irrigSystem share accordingly?

  # empty MAgPIE object with correct dimensionality
  out <- irrigShr * irrigSuit
  out[, , ] <- 0

  ####################
  ### CALCULATIONS ###
  ####################
  # all crops
  croplist <- getItems(irrigArea, dim = "crop")
  # crops that area suitable for drip irrigation
  dripCrops <- gsub(".drip", "", where(irrigSuit[, , "drip"] == 1)$true$data)
  # crops that area suitable for sprinkler irrigation
  sprinklerCrops <- gsub(".sprinkler", "", where(irrigSuit[, , "sprinkler"] == 1)$true$data)
  # crops that area suitable for surface irrigation
  surfaceCrops <- gsub(".surface", "", where(irrigSuit[, , "surface"] == 1)$true$data)

  # irrigated area per irrigation system
  systemArea <- dimSums(irrigArea, "crop") * irrigShr


  ## Step 1: grid cells with drip-suitable crops      ##
  ##         assign drip irrigation areas to          ##
  ##         irrigated areas with drip-suitable crops ##

  # area that has crops that are suitable for drip irrigation
  dripSuitArea <- dimSums(irrigArea[, , dripCrops], dim = "crop")
  # area that is irrigated with drip irrigation
  dripArea <- systemArea[, , "drip"]

  shrDrip <- ifelse(dripSuitArea > 0 & dripSuitArea >= dripArea,
                      dripArea / dripSuitArea,
                    1)
  # assign drip irrigation area to drip-suitable crops
  out[, , "drip"][, , dripCrops] <- irrigArea[, , dripCrops] * shrDrip

  # remaining area to be allocated to sprinkler or surface irrigation
  remainingIrrigArea <- irrigArea - dimSums(out, dim = "system")


  ## Step 2: grid cells with sprinkler-suitable crops       ##
  ##         assign sprinkler irrigation areas to remaining ##
  ##         irrigated areas with sprinkler-suitable crops  ##

  # area that has crops that are suitable for sprinkler irrigation
  sprinklerSuitArea <- dimSums(remainingIrrigArea[, , sprinklerCrops], dim = "crop")
  # area that is irrigated with sprinkler irrigation
  sprinklerArea <- systemArea[, , "sprinkler"]

  shrSprinkler <- ifelse(sprinklerSuitArea > 0 & sprinklerSuitArea >= sprinklerArea,
                          sprinklerArea / sprinklerSuitArea,
                         1)
  # assign sprinkler irrigation area to sprinkler-suitable crops
  out[, , "sprinkler"][, , sprinklerCrops] <- remainingIrrigArea[, , sprinklerCrops] * shrSprinkler

  remainingIrrigArea <- irrigArea - dimSums(out, dim = "system")


  ## Step 3: grid cells with surface-suitable crops       ##
  ##         assign surface irrigation areas to remaining ##
  ##         irrigated areas with surface-suitable crops  ##

  # all remaining area is surface irrigated
  out[, , "surface"][, , surfaceCrops] <- remainingIrrigArea[, , surfaceCrops]

  # Check whether all irrigated area has been attributed to a specific irrigation system
  remainingIrrigArea <- irrigArea - dimSums(out, dim = "system")
  if (any(round(remainingIrrigArea, digits = 3) != 0)) {
    stop("Not all irrigated area has been allocated to
          an irrigation system")
  }

  # Check whether irrigation system shares add up to 1
  test <- ifelse(dimSums(irrigArea, dim = "crop") > 0,
                  dimSums(out, dim = "crop") / dimSums(irrigArea, dim = "crop"),
                 0)
  test[dimSums(irrigArea, dim = "crop") == 0] <- irrigShr[dimSums(irrigArea, dim = "crop") == 0]
  getSets(test) <- getSets(irrigShr)
  if (any(round(dimSums(test, dim = 3), digits = 10) != 1)) {
      stop("Problem in calcIrrigSystemShr: sum over shares not equal to 1")
  }

  ## Step 5 Calculate irrigation system share ##
  ##         per crop and country             ##
  out <- ifelse(out[, , croplist] * irrigArea[, , croplist] > 0,
                  out[, , croplist] / irrigArea[, , croplist],
                0)

  ## Step 6: Correct data inconsistencies:
  ##         where irrigated areas in initialization year are 0, ##
  ##         insert irrigShr equally for all crops               ##
  out[dimSums(irrigArea, dim = "crop") == 0, , ] <- irrigShr[dimSums(irrigArea, dim = "crop") == 0, , ]

  # Dimension ordering and naming
  out <- dimOrder(out, perm = c(2, 1), dim = 3)

  ##############
  ### Checks ###
  ##############
  if (any(is.na(out))) {
    stop("Problem in calcIrrigSystemShr: produced NA irrigation system share")
  }
  if (any(round(dimSums(irrigArea, dim = "crop"),
                        digits = 3) != round(dimSums(out * irrigArea,
                                                    dim = 3),
                digits = 3))) {
    stop("Problem in calcIrrigSystemShr:
         The attributed irrigation system shares do not match the irrigated areas")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "share",
              description  = "irrigation system share (share of irrigated area) per crop",
              isocountries = FALSE))
}
