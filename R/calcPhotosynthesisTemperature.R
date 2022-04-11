#' @title       calcPhotosynthesisTemperature
#' @description This function calculates crop-specific temperature limits for
#'              the multicropping mask based on the photosynthesis optimum
#'              and the LPJmL parameters temp_co2 and temp_photos
#'
#' @param threshold Photosynthesis efficiency threshold (between 0 and 1)
#'
#' @return magpie object
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("PhotosynthesisTemperature", aggregate = FALSE)
#' }
#'
#' @importFrom stats uniroot
#' @importFrom magclass add_dimension as.magpie mbind

calcPhotosynthesisTemperature <- function(threshold = 0.8) {

  # LPJmL crop-specific temperature limits (ToDo: readFunction from LPJmL parameter json file)
  tempCO2LIST    <- list(maize = c(8, 42),
                         tece = c(0, 40),
                         rice = c(6, 55),
                         trce = c(6, 55),
                         pulses = c(-4, 45),
                         tero = c(-4, 45),
                         trro = c(6, 55),
                         sunflower = c(8, 42),
                         soybean = c(5, 45),
                         groundnut = c(0, 40),
                         rapeseed = c(0, 40),
                         sugarcane = c(8, 42))
  tempPhotosLIST <- list(maize = c(21, 26),
                         tece = c(12, 17),
                         rice = c(20, 45),
                         trce = c(20, 45),
                         pulses = c(10, 30),
                         tero = c(10, 30),
                         trro = c(20, 45),
                         sunflower = c(25, 32),
                         soybean = c(28, 32),
                         groundnut = c(12, 17),
                         rapeseed = c(12, 17),
                         sugarcane = c(18, 30))

  # Photosynthesis efficiency at different temperatures
  photosynthesisLevel <- function(temp, tempCO2, tempPhotos) {
    # make sure parameters are sorted correctly
    tempCO2    <- sort(tempCO2, decreasing = FALSE)
    tempPhotos <- sort(tempPhotos, decreasing = FALSE)

    # functional relationship definition according to LPJmL
    k1   <- 2 * log(1 / 0.99 - 1) / (tempCO2[1] - tempPhotos[1])
    k2   <- (tempCO2[1] + tempPhotos[1]) * 0.5
    k3   <- log(0.99 / 0.01) / (tempCO2[2] - tempPhotos[2])
    low  <- 1 / (1 + exp(k1 * (k2 - temp)))
    high <- 1 - 0.01 * exp(k3 * (temp - tempPhotos[2]))

    ifelse(temp <= tempCO2[2], low * high,
           0)
  }

  # Select crop (ToDo: full list of lpjml crop types)
  croplist <- c("maize", "tece", "trce", "rice")
  minTemp  <- list()
  maxTemp  <- list()

  for (crop in croplist) {

    # Select crop
    tempCO2    <- tempCO2LIST[[crop]]
    tempPhotos <- tempPhotosLIST[[crop]]

    # Get the root of the function at the threshold value on both ends
    minTemp[crop] <- uniroot(function(x) photosynthesisLevel(x, tempCO2 = tempCO2, tempPhotos = tempPhotos) - threshold,
                     c(tempCO2[1], tempPhotos[1]))$root
    maxTemp[crop] <- uniroot(function(x) photosynthesisLevel(x, tempCO2 = tempCO2, tempPhotos = tempPhotos) - threshold,
                     c(tempCO2[2], tempPhotos[2]))$root
  }

  # magpie object
  minTemp <- add_dimension(as.magpie(unlist(minTemp)), dim = 3.1,
                           add = "temperature", nm = "min")
  maxTemp <- add_dimension(as.magpie(unlist(maxTemp)), dim = 3.1,
                           add = "temperature", nm = "max")

  # combine to one object
  out <- mbind(minTemp, maxTemp)

  # Checks
  if (any(is.na(out))) {
    stop("calcPhotosynthesisTemperature produced NAs")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "degree Celsius",
              description  = "crop-specific minimum and maximum temperature
                              for efficient photosynthesis",
              isocountries = FALSE))
}
