#' @title readSoilClassification
#' @description Read soil classification
#' @param subtype Switch between different inputs
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Marcos Alves, Kristine Karstens
#' @seealso
#' \code{\link{readSoilClassification}}
#' @examples
#'
#' \dontrun{
#' readSource("SoilClassification", subtype="HWSD.soil", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @import dplyr
#' @importFrom lucode path

readSoilClassification <-
  function(subtype = "SoilClassification:HWSD.soil") {
    if (grepl("\\.", subtype) & grepl("\\:", subtype)) {
      subtype     <- strsplit(gsub(":", "/" , subtype), split = "\\.")
      folder      <- unlist(subtype)[1]
      subtype     <- unlist(subtype)[2]
      
    } else {
      
      stop("Subtype did not define either: soil clasification source or file. 
            Please check the defintion of your subtype parameter.")
      
    }
    
    if (exists(path(folder))) {
      files_list <- list.files(path(folder))
      files <- c(soil = files_list[grep("soil", files_list)],
                 grid = files_list[grep("grid", files_list)])
    } else {
      stop(paste("Path", path(folder),
          "does not exist. Check the defition of your 
          subtype or the folder structure you are trying to access."))
    }
    
    file_name <- toolSubtypeSelect(subtype, files)
    
    
    if (subtype %in% "soil") {
      
      grid = readRDS(path(folder, files["grid"]))
      cell_mapping <- toolGetMapping(name = "CountrytoCellMApping.csv", type = "cell")
      
      sk <- file(path(folder, file_name), "rb")
      y  <- readBin(sk, integer(), n = 67420, size = 1)
      close(sk)
      
      years <- seq(1900,2099,1)
      
      x  <-
        array(NA,
              dim = c(67420, length(years)),
              dimnames = list(1:67420, paste0("y", years)))
      for (i in paste0("y", years)) {
        x[, i] <- y
      }
      
      x <- cbind(grid, x)
      x <- left_join(cell_mapping, x, by = c("lon", "lat"))
      x <- as.magpie(x[,-c(1, 3, 4, 5, 6)])
      x <- collapseNames(as.magpie(x))
      getNames(x) <- subtype
      
    } else {
      stop(paste0("subtype ", subtype, " is not existing"))
    }
    
    return(x)
    
  }