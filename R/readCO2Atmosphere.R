#' @title readCO2Atmosphere
#' @description Read CO2 global atmospheric concentration
#' @param subtype Switch between different inputs
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Marcos Alves, Kristine Karstens
#' @seealso
#' \code{\link{readCO2Atmosphere}}
#' @examples
#'
#' \dontrun{
#' readSource("CO2Atmosphere", subtype="Elevated.CO2", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @importFrom lucode path

readCO2Atmosphere <-
  function(subtype = "CO2Atmosphere:Elevated.CO2") {
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
      file_name <- files_list[grep("CO2", files_list)]
    
      } else {
      stop(paste("Path", path(folder),
                 "does not exist. Check the defition of your 
          subtype or the folder structure you are trying to access."))
    }
    
    years <-seq()
    
    if (grepl(".dat", file_name)) {
      
      x  <- read.table(path(folder, file_name))
      #id <- match(years, x[, 1])
      #x  <- x[id, ]
      x  <- collapseNames(as.magpie(x))
      getNames(x) <- subtype
      
    } else {
      stop(paste0("Only '.dat' files are supported"))
    }
    
    return(x)
    
  }