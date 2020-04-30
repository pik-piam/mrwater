#' @title readGCMClimate
#' @description Read Climate data used as LPJmL inputs into MAgPIE objects
#' @param subtype Switch between different inputs
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Marcos Alves, Kristine Karstens
#' @seealso
#' \code{\link{readGCMClimate}}
#' @examples
#'
#' \dontrun{
#' readSource("GCMClimate", subtype="HadGEM2:rcp85.temperature", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @import dplyr
#' @importFrom lpjclass read.LPJ_input
#' @importFrom lucode path
#' @export readGCMClimate

readGCMClimate <-
  function(subtype = "GCMClimate:rcp85:HadGEM2.temperature") {
    if (grepl("\\.", subtype) & grepl("\\:", subtype)) {
      subtype     <- strsplit(gsub(":", "/" , subtype), split = "\\.")
      folder      <- unlist(subtype)[1]
      subtype     <- unlist(subtype)[2]

    } else {

      stop("Subtype did not define either: RCP, climate model or file. Please check the defintion of your subtype parameter.")

    }

    if (exists(path(folder))) {
      files_list <- list.files(path(folder))
      files <-
        c(temperature           = files_list[grep("tas", files_list)],
          precipitation         = files_list[grep("pr", files_list)],
          longwave_radiation    = files_list[grep("lwnet", files_list)],
          shortwave_radiation   = files_list[grep("rsds", files_list)],
          wetdays               = files_list[grep("wet", files_list)])
    } else {

      stop(paste("Path", path(folder), "does not exist. Check the defition of your subtype or the folder structure you are trying to access."))

    }

    file_name <- toolSubtypeSelect(subtype, files)

    years <- as.numeric(unlist(regmatches(file_name, gregexpr("\\d{4}",file_name))))
    years <- seq(years[1],years[2],1)

    if (subtype %in% c("temperature", "precipitation", "wetdays")) {
      x <- read.LPJ_input(
        file_name = path(folder, file_name),
        out_years = paste0("y", years),
        namesum = TRUE,
        ncells = 59199
      )

      x <- collapseNames(as.magpie(x))
      x <- x / 12
      getNames(x) <- subtype

    } else if (subtype %in% c("longwave_radiation", "shortwave_radiation")) {
      x <- read.LPJ_input(
        file_name = path(folder, file_name),
        out_years = paste0("y", years),
        namesum = TRUE,
        ncells = 59199
      )

      x <- collapseNames(as.magpie(x))
      x <- x / 365
      getNames(x) <- subtype

    } else {
      stop(paste0("subtype ", subtype, " is not existing"))
    }

    return(x)

  }
