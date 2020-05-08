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
#' @importFrom dplyr left_join
#' @importFrom utils read.csv

readSoilClassification <-
  function(subtype = "SoilClassification:HWSD.soil") {
    if (grepl("\\.", subtype) & grepl("\\:", subtype)) {
      subtype     <- strsplit(gsub(":", "/" , subtype), split = "\\.")
      folder      <- unlist(subtype)[1]
      subtype     <- unlist(subtype)[2]

    } else {
      stop(
        "Subtype did not define either: soil clasification source or file.
            Please check the defintion of your subtype parameter."
      )

    }

    if (exists(folder)) {
      files_list <- list.files(folder)
      files <- c(soil = files_list[grep("soil", files_list)])
    } else {
      stop(
        paste(
          "Path", folder,
          "does not exist. Check the defition of your
          subtype or the folder structure you are trying to access."
        )
      )
    }

    file_name <- toolSubtypeSelect(subtype, files)


    if (subtype %in% "soil") {
      sk <- file(paste0(folder,"/",file_name), "rb")
      y <- readBin(sk, integer(), n = 67460, size = 1)
      close(sk)
      lpjcells <- readRDS("lpjcells.rds")
      y = y[-lpjcells$exclude_cells]
      y = y[lpjcells$lpj_index]
      df.y = data.frame("soil" = y)

      soilpar = read.csv(paste0(folder,"/soilpar.csv"))
      y = left_join(df.y, soilpar, by = "soil")

      years <- seq(1995, 2099, 1)

      y = as.matrix(y)
      x <-
        array(
          NaN,
          dim = c(59199, length(years), dim(soilpar)[2]),
          dimnames = list(1:59199, years, colnames(soilpar))
        )
      for (i in 1:length(years)) {
        x[, i,] <- y
      }
      x <-  collapseNames(as.magpie(x, spatial = 1))
      getNames(x) <- subtype

    } else {
      stop(paste0("subtype ", subtype, " is not existing"))
    }
    return(x)

  }
