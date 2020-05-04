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
#' @importFrom dplyr left_join
#' @importFrom lucode path

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

    if (exists(path(folder))) {
      files_list <- list.files(path(folder))
      files <- c(soil = files_list[grep("soil", files_list)])
    } else {
      stop(
        paste(
          "Path",
          path(folder),
          "does not exist. Check the defition of your
          subtype or the folder structure you are trying to access."
        )
      )
    }

    file_name <- toolSubtypeSelect(subtype, files)


    if (subtype %in% "soil") {
      sk <- file(path(folder, file_name), "rb")
      y <- readBin(sk, integer(), n = 67460, size = 1)
      close(sk)
      y = y[which(magclassdata$grid_67420_59199 == 1)]
      y = y[magclassdata$cellbelongings$LPJ.Index]
      df.y = data.frame("soil" = y)

      soilpar = read.csv(path(folder,"soilpar.csv"))
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
