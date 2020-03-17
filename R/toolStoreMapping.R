#' Tool: StoreMapping
#'
#' Function which creates a mapping file
#'
#'
#' @param map mapping data to be stored.
#' @param name File name of the mapping file. Supported file types are currently csv (, or ; separated)
#' and rda (which needs to have the data stored with the object name "data"!). Use code{\link{toolConvertMapping}}
#' to convert between both formats
#' @param type Mapping type (e.g. "regional", "cell", or "sectoral"). Can be set to NULL if file
#' is not stored in a type specific subfolder
#' @param error.existing Boolean which decides whether an error is returned if
#' the mapping file does not exist or not.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}, \code{\link{toolConvertMapping}}
#' @importFrom tools file_ext
#' @importFrom utils write.table
#' @export
#'
toolStoreMapping <- function(map, name, type=NULL, error.existing=TRUE) {
  mf <- getConfig("mappingfolder")
  if(is.null(mf)) stop('No mappingfolder specified in used cfg! Please load a config with the corresponding information!')
  fname <- paste0(mf,"/",type,"/",name)
  if(error.existing && file.exists(fname)) {
    stop('Mapping "',name,'" exists already!')
  }
  fname <- gsub("/+","/",fname)

  if(is.magpie(map)) {
    pattern <- "^(.*)\\.(.*)\\.(.*)\\.(.*)$"
    map <- data.frame(country.cell=sub(pattern,"\\1.\\3",getCells(map)),
                      region.cluster=sub(pattern,"\\2.\\4",getCells(map)))
  } else stop("Cannot handle this mapping format!")

  filetype <- tolower(file_ext(fname))
  if(filetype=="csv") {
    write.table(map, fname, sep=";", quote=FALSE)
  } else {
    stop("Unsupported filetype \"", filetype,"\"")
  }
}
