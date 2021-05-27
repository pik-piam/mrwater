#' @title toolLPJcell2MAgPIEcell
#' @description transforms magpie object with 67420 cells to object with 59199 cells
#'
#' This tool function selects the required cells from object with
#' 67420 cells to be able to plot using plotmap or plotmap2 that
#' are applied to magpie object of size 59199 cells
#'
#' @param x magpie object to be transformed
#'
#' @return magpie object with correct dimension for use in plotmap
#' @author Felicitas Beier
#'
#' @import magclass
#'
#' @export


toolLPJcell2MAgPIEcell <- function(x) {
  # select correct cells and transform to magpie object
  out <- x[magclassdata$cellbelongings$LPJ_input.Index]
  out <- toolCell2isoCell(out)

  return(out)
}
