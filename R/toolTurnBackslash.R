#' @title toolTurnBackslash
#' @description turns around backslashes in a string
#'
#' @return string
#' @author Felicitas Beier

toolTurnBackslash <- function(x) {
  # read in string
  x   <- readClipboard()

  # turn around backslashes
  out <- gsub("\\\\", "/", x)

  return(out)
}
