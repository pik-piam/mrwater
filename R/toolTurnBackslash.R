#' @title toolTurnBackslash
#' @description turns around backslashes in a string
#'
#' @importFrom utils readClipboard
#'
#' @return string
#' @author Felicitas Beier

toolTurnBackslash <- function() {
  # read in string
  x   <- readClipboard()

  # turn around backslashes
  out <- gsub("\\\\", "/", x)

  return(out)
}
