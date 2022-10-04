#' @title       toolNeighborWaterProvision
#' @description This tool function allows to draw water from surrounding cells
#'              if local water availability is not sufficient to fulfill demand
#'
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand
#' @param rs             River structure list including neighboring cells
#' @param c              Current local cell that requires additional water from
#'                       surrounding cells
#' @param missingWW      Volume of water withdrawal
#'                       that needs to come from surrounding cells
#' @param missingWC      Volume of water consumption
#'                       that needs to come from surrounding cells
#' @param inoutLIST      List of objects that are updated
#'                       (discharge, watReserved, missingWW, missingWC)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'

toolNeighborWaterProvision <- function(transDist, c,
                                       rs,
                                       missingWW, missingWC, inoutLIST) {

  # read in variables that need to be updated
  discharge   <- inoutLIST$discharge
  watReserved <- inoutLIST$watReserved
  resNeighborWC <- inoutLIST$resNeighborWC
  resNeighborWW <- inoutLIST$resNeighborWW
  neighborProvisionWC <- inoutLIST$neighborProvisionWC
  neighborProvisionWW <- inoutLIST$neighborProvisionWW

  # initialize variables to be filled
  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(discharge),
                          years = getYears(discharge),
                          names = getNames(discharge),
                          fill = 0,
                          sets = getSets(discharge))
    # bring object x to dimension of object0
    out     <- object0 + x
    return(out)
  }

  if (transDist != 0 && !is.null(rs$neighborcell[[c]])) {

    # Loop over neighbor cells (by distance) until water requirements fulfilled
    for (n in rs$neighborcell[[c]]) {

      ### How to ensure that calcorder is met in neighbor cells?
      ### (Otherwise discharge seen might be different at different calculation steps)
      ### How do we do that in Discharge Allocation??

      # initialize local variables
      fracNeighbor <- as.array(.transformObject(0))

      # Helper vectors for subsetting of objects
      # vector of downstreamcells of n
      vDOWN <- unlist(rs$downstreamcells[[n]])
      # vector of n in length of downstreamcells of n
      vCELL <- rep(n, length(rs$downstreamcells[[n]]))
      # vector of 1s in length of downstreamcells of c
      vONES <- rep(1, length(rs$downstreamcells[[n]]))

      # available water for additional irrigation withdrawals
      avlWatWW <- pmax(discharge[n, , , drop = FALSE] -
                          watReserved[n, , , drop = FALSE],
                      0)

      # how much of missing water can be fulfilled by available water in this neighboring cell
      fracNeighbor[n, , ][missingWW != 0] <- pmin(avlWatWW / missingWW,
                                                  1)[missingWW != 0]

      if (length(vDOWN) > 0) {

        # available water for additional irrigation consumption (considering downstream availability)
        avlWatWC <- pmax(apply(discharge[vDOWN, , , drop = FALSE] -
                                  watReserved[vDOWN, , , drop = FALSE],
                               MARGIN = 3, min),
                        0)

        # how much consumption can be fulfilled by available water
        fracNeighbor[n, , ][missingWC != 0] <- pmin(avlWatWC / missingWC,
                                                    fracNeighbor[n, , , drop = FALSE])[missingWC != 0]
      }

      # adjust discharge in neighbor cell and downstream cells of neighbor
      # (subtract irrigation water consumption)
      discharge[c(vDOWN, n), , ] <- (discharge[c(vDOWN, n), , , drop = FALSE] -
                                        missingWC[c(vONES, 1), , , drop = FALSE] *
                                         fracNeighbor[c(vCELL, n), , , drop = FALSE])
      # update minimum water reserved in this cell (required in cell and to provide water to neighbor):
      watReserved[n, , ] <- (watReserved[n, , , drop = FALSE] +
                                fracNeighbor[n, , , drop = FALSE] * missingWW)

      # update water provided by neighboring cells
      neighborProvisionWW[c, , ] <- neighborProvisionWW[c, , , drop = FALSE] +
                                      fracNeighbor[n, , , drop = FALSE] * missingWW
      neighborProvisionWC[c, , ] <- neighborProvisionWC[c, , , drop = FALSE] +
                                      fracNeighbor[n, , , drop = FALSE] * missingWC
      # update reserved water in respective neighboring cell
      resNeighborWW[n, , ] <- resNeighborWW[n, , , drop = FALSE] +
                                fracNeighbor[n, , , drop = FALSE] * missingWW
      resNeighborWC[n, , ] <- resNeighborWC[n, , , drop = FALSE] +
                                fracNeighbor[n, , , drop = FALSE] * missingWC

      # Update locally missing water in c
      missingWW <- missingWW - fracNeighbor[n, , , drop = FALSE] * missingWW
      missingWC <- missingWC - fracNeighbor[n, , , drop = FALSE] * missingWC

      if (any(missingWW < 0)) {
        stop(paste0("More water than necessary provided to missingWW
                    in toolNeighborWaterProvision in cell ", c,
                    " by neighborcell ", n))
      }
      if (any(missingWC < 0)) {
        stop(paste0("More water than necessary provided to missingWC
                    in toolNeighborWaterProvision in cell ", c,
                    " by neighborcell ", n))
      }

      if (all(missingWW <= 0) && all(missingWC <= 0)) {
        break
      }
    }
  }

  # Return outputs
  out <- list(discharge = discharge,
              watReserved = watReserved,
              neighborProvisionWW = neighborProvisionWW,
              neighborProvisionWC = neighborProvisionWC,
              resNeighborWC = resNeighborWC,
              resNeighborWW = resNeighborWW)

  return(out)
}
