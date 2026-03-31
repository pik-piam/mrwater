# toolRiverDischargeAllocation

This tool function allocates discharge for grid cells respecting
upstream-downstream relationships and various water constraints

## Usage

``` r
toolRiverDischargeAllocation(
  rs,
  c,
  downCells,
  iteration,
  transDist,
  inLIST,
  inoutLIST
)
```

## Arguments

- rs:

  River structure with information on upstreamcells, downstreamcells and
  neighboring cells and distances

- c:

  Current cell for which water shall be allocated

- downCells:

  Downstream cells of c

- iteration:

  Currently active iteration of river discharge allocation. Arguments:
  "main" for case of main river cells "neighbor" for case of neighboring
  cells of main river cells

- transDist:

  Water transport distance allowed to fulfill locally unfulfilled water
  demand by surrounding cell water availability

- inLIST:

  List of objects that are inputs to the function irrigGain,
  gainthreshold,

- inoutLIST:

  List of objects that are inputs to the function and are updated by the
  function

## Value

magpie object in cellular resolution

## Author

Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
