# toolRiverUpDownBalance

This function calculates the cell water balance under consideration of
different reserved human uses (non-agricultural, neighbor water
requirements, committed-agricultural uses)

## Usage

``` r
toolRiverUpDownBalance(inLIST, inoutLIST)
```

## Arguments

- inLIST:

  List of objects that are inputs to the function: previously reserved
  withdrawals and consumption in current cell; currently requested
  withdrawal in current cell

- inoutLIST:

  List of objects that are inputs to the function and are updated by the
  function: discharge (including up- and downstream cells) currently
  requested consumption (including upstream cells)

## Value

list of arrays objects in cellular resolution

## Author

Felicitas Beier, Jens Heinke
