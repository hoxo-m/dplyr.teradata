# dplyr.teradata 0.3.1

## Changes

- Add the dots arguments to sample_n() for dplyr 0.8. See https://github.com/tidyverse/dplyr/blob/master/NEWS.md#breaking-changes.

# dplyr.teradata 0.3.0

## New features

- Add `sample_n()` (#17)

## Changes

- Remove dependency to **tidyverse**
- Revert loading messages
- Remove dbUnQuoteIdentifier, as it was removed in favor of
  DBI::dbUnquoteIdentifier in odbc 1.1.6 (#14, @jimhester)
  
## Improvements

- Activate `charset` argument on `dbConnect()`

# dplyr.teradata 0.2.0

## New features

- Add `count_if()` and `n_if()` (#6)
- Add `bool_to_int()`

## Improvements

- Change loading messages
