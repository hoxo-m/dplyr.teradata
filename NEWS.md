# dplyr.teradata 0.4.1

- Now supports for **dplyr** 1.0.0 and **dbplyr** 2.0.0 (#26)

## Changes

- Delegate Teradata Odbc Connection Methods to **odbc** (#27)
- Deprecate `todbc()` (#28). Use `odbc::odbc()` instead. If you use the versions of Teradata ODBC Driver 15.20 or earlier, you may need to set like `dbConnect(odbc(), ..., dbms.name = 'Teradata')`. 

## Improvements

- Export dummy functions (`like`, `to_timestamp`, `mod`, `count_if`, `n_if` and `bool_to_int`) to make code completion work (#29). These functions can only be used inside the functions to build queries (e.g. `mutate`).

# dplyr.teradata 0.3.2

## Changes

- Delegate SQL translations of `case_when()` and **lubridate**-family (e.g. `year()`, `month()`, `day()`) to **dbplyr**. See https://github.com/tidyverse/dbplyr/blob/master/NEWS.md#sql-translations.

# dplyr.teradata 0.3.1

## Changes

- Add the dots arguments to `sample_n()` for **dplyr** 0.8. See https://github.com/tidyverse/dplyr/blob/master/NEWS.md#breaking-changes.

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
