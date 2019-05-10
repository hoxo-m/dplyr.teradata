.onAttach <- function(libname, pkgname) {
  assign("cut", sql_cut, envir = dbplyr::base_odbc_scalar)
  assign("like", sql_like, envir = dbplyr::base_odbc_scalar)
  assign("to_timestamp", sql_to_timestamp, envir = dbplyr::base_odbc_scalar)
  assign("%%", sql_mod, envir = dbplyr::base_odbc_scalar)
  assign("count_if", sql_count_if, envir = dbplyr::base_odbc_scalar)
  assign("n_if", sql_count_if, envir = dbplyr::base_odbc_scalar)
  assign("bool_to_int", sql_bool_to_int, envir = dbplyr::base_odbc_scalar)
}
