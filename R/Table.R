#' @rdname todbc-tables
#' @inheritParams DBI::dbReadTable
#' @export
setMethod("sqlData", "Teradata", function(con, value, row.names = NA, ...) {
  names(value) <- dot_to_underscore(names(value))
  getMethod("sqlData", "OdbcConnection", "odbc")(
    con, value, row.names = row.names, ... = ...)
})

dot_to_underscore <- function(x) {
  gsub(".", "_", x, fixed = TRUE)
}
