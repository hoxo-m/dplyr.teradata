#' @rdname todbc-tables
#' @inheritParams DBI::sqlCreateTable
#' @param field.types Additional field types used to override derived types.
#' @importFrom methods getMethod
#' @export
setMethod("sqlCreateTable", "TeradataOdbcConnection",
          getMethod("sqlCreateTable", "Teradata", "odbc"))

#' @rdname todbc-tables
#' @inheritParams DBI::dbReadTable
#' @export
setMethod("sqlData", "TeradataOdbcConnection", function(con, value, row.names = NA, ...) {
  names(value) <- dot_to_underscore(names(value))
  getMethod("sqlData", "OdbcConnection", "odbc")(
    con, value, row.names = row.names, ... = ...)
})
