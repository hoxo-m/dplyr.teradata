#' @rdname todbc-tables
#' @inheritParams DBI::sqlCreateTable
#' @param field.types Additional field types used to override derived types.
#' @importFrom methods getMethod
#' @export
setMethod("sqlCreateTable", "TeradataOdbcConnection",
          getMethod("sqlCreateTable", "Teradata", "odbc"))
