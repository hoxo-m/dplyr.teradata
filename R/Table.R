#' Convenience functions for reading/writing DBMS tables
#'
#' @param value A data.frame to write to the database.
#' @inheritParams DBI::sqlCreateTable
#' @examples
#' \dontrun{
#' library(dplyr.teradata)
#' con <- dbConnect(todbc())
#' dbListTables(con)
#' dbWriteTable(con, "mtcars", mtcars, temporary = TRUE)
#' dbReadTable(con, "mtcars")
#'
#' dbListTables(con)
#' dbExistsTable(con, "mtcars")
#'
#' # A zero row data frame just creates a table definition.
#' dbWriteTable(con, "mtcars2", mtcars[0, ], temporary = TRUE)
#' dbReadTable(con, "mtcars2")
#'
#' dbDisconnect(con)
#' }
#' @name todbc-tables
NULL

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
