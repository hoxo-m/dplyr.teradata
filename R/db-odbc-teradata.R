# DBI methods ------------------------------------------------------------------

#' @importFrom odbc dbGetQuery dbListTables
#' @importFrom dplyr db_list_tables
#' @export
db_list_tables.Teradata <- function(con) {
  # message("Getting all table names for all schema.")
  query <- sprintf("SELECT DATABASENAME, TABLENAME FROM DBC.TABLES")
  res <- dbGetQuery(con, query)
  dbname <- tolower(con@info$dbname)
  if (nzchar(dbname)) {
    table_names <- res[tolower(trimws(res$DatabaseName)) == dbname, ]$TableName
  } else {
    table_names <- sprintf("%s.%s", res$DatabaseName, res$TableName)
  }
  trimws(table_names)
}

#' @export
#' @importFrom dplyr db_has_table
db_has_table.Teradata <- function(con, table, ...) {
  table <- tolower(table)
  table_names <- tolower(db_list_tables(con))
  table %in% table_names
}
