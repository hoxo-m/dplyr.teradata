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


#' #' @importFrom dplyr sql_translate_env
#' #' @importFrom dbplyr sql_variant sql_translator base_scalar base_agg base_win
#' sql_translate_env.Teradata <- function(con) {
#'   sql_variant_org <- sql_translate_env_org(con)
#'   sql_variant(
#'     sql_translator(
#'       .parent = sql_variant_org$scalar,
#'       case_when = sql_case_when,
#'       extract = sql_extract,
#'       year = make_extract("YEAR"),
#'       month = make_extract("MONTH"),
#'       day = make_extract("DAY"),
#'       hour = make_extract("HOUR"),
#'       minute = make_extract("MINUTE"),
#'       second = make_extract("SECOND"),
#'       cut = sql_cut
#'     ),
#'     sql_variant_org$aggregate,
#'     sql_variant_org$win
#'   )
#' }

