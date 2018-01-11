# TODO: Delete this function when the pull request is merged.
# https://github.com/tidyverse/dbplyr/pull/42
#' @export
db_desc.TeradataOdbcConnection <- function(x) {
  info <- DBI::dbGetInfo(x)

  host <- if (info$servername == "") "localhost" else info$servername
  port <- if (info$port == "") "" else paste0(":", info$port)

  paste0(
    info$dbms.name, " ", info$db.version,
    "[", info$username, "@", host, port,
    "/", info$dbname , "]")
}

# DBI methods ------------------------------------------------------------------

#' @importFrom odbc dbGetQuery dbListTables
#' @importFrom dplyr db_list_tables
#' @export
db_list_tables.TeradataOdbcConnection <- function(con) {
  dbname <- con@info$dbname
  if (nzchar(dbname)) {
    table_names <- dbListTables(con)
  } else {
    message("Getting all table names for all schema.")
    query <- sprintf("SELECT DATABASENAME, TABLENAME FROM DBC.TABLES")
    res <- dbGetQuery(con, query)
    table_names <- sprintf("%s.%s", res$DatabaseName, res$TableName)
  }
  trimws(table_names)
}

#' @export
#' @importFrom dplyr db_has_table
db_has_table.TeradataOdbcConnection <- function(con, table, ...) {
  table <- tolower(table)
  table_names <- tolower(db_list_tables(con))
  table %in% table_names
}

#' @importFrom odbc dbGetQuery
#' @importFrom dplyr db_explain
#' @export
db_explain.TeradataOdbcConnection <- function(con, sql, format = "text", ...) {
  if (is.ident(sql)) {
    exsql <- build_sql("EXPLAIN SELECT * FROM ", sql)
  } else {
    sql <- sql_render(sql)
    exsql <- build_sql("EXPLAIN ", sql)
  }
  expl <- dbGetQuery(con, exsql)
  paste(expl[[1]], collapse = "\n")
}

#' @importFrom dbplyr db_sql_render
#' @export
db_sql_render.TeradataOdbcConnection <- function(con, sql, ...) {
  qry <- sql_build(sql, con = con)
  to_teradata_sql(sql_render(qry, con))
}

#' @importFrom dplyr db_analyze
#' @export
db_analyze.TeradataOdbcConnection <- function(con, table, ...) {
  # no process
}

#' @importFrom dplyr sql_translate_env
#' @importFrom dbplyr sql_variant base_scalar sql_translator base_agg base_win
#' @export
sql_translate_env.TeradataOdbcConnection <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_scalar,
      case_when = case_when_teradata,
      extract = extract_teradata,
      year = make_extract("YEAR"),
      month = make_extract("MONTH"),
      day = make_extract("DAY"),
      hour = make_extract("HOUR"),
      minute = make_extract("MINUTE"),
      second = make_extract("SECOND"),
      cut = cut_teradata
    ),
    sql_translator(
      .parent = base_agg,
      n = function() sql("count(*)")
    ),
    base_win
  )
}
