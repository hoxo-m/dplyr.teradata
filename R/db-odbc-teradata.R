# DBI methods ------------------------------------------------------------------

#' @importFrom odbc dbGetQuery dbListTables
#' @export
db_list_tables.TeradataOdbcConnection <- function(con) {
  table_names <- attr(con, "table_names")
  if (is.null(table_names)) {
    dbname <- con@info$dbname
    if (nzchar(dbname)) {
      query <- sprintf("SELECT TABLENAME FROM DBC.TABLES WHERE DATABASENAME = '%s'", dbname)
      res <- dbGetQuery(con, query)
      table_names <- res$TableName
    } else {
      message("Getting all table names for all schema. This process may spend much time at first.")
      query <- sprintf("SELECT DATABASENAME, TABLENAME FROM DBC.TABLES")
      res <- dbGetQuery(con, query)
      table_names <- sprintf("%s.%s", res$DatabaseName, res$TableName)
    }
    table_names <- gsub("\\s+", "", table_names)
  }
  table_names
}

#' @export
db_has_table.TeradataOdbcConnection <- function(con, table, ...) {
  table <- tolower(table)
  table_names <- tolower(db_list_tables(con))
  table %in% table_names
}

#' @importFrom odbc dbGetQuery
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

#' @export
db_sql_render.TeradataOdbcConnection <- function(con, sql, ...) {
  qry <- sql_build(sql, con = con)
  to_teradata_sql(sql_render(qry, con))
}

#' @export
sql_translate_env.TeradataOdbcConnection <- function(con) {
  sql_variant(
    base_scalar,
    sql_translator(
      .parent = base_agg,
      n = function() sql("count(*)")
    ),
    base_win
  )
}
