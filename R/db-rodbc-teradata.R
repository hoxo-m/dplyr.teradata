#' @importFrom RODBC sqlQuery
#' @export
db_list_tables.TeradataODBCConnection <- function(con) {
  table_names <- attr(con, "table_names")
  if (is.null(table_names)) {
    dbname <- attr(con, "dbname")
    if (dbname != "") {
      query <- sprintf("SELECT TABLENAME FROM DBC.TABLES WHERE DATABASENAME = '%s'", dbname)
      qry <- sqlQuery(con, query)
      table_names <- gsub("\\s+", "", as.character(qry$TableName))
    } else {
      message("Getting all table names for all schema.")
      table_names <- db_list_tables.RODBC(con)
    }
  }
  table_names
}

#' @importFrom RODBC sqlQuery
#' @export
db_explain.TeradataODBCConnection <- function(con, sql, format = "text", ...) {
  # format <- match.arg(format, c("text", "json", "yaml", "xml"))
  # exsql <- build_sql("EXPLAIN ", if (!is.null(format))
  #   build_sql("(FORMAT ", sql(format), ") "), sql)
  if (is.ident(sql) || db_has_table(con, sql)) {
    exsql <- build_sql("EXPLAIN SELECT * FROM ", sql)
  } else {
    exsql <- build_sql("EXPLAIN ", sql)
  }
  expl <- sqlQuery(con, exsql)
  paste(expl[[1]], collapse = "\n")
}

#' @export
sql_translate_env.TeradataODBCConnection <- function(con) {
  sql_variant(
    base_scalar_teradata,
    sql_translator(.parent = base_agg,
                   n = function() sql("count(*)"),
                   cor = sql_prefix("corr"),
                   cov = sql_prefix("covar_samp"),
                   sd = sql_prefix("stddev_samp"),
                   var = sql_prefix("var_samp"),
                   all = sql_prefix("bool_and"),
                   any = sql_prefix("bool_or"),
                   paste = function(x, collapse) build_sql("string_agg(", x, ", ", collapse, ")")
    ),
    base_win
  )
}
