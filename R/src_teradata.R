#' @importFrom RODBC odbcDriverConnect
#' @importFrom rstudioapi isAvailable askForPassword
#' @export
src_teradata <- function(dbname = NULL, host = NULL, port = NULL, user = NULL,
                         password = NULL, case = FALSE, ...) {
  if (is.null(dbname)) dbname <- Sys.getenv("TDDATABASE")
  if (is.null(host)) host <- Sys.getenv("TDHOST")
  if (is.null(user)) user <- Sys.getenv("TDUSER")
  if (is.null(password)) password <- Sys.getenv("TDPASSWORD")
  if (password == "" && isAvailable()) {
    password <- askForPassword("Input Password for Teradata")
  }
  connection <- sprintf("Driver=Teradata;DBCName=%s;UID=%s;PWD=%s", host, user, password)
  con <- odbcDriverConnect(connection, case = case, readOnlyOptimize = TRUE)
  attr(con, "user") <- user
  attr(con, "schema") <- dbname
  class(con) <- c("TeradataODBCConnection", class(con))
  src_sql("teradata", con = con, schema = dbname, user = user)
}

#' @importFrom RODBC odbcGetInfo
#' @export
src_desc.src_teradata <- function(x) {
  info <- odbcGetInfo(x$con)
  server_name <- info["Server_Name"]
  dbms_ver <- strsplit(info["DBMS_Ver"], split = "\\s+")$DBMS_Ver[2]
  sprintf("Teradata %s %s[%s@%s]", dbms_ver, x$schema, x$user, server_name)
}

#' @importFrom RODBC sqlQuery
#' @export
db_list_tables.TeradataODBCConnection <- function(con) {
  schema <- attr(con, "schema")
  query <- sprintf("SELECT TABLENAME FROM DBC.TABLES WHERE DATABASENAME = '%s'", schema)
  qry <- sqlQuery(con, query)
  gsub("\\s+", "", as.character(qry$TableName))
}

#' @export
tbl.src_teradata <- function(src, from, ...) {
  table_names <- db_list_tables.TeradataODBCConnection(src$con)
  pattern <- sprintf("(%s)", paste(table_names, collapse = "|"))
  replace <- sprintf("%s.\\1", src$schema)
  from <- gsub(pattern, replace, from)
  tbl_sql("teradata", src = src, from = sql(from), ...)
}

#' @importFrom RODBC sqlQuery sqlClear
#' @export
db_query_fields.TeradataODBCConnection <- function (con, sql, ...)
{
  fields <- build_sql("SELECT * FROM ", sql_subquery(con, sql), " WHERE 0=1", con = con)

  qry <- sqlQuery(con, fields)

  colnames(qry)
}

#' @export
sql_subquery.TeradataODBCConnection <- function(con, from, name = NULL, ...) {
  if (is_table_name(con, from)) {
    return(from)
  }
  if (is.null(name)) {
    name <- random_table_name()
  }
  build_sql("(", from, ") AS ", ident(name), con = con)
}

is_table_name <- function(con, sql) {
  table_names <- db_list_tables.TeradataODBCConnection(con)
  schema <- attr(con, "schema")
  table_names <- c(table_names, paste(schema, table_names, sep="."))
  any(sql %in% table_names)
}

print.tbl_teradata <- function(x, ..., n = NULL, width = NULL) {
  cat("Source:   query ", dim_desc(x), "\n", sep = "")
  cat("Database: ", src_desc(x$src), "\n", sep = "")
  grps <- op_grps(x$ops)
  if (length(grps) > 0) {
    cat("Groups: ", commas(op_grps(x$ops)), "\n", sep = "")
  }
  cat("\n")

  # n <- ifelse(is.null(n), 6, n)
  # sql <- build_sql("SELECT TOP ", n, " * FROM ", sql_subquery(con, sql), con = con)
  # result <- sqlQuery(con, sql)

  invisible(x)
}

#' @importFrom RODBC sqlQuery
#' @export
db_explain.TeradataODBCConnection <- function(con, sql, format = "text", ...) {
  # format <- match.arg(format, c("text", "json", "yaml", "xml"))
  # exsql <- build_sql("EXPLAIN ", if (!is.null(format))
  #   build_sql("(FORMAT ", sql(format), ") "), sql)
  if (is_table_name(con, sql)) {
    exsql <- build_sql("EXPLAIN SELECT * FROM ", sql)
  } else {
    exsql <- build_sql("EXPLAIN ", sql)
  }
  print(exsql)
  expl <- sqlQuery(con, exsql)
  paste(expl[[1]], collapse = "\n")
}

sql_escape_ident.TeradataODBCConnection <- function(con, x) {
  x
}

#' @importFrom RODBC sqlQuery sqlClear sqlFetch
#' @importFrom assertthat assert_that
#' @export
collect.tbl_teradata <- function(x, ..., n = 1e+05, warn_incomplete = TRUE)  {
  assert_that(length(n) == 1, n > 0L)
  if (n == Inf) {
    n <- -1
  }
  sql <- sql_render(x)
  sql <- to_teradata_sql(sql)
  out <- sqlQuery(x$src$con, sql, rows_at_time = n)

  # if (warn_incomplete) {
  #   res_warn_incomplete(out, "n = Inf")
  # }
  grouped_df(out, groups(x))
}

to_teradata_sql <- function(sql) {
  sql <- gsub("\\bLIMIT\\b", " SAMPLE ", sql)
  sql
}

#' @export
sql_translate_env.TeradataODBCConnection <- function(con) {
  sql_variant(
    base_scalar,
    sql_translator(
      .parent = base_agg,
      n = function() sql("count(*)"), cor = sql_prefix("corr"),
      cov = sql_prefix("covar_samp"), sd = sql_prefix("stddev_samp"),
      var = sql_prefix("var_samp"), all = sql_prefix("bool_and"),
      any = sql_prefix("bool_or"),
      paste = function(x, collapse) build_sql("string_agg(", x, ", ", collapse, ")")
    ), base_win)
}

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}
