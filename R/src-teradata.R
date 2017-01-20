#' Connect to Teradata.
#'
#' @importFrom methods new setClass
#' @importFrom rJava .jpackage
#' @importFrom RJDBC dbConnect JDBC
#' @importFrom rstudioapi isAvailable askForPassword
#' @export
src_teradata <- function(dbname = NULL, host = NULL, port = NULL, user = NULL,
                         password = NULL, charset = c("UTF8", "ASCII", "UTF16"),
                         tmode = c("ANSI", "TERA")) {
  if (is.null(dbname)) dbname <- Sys.getenv("TDDATABASE")
  if (is.null(host)) host <- Sys.getenv("TDHOST")
  if (is.null(user)) user <- Sys.getenv("TDUSER")
  if (is.null(password)) password <- Sys.getenv("TDPASSWORD")
  if (password == "" && isAvailable()) {
    password <- askForPassword("Input Password for Teradata")
  }
  charset <- match.arg(charset)
  tmode <- match.arg(tmode)

  drv <- tryCatch({
    JDBC("com.teradata.jdbc.TeraDriver")
  }, error = function(e) {
    .jpackage("RJDBC")
    add_jdbc_class_path()
    JDBC("com.teradata.jdbc.TeraDriver")
  })
  st <- sprintf("jdbc:teradata://%s", host)
  if (!is.null(dbname)) {
    st <- sprintf("%s/database=%s", st, dbname)
  }
  st <- sprintf("%s,charset=%s,tmode=%s", st, charset, tmode)
  con <- dbConnect(drv, st, user = user, password = password)

  con <- new("TeradataJDBCConnection", con)
  attr(con, "dbname") <- dbname
  attr(con, "table_names") <- db_list_tables(con)

  info <- list(host=host, user = user, dbname = dbname, server_version = get_td_version(con))

  src_sql("teradata", con, info = info, disco = db_disconnector(con, "teradata"))
}

#' @export
#' @rdname src_teradata
tbl.src_teradata <- function(src, from, ...) {
  tbl_sql("teradata", src = src, from = sql(from), ...)
}

#' @export
src_desc.src_teradata <- function(x) {
  info <- x$info

  host <- if (info$host == "") "localhost" else info$host

  sprintf("Teradata %s [%s@%s/%s]", info$server_version, info$user, host, info$dbname)
}

# JDBC methods ------------------------------------------------------------

#' @importFrom RJDBC dbGetQuery
#' @export
db_list_tables.TeradataJDBCConnection <- function(con) {
  table_names <- attr(con, "table_names")
  if (is.null(table_names)) {
    dbname <- attr(con, "dbname")
    query <- sprintf("SELECT TABLENAME FROM DBC.TABLES WHERE DATABASENAME = '%s'", dbname)
    qry <- dbGetQuery(con, query)
    table_names <- gsub("\\s+", "", as.character(qry$TableName))
  }
  table_names
}

#' @export
db_has_table.TeradataJDBCConnection <- function(con, table) {
  table_names <- db_list_tables(con)
  table %in% table_names
}

#' @importFrom RJDBC dbGetQuery
#' @export
db_explain.TeradataJDBCConnection <- function(con, sql, format = "text", ...) {
  # format <- match.arg(format, c("text", "json", "yaml", "xml"))
  # exsql <- build_sql("EXPLAIN ", if (!is.null(format))
  #   build_sql("(FORMAT ", sql(format), ") "), sql)
  format <- match.arg(format)
  if (is.ident(sql) || db_has_table(con, sql)) {
    exsql <- build_sql("EXPLAIN SELECT * FROM ", sql)
  } else {
    exsql <- build_sql("EXPLAIN ", sql)
  }
  expl <- dbGetQuery(con, exsql)
  paste(expl[[1]], collapse = "\n")
}

#' @export
sql_translate_env.TeradataJDBCConnection <- function(con) {
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

# SQL generic -------------------------------------------------------------

#' @export
sql_subquery.TeradataJDBCConnection <- function(con, from, name = NULL, ...) {
  if (!grepl("\\s", from)) {
    return(from)
  }
  if (is.null(name)) {
    name <- random_table_name()
  }
  build_sql("(", from, ") AS ", ident(name), con = con)
}

#' @export
sql_escape_ident.TeradataJDBCConnection <- function(con, x) {
  x
}

# SQL render --------------------------------------------------------------

#' @export
sql_render.tbl_teradata <- function(query, con = NULL, ...) {
  sql <- sql_render(sql_build(query$ops, query$src$con, ...), con = query$src$con, ...)
  # sql <- remove_constant_groups(sql, query$ops)
  to_teradata_sql(sql)
}

# Utility functions -------------------------------------------------------

to_teradata_sql <- function(sql) {
  gsub("\\bLIMIT\\b", "SAMPLE", sql)
}

#' @importFrom rJava .jaddClassPath
add_jdbc_class_path <- function(tdstudio_path = "/Applications/TeradataStudio") {
  dir <- list.files(paste0(tdstudio_path, "/plugins"), pattern = "terajdbc", full.names = TRUE)
  message(sprintf("Add Class Path: %s", file.path(dir, "terajdbc4.jar")))
  .jaddClassPath(file.path(dir, "terajdbc4.jar"))
  message(sprintf("Add Class Path: %s", file.path(dir, "tdgssconfig.jar")))
  .jaddClassPath(file.path(dir, "tdgssconfig.jar"))
}

#' @importFrom RJDBC dbGetQuery
get_td_version <- function(con) {
  sql <- "SELECT * FROM DBC.DBCINFO"
  dbcinfo <- dbGetQuery(con, sql)
  td_version <- dbcinfo$InfoData[dbcinfo$InfoKey == "VERSION"]
  td_version
}
