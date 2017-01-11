#' Connect to Teradata.
#'
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
  attr(con, "dbname") <- dbname
  class(con) <- c("TeradataODBCConnection", class(con))
  attr(con, "table_names") <- db_list_tables(con)
  info <- odbcGetInfo(con)
  src_sql("teradata", con = con, dbname = dbname, user = user, info = info, disco = db_disconnector(con, "teradata"))
}

#' @export
#' @rdname src_teradata
tbl.src_teradata <- function(src, from, ...) {
  table_names <- db_list_tables(src$con)
  pattern <- sprintf("\\b(%s)\\b", paste(table_names, collapse = "|"))
  replace <- sprintf("%s.\\1", src$dbname)
  from <- gsub(pattern, replace, from)
  tbl_sql("teradata", src = src, from = sql(from), ...)
}

#' @importFrom RODBC odbcGetInfo
#' @export
src_desc.src_teradata <- function(x) {
  info <- x$info
  server_name <- info["Server_Name"]
  dbms_ver <- strsplit(info["DBMS_Ver"], split = "\\s+")$DBMS_Ver[2]
  sprintf("Teradata %s [%s@%s/%s]", dbms_ver, x$user, server_name, x$dbname)
}

# ODBC methods ------------------------------------------------------------

#' @importFrom RODBC sqlQuery
#' @export
db_list_tables.TeradataODBCConnection <- function(con) {
  table_names <- attr(con, "table_names")
  if (is.null(table_names)) {
    dbname <- attr(con, "dbname")
    query <- sprintf("SELECT TABLENAME FROM DBC.TABLES WHERE DATABASENAME = '%s'", dbname)
    qry <- sqlQuery(con, query)
    table_names <- gsub("\\s+", "", as.character(qry$TableName))
  }
  table_names
}

#' @export
db_has_table.TeradataODBCConnection <- function(con, table) {
  dbname <- attr(con, "dbname")
  table_names <- db_list_tables(con)
  table %in% c(table_names, paste(dbname, table_names, sep="."))
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
db_insert_into.TeradataODBCConnection <- function(con, table, values, ...) {

  # if (nrow(values) == 0)
  #   return(NULL)
  #
  # cols <- lapply(values, escape, collapse = NULL, parens = FALSE, con = con)
  # col_mat <- matrix(unlist(cols, use.names = FALSE), nrow = nrow(values))
  #
  # rows <- apply(col_mat, 1, paste0, collapse = ", ")
  # values <- paste0("(", rows, ")", collapse = "\n, ")
  #
  # sql <- build_sql("INSERT INTO ", ident(table), " VALUES ", sql(values))
  # dbGetQuery(con, sql)
  stop("Unimplemented")
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
sql_translate_env.TeradataODBCConnection <- function(con) {
  sql_variant(
    base_scalar,
    sql_translator(.parent = base_agg,
                   n = function() sql("count(*)"),
                   cor = sql_prefix("corr"),
                   cov = sql_prefix("covar_samp"),
                   sd = sql_prefix("stddev_samp"),
                   var = sql_prefix("var_samp"),
                   all = sql_prefix("bool_and"),
                   any = sql_prefix("bool_or"),
                   paste = function(x, collapse) build_sql("string_agg(", x, ", ", collapse, ")"),
                   case_when = case_when_teradata
    ),
    base_win
  )
}

# SQL generic -------------------------------------------------------------

#' @export
sql_subquery.TeradataODBCConnection <- function(con, from, name = NULL, ...) {
  if (db_has_table(con, from)) {
    return(from)
  }
  if (is.null(name)) {
    name <- random_table_name()
  }
  build_sql("(", from, ") AS ", ident(name), con = con)
}

#' @export
sql_escape_ident.TeradataODBCConnection <- function(con, x) {
  x
}

# SQL render --------------------------------------------------------------

#' @export
sql_render.tbl_teradata <- function(query, con = NULL, ...) {
  sql <- sql_render(sql_build(query$ops, query$src$con, ...), con = query$src$con, ...)
  sql <- remove_constant_groups(sql, query$ops)
  to_teradata_sql(sql)
}

# Utility functions -------------------------------------------------------

case_when_teradata <- function(...) {
  formulas <- list(...)
  n <- length(formulas)
  if (n == 0) {
    stop("No cases provided", call. = FALSE)
  }
  query <- vector("list", n)
  value <- vector("list", n)
  for (i in seq_len(n)) {
    f <- formulas[[i]]
    f <- gsub("=", "==", f)
    f <- parse(text = f)[[1]]
    # if (!inherits(f, "formula") || length(f) != 3) {
    #   non_formula_arg <- substitute(list(...))[[i + 1]]
    #   stop("Case ", i, " (", deparse_trunc(non_formula_arg),
    #        ") is not a two-sided formula", call. = FALSE)
    # }
    env <- environment(f)
    query[[i]] <- gsub("==", "=", deparse(f[[2]]))
    value[[i]] <- eval(f[[3]], envir = env)
  }
  # print(query)
  # print(value)
  build_sql("CASE WHEN ", sql(query[[1]]), " THEN ", value[[1]], " ELSE ", value[[2]], " END")
}

remove_constant_groups <- function(sql, ops) {
  if (!("name" %in% ls(ops))) {
    sql
  } else if (ops$name == "summarise") {
    groups <- ops$x$dots
    groups <- lapply(groups, function(x) x$expr)
    numeric_groups <- Filter(is.numeric, groups)
    if (length(numeric_groups) != 0) {
      patterns <- paste0(numeric_groups, ", ")
      for (pattern in patterns) {
        sql <- sub(pattern, "", sql)
      }
    }
    sql
  } else {
    remove_constant_groups(sql, ops$x)
  }
}

to_teradata_sql <- function(sql) {
  gsub("\\bLIMIT\\b", "SAMPLE", sql)
}

is_numeric_group <- function(group) {
  tryCatch({as.numeric(deparse(group));TRUE}, error = function(e) FALSE)
}

