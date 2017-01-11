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
  attr(con, "table_names") <- get_table_names(con)
  class(con) <- c("TeradataODBCConnection", class(con))
  src_sql("teradata", con = con, dbname = dbname, user = user, disco = db_disconnector(con, "teradata"))
}

#' @importFrom RODBC sqlQuery
get_table_names <- function(con) {
  table_names <- attr(con, "table_names")
  if (is.null(table_names)) {
    dbname <- attr(con, "dbname")
    query <- sprintf("SELECT TABLENAME FROM DBC.TABLES WHERE DATABASENAME = '%s'", dbname)
    qry <- sqlQuery(con, query)
    table_names <- gsub("\\s+", "", as.character(qry$TableName))
  }
  table_names
}

#' @importFrom RODBC odbcGetInfo
#' @export
src_desc.src_teradata <- function(x) {
  info <- odbcGetInfo(x$con)
  server_name <- info["Server_Name"]
  dbms_ver <- strsplit(info["DBMS_Ver"], split = "\\s+")$DBMS_Ver[2]
  sprintf("Teradata %s [%s@%s/%s]", dbms_ver, x$user, server_name, x$dbname)
}

#' @export
db_list_tables.TeradataODBCConnection <- get_table_names

#' @export
tbl.src_teradata <- function(src, from, ...) {
  table_names <- get_table_names(src$con)
  pattern <- sprintf("\\b(%s)\\b", paste(table_names, collapse = "|"))
  replace <- sprintf("%s.\\1", src$dbname)
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
  table_names <- get_table_names(con)
  dbname <- attr(con, "dbname")
  table_names <- paste(dbname, table_names, sep=".")
  any(sql %in% table_names)
}

#' @export
print.tbl_teradata <- function(x, ..., n = NULL, width = NULL) {
  cat("Source:   query ", dim_desc(x), "\n", sep = "")
  cat("Database: ", src_desc(x$src), "\n", sep = "")
  grps <- op_grps(x$ops)
  if (length(grps) > 0) {
    # cat("Groups: ", commas(op_grps(x$ops)), "\n", sep = "")
    cat("Groups: ", paste0(op_grps(x$ops), collapse=", "), "\n", sep = "")
  }
  cat("\n")
  cat(sql_render(x), "\n", sep = "")

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
  if (is.ident(sql) || is_table_name(con, sql)) {
    exsql <- build_sql("EXPLAIN SELECT * FROM ", sql)
  } else {
    exsql <- build_sql("EXPLAIN ", sql)
  }
  expl <- sqlQuery(con, exsql)
  paste(expl[[1]], collapse = "\n")
}

#' @export
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
  out <- sqlQuery(x$src$con, sql, rows_at_time = n)

  # if (warn_incomplete) {
  #   res_warn_incomplete(out, "n = Inf")
  # }

  vars <- Filter(Negate(is_numeric_group), groups(x))
  grouped_df(out, vars = vars)
}

is_numeric_group <- function(group) {
  tryCatch({as.numeric(deparse(group));TRUE}, error = function(e) FALSE)
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
      paste = function(x, collapse) build_sql("string_agg(", x, ", ", collapse, ")"),
      case_when = case_when_teradata
    ), base_win)
}

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

#' @export
sql_render.tbl_teradata <- function(query, con = NULL, ...) {
  sql <- sql_render(sql_build(query$ops, query$src$con, ...), con = query$src$con, ...)
  sql <- remove_constant_groups(sql, query$ops)
  to_teradata_sql(sql)
}

#' @export
db_has_table.TeradataODBCConnection <- function(con, table) {
  dbname <- attr(con, "dbname")
  table_names <- get_table_names(con)
  table %in% c(table_names, paste(dbname, table_names, sep="."))
}

#' @export
db_data_type.TeradataODBCConnection <- function(con, fields) {
  message("Unimplemented")
}

#' @export
db_save_query.TeradataODBCConnection <- function(con, sql, name, temporary = TRUE, ...) {
  # tt_sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
  #                     "TABLE ", ident(name), " AS ", sql, con = con)
  # dbGetQuery(con, tt_sql)
  # name
  message("Unimplemented")
}

#' @export
db_begin.TeradataODBCConnection <- function(con, ...) {
  # dbBegin(con)
  message("Unimplemented")
}

#' @export
db_commit.TeradataODBCConnection <- function(con, ...) {
  # dbCommit(con)
  message("Unimplemented")
}

#' @export
db_rollback.TeradataODBCConnection <- function(con, ...) {
  # dbRollback(con)
  message("Unimplemented")
}

#' @export
db_create_table.TeradataODBCConnection <- function(con, table, types, temporary = FALSE, ...) {
  # assert_that(is.string(table), is.character(types))
  #
  # field_names <- escape(ident(names(types)), collapse = NULL, con = con)
  # fields <- sql_vector(paste0(field_names, " ", types), parens = TRUE,
  #                      collapse = ", ", con = con)
  # sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
  #                  "TABLE ", ident(table), " ", fields, con = con)
  #
  # dbGetQuery(con, sql)
  message("Unimplemented")
}

#' @export
db_create_indexes.TeradataODBCConnection <- function(con, table, indexes = NULL, unique = FALSE, ...) {
  # if (is.null(indexes)) return()
  # assert_that(is.list(indexes))
  #
  # for(index in indexes) {
  #   db_create_index(con, table, index, unique = unique, ...)
  # }
}

#' @export
db_create_index.TeradataODBCConnection <- function(con, table, columns, name = NULL, unique = FALSE, ...) {
  # assert_that(is.string(table), is.character(columns))
  #
  # name <- name %||% paste0(c(table, columns), collapse = "_")
  # fields <- escape(ident(columns), parens = TRUE, con = con)
  # sql <- build_sql(
  #   "CREATE ", if (unique) sql("UNIQUE "), "INDEX ", ident(name),
  #   " ON ", ident(table), " ", fields,
  #   con = con)
  #
  # dbGetQuery(con, sql)
  message("Unimplemented")
}

#' @export
db_drop_table.TeradataODBCConnection <- function(con, table, force = FALSE, ...) {
  # sql <- build_sql("DROP TABLE ", if (force) sql("IF EXISTS "), ident(table),
  #                  con = con)
  # dbGetQuery(con, sql)
  message("Unimplemented")
}

#' @export
db_analyze.TeradataODBCConnection <- function(con, table, ...) {
  # sql <- build_sql("ANALYZE ", ident(table), con = con)
  # dbGetQuery(con, sql)
  message("Unimplemented")
}

#' @export
db_query_rows.TeradataODBCConnection <- function(con, sql, ...) {
  # from <- sql_subquery(con, sql, "master")
  # rows <- build_sql("SELECT count(*) FROM ", from, con = con)
  #
  # as.integer(dbGetQuery(con, rows)[[1]])
  message("Unimplemented")
}

# Creates an environment that disconnects the database when it's
# garbage collected
#' @importFrom RODBC odbcClose
db_disconnector <- function(con, name, quiet = FALSE) {
  reg.finalizer(environment(), function(...) {
    if (!quiet) {
      message("Auto-disconnecting ", name, " connection ",
              "(", paste(con[1], collapse = ", "), ")")
    }
    odbcClose(con)
  })
  environment()
}

res_warn_incomplete <- function(res, hint = "n = -1") {
  # if (dbHasCompleted(res)) return()
  #
  # rows <- big_mark(dbGetRowCount(res))
  # warning("Only first ", rows, " results retrieved. Use ", hint, " to retrieve all.",
  #         call. = FALSE)
  message("Unimplemented")
}

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

# group_by_.tbl_teradata <- function(.data, ..., .dots, add = TRUE) {
#   dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
#   result <- add_op_single("group_by", .data, dots = dots, args = list(add = add))
#   print(result)
#   result
# }

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
