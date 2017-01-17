
#' @export
db_list_tables.RODBC <- function(con) {
  # dbListTables(con)
  stop("Unimplemented")
}

#' @export
db_has_table.RODBC <- function(con, table) {
  # dbExistsTable(con, table)
  stop("Unimplemented")
}

#' @export
db_data_type.RODBC <- function(con, fields) {
  # vapply(fields, dbDataType, dbObj = con, FUN.VALUE = character(1))
  stop("Unimplemented")
}

#' @export
db_save_query.RODBC <- function(con, sql, name, temporary = TRUE, ...) {
  # tt_sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
  #                     "TABLE ", ident(name), " AS ", sql, con = con)
  # dbGetQuery(con, tt_sql)
  # name
  stop("Unimplemented")
}

#' @export
db_begin.RODBC <- function(con, ...) {
  # dbBegin(con)
  stop("Unimplemented")
}

#' @export
db_commit.RODBC <- function(con, ...) {
  # dbCommit(con)
  stop("Unimplemented")
}

#' @export
db_rollback.RODBC <- function(con, ...) {
  # dbRollback(con)
  stop("Unimplemented")
}

#' @export
db_create_table.RODBC <- function(con, table, types, temporary = FALSE, ...) {
  # assert_that(is.string(table), is.character(types))
  #
  # field_names <- escape(ident(names(types)), collapse = NULL, con = con)
  # fields <- sql_vector(paste0(field_names, " ", types), parens = TRUE,
  #                      collapse = ", ", con = con)
  # sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
  #                  "TABLE ", ident(table), " ", fields, con = con)
  #
  # dbGetQuery(con, sql)
  stop("Unimplemented")
}

#' @export
db_insert_into.RODBC <- function(con, table, values, ...) {
  stop("Unimplemented")
}

#' @export
db_create_indexes.RODBC <- function(con, table, indexes = NULL, unique = FALSE, ...) {
  # if (is.null(indexes)) return()
  # assert_that(is.list(indexes))
  #
  # for(index in indexes) {
  #   db_create_index(con, table, index, unique = unique, ...)
  # }
  stop("Unimplemented")
}

#' @export
db_create_index.RODBC <- function(con, table, columns, name = NULL, unique = FALSE, ...) {
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
  stop("Unimplemented")
}

#' @export
db_drop_table.RODBC <- function(con, table, force = FALSE, ...) {
  # sql <- build_sql("DROP TABLE ", if (force) sql("IF EXISTS "), ident(table),
  #                  con = con)
  # dbGetQuery(con, sql)
  stop("Unimplemented")
}

#' @export
db_analyze.RODBC <- function(con, table, ...) {
  # sql <- build_sql("ANALYZE ", ident(table), con = con)
  # dbGetQuery(con, sql)
  stop("Unimplemented")
}

#' @export
db_explain.RODBC <- function(con, sql, ...) {
  # exsql <- build_sql("EXPLAIN ", sql, con = con)
  # expl <- dbGetQuery(con, exsql)
  # out <- utils::capture.output(print(expl))
  #
  # paste(out, collapse = "\n")
  stop("Unimplemented")
}

#' @export
db_query_fields.RODBC <- function(con, sql, ...) {
  # sql <- sql_select(con, sql("*"), sql_subquery(con, sql), where = sql("0 = 1"))
  # qry <- dbSendQuery(con, sql)
  # on.exit(dbClearResult(qry))
  #
  # res <- dbFetch(qry, 0)
  # names(res)
  stop("Unimplemented")
}

#' @export
db_query_rows.RODBC <- function(con, sql, ...) {
  # from <- sql_subquery(con, sql, "master")
  # rows <- build_sql("SELECT count(*) FROM ", from, con = con)
  #
  # as.integer(dbGetQuery(con, rows)[[1]])
  stop("Unimplemented")
}

# Utility functions -------------------------------------------------------

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
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
    tryCatch({
      odbcClose(con)
    }, error = function(e) {
      message("Already closed.")
    })
  })
  environment()
}

res_warn_incomplete <- function(res, hint = "n = -1") {
  # if (dbHasCompleted(res)) return()
  #
  # rows <- big_mark(dbGetRowCount(res))
  # warning("Only first ", rows, " results retrieved. Use ", hint, " to retrieve all.",
  #         call. = FALSE)
  stop("Unimplemented")
}

