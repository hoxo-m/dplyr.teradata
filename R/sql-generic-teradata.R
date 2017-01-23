
# TeradataODBCConnection --------------------------------------------------

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

#' @export
sql_join.TeradataODBCConnection <- function(con, x, y, type = "inner", by = NULL, ...) {
  join <- switch(type,
                 left = sql("LEFT"),
                 inner = sql("INNER"),
                 right = sql("RIGHT"),
                 full = sql("FULL"),
                 stop("Unknown join type:", type, call. = FALSE)
  )

  on <- sql_vector(paste0(sql_escape_ident(con, by$x), " = ", sql_escape_ident(con, by$y)),
                   collapse = " AND ", parens = TRUE)
  cond <- build_sql("ON ", on, con = con)

  # Wrap with SELECT since callers assume a valid query is returned
  build_sql(
    'SELECT * FROM ',x, "\n\n",
    join, " JOIN\n\n" ,
    y, "\n\n",
    cond,
    con = con
  )
}

# Utility Functions -------------------------------------------------------

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}
