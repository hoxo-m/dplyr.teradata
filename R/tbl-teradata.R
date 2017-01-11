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
