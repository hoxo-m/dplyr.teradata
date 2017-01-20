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
