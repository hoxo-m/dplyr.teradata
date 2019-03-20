#' @import odbc dbplyr
NULL

#' @importFrom dplyr tbl
#' @export
tbl.Teradata <- function(src, from, ...) {
  tbl <- tbl(src_dbi(src), from = from, ...)
  class(tbl) <- c("tbl_teradata", class(tbl))
  tbl
}

#' @importFrom dplyr dim_desc db_desc
#' @export
print.tbl_teradata <- function(x, ..., n = NULL, width = NULL) {
  cat("Source:   query ", dim_desc(x), "\n", sep = "")
  cat("Database: ", db_desc(x$src$con), "\n", sep = "")
  grps <- op_grps(x$ops)
  if (length(grps) > 0) {
    cat("Groups: ", paste0(op_grps(x$ops), collapse=", "), "\n", sep = "")
  }
  cat("\n")
  cat(sql_render(x), "\n", sep = "")

  invisible(x)
}

#' @importFrom dplyr collect db_explain groups grouped_df
#' @export
collect.tbl_teradata <- function(x, ..., n = Inf, warn_incomplete = TRUE, safety = TRUE) {
  if (is.infinite(n)) {
    n <- -1
  }
  sql <- sql_render(x)

  if(safety) {
    explain_text <- db_explain(x$src$con, sql)
    pattern <- "((\\d|,)+[[:space:]]+hours?[[:space:]]+and[[:space:]]+\\d+[[:space:]]+minutes?)"
    if (grepl(pattern, explain_text)) {
      m <- gregexpr(pattern, explain_text)
      estimated_time_text <- regmatches(explain_text, m)[[1]]
      estimated_time_text <- estimated_time_text[length(estimated_time_text)]
      estimated_time_text <- gsub("[[:space:]]+", " ", estimated_time_text)
      message(sprintf("The estimated execution time is %s.", estimated_time_text))
      if (grepl("*", sql)) {
        message("To specify output column using select() may shorten the time.")
      }
      prompt <- "Do you want to send the query? [N/y]  "
      answer <- substr(readline(prompt), 1L, 1L)
      if (!(answer %in% c("y", "Y"))) {
        message("Cancelled by user\n")
        return(invisible(NULL))
      }
    }
  }

  res <- dbSendQuery(x$src$con, sql)
  out <- dbFetch(res, n = n, ...)
  dbClearResult(res)

  group_columns <- groups(x)
  group_column_types <- vapply(group_columns, function(col) typeof(out[,as.character(col)]), character(1))
  if ("list" %in% group_column_types) {
    message("Cannot group by list type column in R. The result returned as plain data.frame.")
    group_columns <- group_columns[group_column_types != "list"]
  }

  grouped_df(out, group_columns)
}

#' @importFrom dplyr sample_n
#' @export
sample_n.tbl_teradata <- function(tbl, size, replace = NULL, weight = NULL,
                                  .env = NULL, ...) {
  if (!is.null(replace) || !is.null(weight) || !is.null(.env)) {
    warning("sample_n() has not implemented for arguments replace, weight and .env")
  }

  if (inherits(tbl$ops, "op_sample_n")) {
    tbl$ops$args$n <- min(tbl$ops$args$size, size)
  } else {
    tbl$ops <- op_single("sample_n", x = tbl$ops, dots = tbl$ops$dots, args = list(size = size))
  }
  tbl
}

#' @export
sql_build.op_sample_n <- function(op, con, ...) {
  qry <- sql_build(op$x, con)
  sql <- sql_render(qry, con)
  build_sql(sql, "\nSAMPLE ", op$args$size)
}
