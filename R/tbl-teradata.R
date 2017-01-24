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

#' @importFrom assertthat assert_that
#' @importFrom RODBC sqlQuery
#' @export
collect.tbl_teradata <- function(x, ..., n = Inf, warn_incomplete = TRUE)  {
  assert_that(length(n) == 1, n > 0L)
  if (n == Inf) {
    n <- 0
  }
  sql <- sql_render(x)

  explain_text <- db_explain(x$src$con, sql)
  pattern <- "((\\d|,)+[[:space:]]+hours[[:space:]]+and[[:space:]]+\\d+[[:space:]]+minutes?)"
  if (grepl(pattern, explain_text)) {
    m <- gregexpr(pattern, explain_text)
    estimated_time_text <- regmatches(explain_text, m)[[1]]
    print(estimated_time_text)
    estimated_time_text <- estimated_time_text[length(estimated_time_text)]
    estimated_time_text <- gsub("[[:space:]]+", " ", estimated_time_text)
    message(sprintf("Attention! The estimated execution time is %s.", estimated_time_text))
    if (grepl(",", estimated_time_text)) {
      message("To specify output column using select() may shorten the time.")
    }
    prompt <- "Do you want to send the query? [N/y]  "
    answer <- substr(readline(prompt), 1L, 1L)
    if (!(answer %in% c("y", "Y"))) {
      message("Cancelled by user\n")
      return(invisible(NULL))
    }
  }

  out <- sqlQuery(x$src$con, sql, max = n, stringsAsFactors = FALSE, ...)

  group_columns <- groups(x)
  group_column_types <- vapply(group_columns, function(col) typeof(out[,as.character(col)]), character(1))
  if ("list" %in% group_column_types) {
    message("Cannot group by list type column in R. The result returns as plain data.frame.")
    group_columns <- group_columns[group_column_types != "list"]
  }

  grouped_df(out, group_columns)
}

# SQL render --------------------------------------------------------------

#' @export
sql_render.tbl_teradata <- function(query, con = NULL, ...) {
  sql <- sql_render(sql_build(query$ops, query$src$con, ...), con = query$src$con, ...)
  if (sql %in% db_list_tables(query$src$con)) {
    sql <- sprintf("SELECT * FROM %s", sql)
  }
  to_teradata_sql(sql)
}

# Utility Functions -------------------------------------------------------

to_teradata_sql <- function(sql) {
  gsub("\\bLIMIT\\b", "SAMPLE", sql)
}
