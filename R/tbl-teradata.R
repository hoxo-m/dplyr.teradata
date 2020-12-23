#' @import odbc dbplyr
NULL

#' @importFrom dplyr sample_n
#' @export
sample_n.tbl_Teradata <- function(tbl, size, replace = FALSE, weight = NULL,
                                  .env = NULL, randomized_allocation = TRUE,
                                  ...) {
  size <- as.integer(size)
  stopifnot(size > 0L)
  if (!is.null(weight) || !is.null(.env)) {
    warning("sample_n() has not implemented for arguments weight and .env")
  }
  sample_impl(tbl, size, replace, randomized_allocation)
}

#' @importFrom dplyr sample_frac
#' @export
sample_frac.tbl_Teradata <- function(tbl, size = 1, replace = FALSE,
                                     weight = NULL, .env = NULL,
                                     randomized_allocation = TRUE, ...) {
  size <- as.double(size)
  stopifnot(0.0 < size, size < 1.0)
  if (!is.null(weight) || !is.null(.env)) {
    warning("sample_frac() has not implemented for arguments weight and .env")
  }
  sample_impl(tbl, size, replace, randomized_allocation)
}

sample_impl <- function(tbl, size, replace, randomized_allocation) {
  if (inherits(tbl$ops, "op_sample")) {
    tbl$ops$args$size <- min(tbl$ops$args$size, size)
  } else {
    tbl$ops <- op_single("sample", x = tbl$ops, dots = tbl$ops$dots,
                         args = list(size = size, replace = replace,
                                     randomized_allocation = randomized_allocation))
  }
  tbl
}

#' @export
sql_build.op_sample <- function(op, con, ...) {
  qry <- sql_build(op$x, con)
  sql <- sql_render(qry, con)

  str <- "\nSAMPLE "
  if (op$args$replace) {
    str <- paste0(str, "WITH REPLACEMENT ")
  }
  if (op$args$randomized_allocation) {
    str <- paste0(str, "RANDOMIZED ALLOCATION ")
  }
  build_sql(sql, sql(str), op$args$size, con = con)
}
