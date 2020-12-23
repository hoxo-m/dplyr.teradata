#' @import odbc dbplyr
NULL

#' @importFrom dplyr sample_n
#' @export
sample_n.tbl_Teradata <- function(tbl, size, replace = FALSE, weight = NULL,
                                  .env = NULL, randomized_allocation = TRUE,
                                  ...) {
  size <- as.integer(size)
  stopifnot(size >= 1L)
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

#' @importFrom dplyr slice_sample
#' @export
slice_sample.tbl_Teradata <- function(.data, ..., randomized_allocation = TRUE,
                                      n, prop, weight_by = NULL, replace = FALSE) {
  if (missing(n) && missing(prop)) {
    n <- 1L
  }
  if (!is.null(weight_by)) {
    warning("slice_sample() has not implemented for the argument 'weight_by'.")
  }
  size <- NULL
  if (missing(prop)) {
    n <- as.integer(n)
    stopifnot(n >= 1L)
    size <- n
  } else {
    prop <- as.double(prop)
    stopifnot(0.0 < prop, prop < 1.0)
    size <- prop
  }
  sample_impl(.data, size, replace, randomized_allocation)
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
