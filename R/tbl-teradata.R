#' @import odbc dbplyr
NULL

#' @importFrom dplyr sample_n
#' @export
sample_n.tbl_Teradata <- function(tbl, size, replace = NULL, weight = NULL,
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
  build_sql(sql, "\nSAMPLE ", op$args$size, con = con)
}
