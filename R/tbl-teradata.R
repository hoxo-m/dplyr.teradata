#' @import odbc dbplyr
NULL

#' @importFrom dplyr sample_n
#' @export
sample_n.tbl_Teradata <- function(tbl, size, replace = FALSE, weight = NULL,
                                  .env = NULL, ...) {
  size <- as.integer(size)
  if (!is.null(weight) || !is.null(.env)) {
    warning("sample_n() has not implemented for arguments weight and .env")
  }

  if (inherits(tbl$ops, "op_sample")) {
    tbl$ops$args$size <- min(tbl$ops$args$size, size)
  } else {
    tbl$ops <- op_single("sample", x = tbl$ops, dots = tbl$ops$dots,
                         args = list(size = size, replace = replace))
  }
  tbl
}

#' @export
sql_build.op_sample <- function(op, con, ...) {
  qry <- sql_build(op$x, con)
  sql <- sql_render(qry, con)
  if (op$args$replace) {
    build_sql(sql, "\nSAMPLE WITH REPLACEMENT ", op$args$size, con = con)
  } else {
    build_sql(sql, "\nSAMPLE ", op$args$size, con = con)
  }
}
