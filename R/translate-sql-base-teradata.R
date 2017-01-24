base_scalar_teradata <- as.environment(as.list(dplyr::base_scalar, all.names=TRUE))

sql_if_teradata <- function(cond, if_true, if_false = NULL) {
  build_sql(
    "CASE WHEN ", cond,
    " THEN ", if_true,
    if (!is.null(if_false)) build_sql(" ELSE ", if_false),
    " END"
  )
}

assign("if", sql_if_teradata, envir = base_scalar_teradata)
assign("ifelse", sql_if_teradata, envir = base_scalar_teradata)
assign("if_else", sql_if_teradata, envir = base_scalar_teradata)

#' @importFrom stats as.formula
case_when_teradata <- function(...) {
  formulas <- list(...)
  n <- length(formulas)
  if (n == 0) {
    stop("No cases provided", call. = FALSE)
  }
  query <- vector("list", n)
  value <- vector("list", n)
  for (i in seq_len(n)) {
    f <- formulas[[i]]
    f <- gsub("=", "==", f)
    f <- as.formula(f)
    if (!inherits(f, "formula") || length(f) != 3) {
      non_formula_arg <- substitute(list(...))[[i + 1]]
      stop("Case ", i, " (", deparse(non_formula_arg),
           ") is not a two-sided formula", call. = FALSE)
    }
    env <- environment(f)
    query[[i]] <- gsub("==", "=", deparse(f[[2]]))
    value[[i]] <- eval(f[[3]], envir = env)
  }
  sql <- build_sql("CASE")
  for (i in seq_len(n)) {
    if (query[[i]] == "TRUE") break
    sql <- build_sql(sql, " WHEN ", sql(query[[i]]), " THEN ", value[[i]])
  }
  if (query[[i]] == "TRUE") {
    sql <- build_sql(sql, " ELSE ", value[[i]])
  }
  sql <- build_sql(sql, " END")
  sql
}
assign("case_when", case_when_teradata, envir = base_scalar_teradata)

as_teradata <- function(x, type) {
  build_sql("CAST(", x, " AS ", sql(type), ")")
}
assign("as", as_teradata, envir = base_scalar_teradata)

extract <- function(date_column, target) {
  build_sql("EXTRACT(", sql(target), " FROM ", date_column, ")")
}

year <- function(date_column) {
  extract(date_column, "YEAR")
}

month <- function(date_column) {
  extract(date_column, "MONTH")
}

day <- function(date_column) {
  extract(date_column, "DAY")
}

assign("year", year, envir = base_scalar_teradata)
assign("month", month, envir = base_scalar_teradata)
assign("day", day, envir = base_scalar_teradata)

not_equal <- function(x, y) {
  build_sql(x, " <> ", y)
}

assign("!=", not_equal, envir = base_scalar_teradata)

# as.character_teradata <- function(x) {
#   build_sql("CAST(", x, " AS VARCHAR(255))")
# }
# assign("as.character", as.character_teradata, envir = base_scalar_teradata)
