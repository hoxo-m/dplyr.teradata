# base_scalar_teradata <- as.environment(as.list(dbplyr::base_scalar, all.names=TRUE))
#
#
# # if else -----------------------------------------------------------------
# sql_if_teradata <- function(cond, if_true, if_false = NULL) {
#   build_sql(
#     "CASE WHEN ", cond,
#     " THEN ", if_true,
#     if (!is.null(if_false)) build_sql(" ELSE ", if_false),
#     " END"
#   )
# }
#
# assign("if", sql_if_teradata, envir = base_scalar_teradata)
# assign("ifelse", sql_if_teradata, envir = base_scalar_teradata)
# assign("if_else", sql_if_teradata, envir = base_scalar_teradata)

# case when ---------------------------------------------------------------
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
    if (length(f) != 3) {
      stop("Case ", i, " (", f, ") is not a two-sided formula", call. = FALSE)
    }
    query[[i]] <- translate_sql_(list(f[[2]]))
    value[[i]] <- translate_sql_(list(f[[3]]))
  }
  sql <- build_sql("CASE")
  for (i in seq_len(n)) {
    if (query[[i]] == "TRUE") break
    sql <- build_sql(sql, " WHEN ", query[[i]], " THEN ", value[[i]])
  }
  if (query[[i]] == "TRUE") {
    sql <- build_sql(sql, " ELSE ", value[[i]])
  }
  sql <- build_sql(sql, " END")
  sql
}

# assign("case_when", case_when_teradata, envir = base_scalar_teradata)
#
# extract -----------------------------------------------------------------
extract_teradata <- function(date_column, target) {
  build_sql("EXTRACT(", sql(target), " FROM ", date_column, ")")
}

year_teradata<- function(date_column) {
  extract_teradata(date_column, "YEAR")
}

month_teradata <- function(date_column) {
  extract_teradata(date_column, "MONTH")
}

day_teradata <- function(date_column) {
  extract_teradata(date_column, "DAY")
}

# assign("year", year, envir = base_scalar_teradata)
# assign("month", month, envir = base_scalar_teradata)
# assign("day", day, envir = base_scalar_teradata)
#
# # not equal ---------------------------------------------------------------
# not_equal <- function(x, y) {
#   build_sql(x, " <> ", y)
# }
# assign("!=", not_equal, envir = base_scalar_teradata)
#
#
# # cast --------------------------------------------------------------------
# as_teradata <- function(x, type) {
#   build_sql("CAST(", x, " AS ", sql(type), ")")
# }
# assign("as", as_teradata, envir = base_scalar_teradata)
#
#
# # is not null -------------------------------------------------------------
# is_not_null <- function(x) {
#   build_sql("(", x, ") IS NOT NULL")
# }
# assign("is.notnull", is_not_null, envir = base_scalar_teradata)
#
# op_not <- function(..., na.rm) {
#   args <- list(...)
#   if (length(args) == 1) {
#     sql <- args[[1]]
#     if (endsWith(sql, "IS NULL")) {
#       return(gsub("IS NULL", "IS NOT NULL", sql))
#     }
#   }
#   base_scalar$`!`(..., na.rm)
# }
# assign("!", op_not, envir = base_scalar_teradata)
#
#
# # like --------------------------------------------------------------------
#
# like <- function(x, pattern) {
#   build_sql(x, " LIKE ", pattern)
# }
# assign("like", like, envir = base_scalar_teradata)
