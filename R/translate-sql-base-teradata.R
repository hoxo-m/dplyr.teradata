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

# extract -----------------------------------------------------------------
make_extract <- function(target) {
  function(date_column) {
    build_sql("EXTRACT(", sql(target), " FROM ", date_column, ")")
  }
}

extract_teradata <- function(target, date_column) {
  make_extract(target)(date_column)
}

# cut ---------------------------------------------------------------------
cut_teradata <- function(x, breaks = NULL, labels = NULL,
                         include.lowest = FALSE, right = TRUE, dig.lab = 3,
                         ...) {

  # Prepare Arguments -------------------------------------------------------
  x <- deparse(substitute(x))
  if(is.null(breaks)) stop("cut() needs breaks argument")
  if(is.null(labels)) {
    labels <- levels(cut(0, breaks = breaks, include.lowest = include.lowest,
                         right = right, dig.lab = dig.lab))
  }
  if(right) {
    lower_op <- ">"
    higher_op <- "<="
  } else {
    lower_op <- ">="
    higher_op <- "<"
  }
  if(length(labels) == 1 && is.character(labels)) {
    # label is the center mark
    labels <- generate_range_labels(breaks, include.lowest = include.lowest,
                                    right = right, center = labels)
  }

  # Build SQL ---------------------------------------------------------------
  n <- length(labels)
  sql <- build_sql("CASE\n")
  for (i in seq_len(n)) {
    lower_cond <- sql(sprintf("%s %s %s", x, lower_op, breaks[i]))
    higher_cond <- sql(sprintf("%s %s %s", x, higher_op, breaks[i+1]))
    if(i == 1 && breaks[1] == -Inf) {
      sql <- build_sql(sql, " WHEN ", higher_cond, " THEN ", labels[1], "\n")
    } else if(i == 1 && include.lowest && right) {
      lower_cond <- sql(sprintf("%s >= %s", x, breaks[i]))
      sql <- build_sql(sql, " WHEN ", lower_cond, " AND ", higher_cond, " THEN ", labels[i], "\n")
    } else if(i == n && breaks[n+1] == Inf) {
      sql <- build_sql(sql, " WHEN ", lower_cond, " THEN ", labels[n], "\n")
    } else if(i == n && include.lowest && !right) {
      higher_cond <- sql(sprintf("%s <= %s", x, breaks[i+1]))
      sql <- build_sql(sql, " WHEN ", lower_cond, " AND ", higher_cond, " THEN ", labels[i], "\n")
    } else {
      sql <- build_sql(sql, " WHEN ", lower_cond, " AND ", higher_cond, " THEN ", labels[i], "\n")
    }
  }
  sql <- build_sql(sql, " ELSE NULL\nEND")
  sql
}


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


is_integer_or_infinaite <- function(values) {
  all(ifelse(is.finite(values), values %% 1 == 0, TRUE))
}

generate_range_labels <- function(breaks, include.lowest = FALSE, right = TRUE,
                                  center = "-", left_char = "", right_char = "") {
  if(is_integer_or_infinaite(breaks)) {
    len <- length(breaks) - 1
    labels <- character(len)
    for(i in seq_len(len)) {
      p <- breaks[i]
      n <- breaks[i+1]
      if(right) {
        if(i != 1 || !include.lowest) {
          p <- p + 1
        }
        if(p == -Inf) {
          label <- sprintf("%s%s%s%s", left_char, center, n, right_char)
        } else if(n == Inf) {
          label <- sprintf("%s%s%s%s", left_char, p, center, right_char)
        } else if(p == n) {
          label <- sprintf("%s%s%s", left_char, p, right_char)
        } else {
          label <- sprintf("%s%s%s%s%s", left_char, p, center, n, right_char)
        }
      } else {
        if(i != len || !include.lowest) {
          n <- n - 1
        }
        if(p == -Inf) {
          label <- sprintf("%s%s%s%s", left_char, center, n, right_char)
        } else if(n == Inf) {
          label <- sprintf("%s%s%s%s", left_char, p, center, right_char)
        } else if(p == n) {
          label <- sprintf("%s%s%s", left_char, p, right_char)
        } else {
          label <- sprintf("%s%s%s%s%s", left_char, p, center, n, right_char)
        }
      }
      labels[i] <- label
    }
    labels
  } else {
    stop("breaks are not integer or infinite")
  }
}
