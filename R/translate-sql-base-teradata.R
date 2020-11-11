#' @import dbplyr
NULL

# cut ---------------------------------------------------------------------
sql_cut <- function(x, breaks = NULL, labels = NULL,
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

# like --------------------------------------------------------------------
sql_like <- function(x, pattern) {
  build_sql(x, " LIKE ", pattern)
}

# to_timestamp ------------------------------------------------------------
sql_to_timestamp <- function(x) {
  build_sql("CAST(DATE '1970-01-01' + (", x ,
            " / 86400) AS TIMESTAMP(0)) + (", x,
            " MOD 86400) * (INTERVAL '00:00:01' HOUR TO SECOND)")
}

# mod ---------------------------------------------------------------------
sql_mod <- function(x, divisor) {
  build_sql(x, " MOD ", divisor)
}

# count_if ----------------------------------------------------------------
sql_count_if <- function(cond) {
  build_sql("SUM(CASE WHEN (", cond, ") THEN 1 WHEN NOT(", cond, ") THEN 0 END)")
}

# bool_to_int -------------------------------------------------------------
sql_bool_to_int <- function(cond) {
  build_sql("CASE WHEN (", cond, ") THEN 1 WHEN NOT(", cond, ") THEN 0 END")
}

# dummy functions ---------------------------------------------------------
# Export dummy functions to make code completion work.
# Note: cut has the original function base::cut

#' Translatable function for 'LIKE' operator
#' @param x column name
#' @param pattern LIKE pattern
#' @export
like <- function(x, pattern) stop("unimplemented function")

#' Translatable function to convert UNIX time to time-stamp
#' @param x column name stored UNIX time (e.g. 1609459200)
#' @return time-stamp (e.g. "2021-01-01 00:00:00")
#' @export
to_timestamp <- function(x) stop("unimplemented function")

#' Translatable Function for '%%' operator
#' @param x dividend
#' @param divisor divisor
#' @return modulo
#' @export
mod <- function(x, divisor) stop("unimplemented function")

#' Translatable function to count rows satisfied a condition
#' @param cond condition
#' @export
count_if <- function(cond) stop("unimplemented function")

#' Translatable function to count rows satisfied a condition
#' @param cond condition
#' @export
n_if <- function(cond) stop("unimplemented function")

#' Translatable function to convert boolean to integer
#' @param cond condition
#' @return vector. 1 while cond is TRUE, 0 if FALSE
#' @export
bool_to_int <- function(cond) stop("unimplemented function")
