.onAttach <- function(libname, pkgname) {
  assign("case_when", sql_case_when, envir = dbplyr::base_odbc_scalar)
  assign("extract", sql_extract, envir = dbplyr::base_odbc_scalar)
  assign("year", make_extract("YEAR"), envir = dbplyr::base_odbc_scalar)
  assign("month", make_extract("MONTH"), envir = dbplyr::base_odbc_scalar)
  assign("day", make_extract("DAY"), envir = dbplyr::base_odbc_scalar)
  assign("hour", make_extract("HOUR"), envir = dbplyr::base_odbc_scalar)
  assign("minute", make_extract("MINUTE"), envir = dbplyr::base_odbc_scalar)
  assign("second", make_extract("SECOND"), envir = dbplyr::base_odbc_scalar)
  assign("as_date", dbplyr::base_odbc_scalar$as.Date, envir = dbplyr::base_odbc_scalar)
  assign("cut", sql_cut, envir = dbplyr::base_odbc_scalar)
  assign("like", sql_like, envir = dbplyr::base_odbc_scalar)
  assign("to_timestamp", sql_to_timestamp, envir = dbplyr::base_odbc_scalar)
  assign("count_if", sql_count_if, envir = dbplyr::base_odbc_scalar)
  assign("n_if", sql_count_if, envir = dbplyr::base_odbc_scalar)
  assign("bool_to_int", sql_bool_to_int, envir = dbplyr::base_odbc_scalar)

  # Attaching packages ------------------------------------------------------
  # See https://github.com/tidyverse/tidyverse/blob/master/R/zzz.R
  is_attached <- get("is_attached", envir = asNamespace("tidyverse"))
  msg <- get("msg", envir = asNamespace("tidyverse"))

  dep_packages <- c("dplyr", "dbplyr")

  to_load <- dep_packages[!is_attached(dep_packages)]
  if (length(to_load) == 0)
    return()

  crayon::num_colors(TRUE)
  dplyr.teradata_attach(to_load)

  x <- dplyr.teradata_conflicts(to_load)
  msg(dplyr.teradata_conflict_message(x), startup = TRUE)
}

# See https://github.com/tidyverse/tidyverse/blob/master/R/attach.R
dplyr.teradata_attach <- function(to_load) {
  msg <- get("msg", envir = asNamespace("tidyverse"))
  package_version <- get("package_version", envir = asNamespace("tidyverse"))

  if (length(to_load) == 0)
    return(invisible())

  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("dplyr.teradata ", package_version("dplyr.teradata"))
    ),
    startup = TRUE
  )

  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  invisible()
}

# See https://github.com/tidyverse/tidyverse/blob/master/R/conflicts.R
dplyr.teradata_conflicts <- function(to_load) {
  ls_env <- get("ls_env", envir = asNamespace("tidyverse"))
  invert <- get("invert", envir = asNamespace("tidyverse"))
  confirm_conflict <- get("confirm_conflict", envir = asNamespace("tidyverse"))

  envs <- purrr::set_names(search())
  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", to_load)
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "dplyr.teraedata_conflicts")
}

# See https://github.com/tidyverse/tidyverse/blob/master/R/conflicts.R
dplyr.teradata_conflict_message <- function(x) {
  if (length(x) == 0) return("")

  header <- cli::rule(
    left = crayon::bold("Conflicts"),
    right = ""
  )

  pkgs <- purrr::map(x, ~ gsub("^package:", "", .))
  others <- purrr::map(pkgs, `[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(crayon::blue(.x), "::", .y, "()", collapse = ", ")
  )

  winner <- purrr::map_chr(pkgs, 1)
  funs <- format(paste0(crayon::blue(winner), "::", crayon::green(paste0(names(x), "()"))))
  bullets <- paste0(
    crayon::red(cli::symbol$cross), " ", funs,
    " masks ", other_calls,
    collapse = "\n"
  )

  paste0(header, "\n", bullets)
}
