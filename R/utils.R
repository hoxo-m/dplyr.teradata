#' @importFrom dplyr db_list_tables
#' @export
db_list_tables_with_pattern <- function(con, pattern) {
  table_names <- db_list_tables(con)
  Filter(function(x) grepl(pattern, x), table_names)
}

#' @export
raw_to_string <- function(raw) {
  vapply(raw, function(x) paste(as.character(x), collapse = ""), character(1L))
}
