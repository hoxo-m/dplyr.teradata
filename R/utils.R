#' List tables with specified pattern
#'
#' @param con Teradata connection.
#' @param pattern character string containing a regular expression.
#'
#' @importFrom dplyr db_list_tables
#' @export
db_list_tables_with_pattern <- function(con, pattern) {
  table_names <- db_list_tables(con)
  Filter(function(x) grepl(pattern, x), table_names)
}

#' Convert blob to character.
#'
#' @param blob blob vector.
#'
#' @examples
#' (x <- blob::as.blob("Good morning"))
#' #> [1] blob[12 B]
#' x[[1]]
#' #> [1] 47 6f 6f 64 20 6d 6f 72 6e 69 6e 67
#' blob_to_string(x)
#' #> [1] "476f6f64206d6f726e696e67"
#'
#' @export
blob_to_string <- function(blob) {
  vapply(blob, function(x) paste(as.character(x), collapse = ""), character(1L))
}
