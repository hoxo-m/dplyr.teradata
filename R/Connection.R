# Refer to https://github.com/rstats-db/odbc/blob/master/R/Connection.R

#' Teradata Odbc Connection Methods
#'
#' Implementations of pure virtual functions defined in the `DBI` package
#' for `Teradata` objects.
#' @name TeradataOdbcConnection
NULL

class_cache <- get("class_cache", envir = asNamespace("odbc"))
class <- getClassDef("Teradata", where = class_cache, inherits = FALSE)
if (is.null(class) || methods::isVirtualClass(class)) {
  setClass("Teradata",
           contains = "OdbcConnection", where = class_cache)
}

#' @rdname TeradataOdbcConnection
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "Teradata",
  function(object) {
    info <- dbGetInfo(object)

    cat(sep = "", "<Teradata>",
        if (nzchar(info[["servername"]])) {
          paste0(" ",
                 if (nzchar(info[["username"]])) paste0(info[["username"]], "@"),
                 info[["servername"]], "\n")
        },
        if (!dbIsValid(object)) {
          "  DISCONNECTED\n"
        } else {
          paste0(collapse = "",
                 if (nzchar(info[["dbname"]])) {
                   paste0("  Database: ", info[["dbname"]], "\n")
                 },
                 if (nzchar(info[["dbms.name"]]) && nzchar(info[["db.version"]])) {
                   paste0("  ", info[["dbms.name"]], " ", "Version: ", info[["db.version"]], "\n")
                 },
                 NULL)
        })
  })

#' @rdname TeradataOdbcConnection
#' @inheritParams DBI::dbQuoteIdentifier
#' @importFrom dbplyr is.ident
#' @export
setMethod(
  "dbQuoteIdentifier", c("Teradata", "character"),
  function(conn, x, ...) {
    if (nzchar(conn@quote)) {
      x <- gsub(conn@quote, paste0(conn@quote, conn@quote), x, fixed = TRUE)
    }
    schema_tablename_pattern <- "^[[:alnum:]_]+\\.[[:alnum:]_]+$"
    quotes <- ifelse(is.ident(x) && grepl(schema_tablename_pattern, x),
                     "", conn@quote)
    DBI::SQL(paste0(quotes, encodeString(x), quotes))
  })

#' Un-Quote identifiers
#'
#' Call this method to generate a string that is unquoted. This is the inverse
#' of `DBI::dbQuoteIdentifier`.
#'
#' @param x A character vector to un-quote.
#' @inheritParams DBI::dbQuoteIdentifier
#' @export
setMethod(
  "dbUnQuoteIdentifier", c("Teradata", "character"),
  function(conn, x) {
    pattern <- sprintf("^%s", conn@quote)
    x <- sub(pattern, "", x)
    pattern <- sprintf("%s$", conn@quote)
    x <- sub(pattern, "", x)
    x
  })

#' @rdname TeradataOdbcConnection
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "Teradata",
  function(dbObj, ...) {
    info <- dbObj@info
    structure(info, class = c(info$dbms.name, "driver_info", "list"))
  })
