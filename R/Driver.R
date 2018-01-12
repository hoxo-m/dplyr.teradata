# Refer to https://github.com/rstats-db/odbc/blob/master/R/Driver.R

#' Teradata Odbc Driver Methods
#'
#' Implementations of pure virtual functions defined in the `DBI` package
#' for `TeradataOdbcDriver` objects.
#' @name TeradataOdbcDriver
NULL

#' Teradata Odbc driver
#'
#' Driver for an Teradata ODBC database.
#'
#' @export
#' @import methods odbc DBI
#' @examples
#' \dontrun{
#' todbc()
#' }
todbc <- function() {
  new("TeradataOdbcDriver")
}

#' @rdname TeradataOdbcDriver
#' @export
setClass("TeradataOdbcDriver", contains = "OdbcDriver")

#' @rdname TeradataOdbcDriver
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "TeradataOdbcDriver",
  function(object) {
    cat("<TeradataOdbcDriver>\n")
    # TODO: Print more details
  })

#' Connect to a Teradata ODBC compatible database
#'
#' @inheritParams DBI::dbConnect
#' @param dsn The Data Source Name.
#' @param timezone The Server time zone. Useful if the database has an internal
#' timezone that is _not_ 'UTC'. If the database is in your local timezone set
#' to `Sys.timezone()`. See [OlsonNames()] for a complete list of available
#' timezones on your system.
#' @param encoding Alias of `charset`.
#' @param driver The ODBC driver name.
#' @param server Alias of `DBCName`.
#' @param DBCName The server hostname.
#' @param database The database on the server.
#' @param uid The user identifier.
#' @param pwd The password to use.
#' @param charset Character Set. `"ASCII"`(default), `"UTF8"` or `"UTF16"`.
#' @param tmode TMODE. `"ANSI"`(default) or `"TERA"`.
#' @param dbms.name The database management system name. This should normally
#' be queried automatically by the ODBC driver. This name is used as the class
#' name for the OdbcConnect object returned from  `dbConnect()`. However if the
#' driver does not return a valid value it can be set manually with this
#' parameter.
#' @param ... Additional ODBC keywords, these will be joined with the other
#' arguments to form the final connection string.
#' @param .connection_string A complete connection string, useful if you are
#' copy pasting it from another source. If this argument is used any additional
#' arguments will be appended to this string.
#' @param bigint The R type that `SQL_BIGINT` types should be mapped to,
#' default is [bit64::integer64], which allows the full range of 64 bit
#' integers.
#' @details
#' The connection string keywords are driver dependent. The parameters
#' documented here are common, but some drivers may not accept them. Please see
#' the specific driver documentation for allowed parameters,
#' \url{https://www.connectionstrings.com} is also a useful resource of example
#' connection strings for a variety of databases.
#' @aliases dbConnect
#' @export
setMethod(
  "dbConnect", "TeradataOdbcDriver",
  function(drv,
           dsn = NULL,
           ...,
           timezone = "UTC",
           encoding = NULL,
           bigint = c("integer64", "integer", "numeric", "character"),
           driver = NULL,
           server = NULL,
           DBCName = NULL,
           database = "",
           uid = NULL,
           pwd = NULL,
           charset = c("ASCII", "UTF-8", "UTF-16"),
           tmode = c("ANSI", "TERA"),
           dbms.name = NULL,
           .connection_string = NULL) {

    # Preprocessing -----------------------------------------------------------
    bigint <- match.arg(bigint)
    charset <- match.arg(charset)
    tmode <- match.arg(tmode)
    if (!is.null(encoding)) {
      charset <- encoding
    }
    if (!is.null(server)) {
      DBCName <- server
    }
    if (!is.null(dsn)) {
      stop("DSN is unsupported yet.")
    }
    port <- 1025
    if (!is.null(list(...)$port)) {
      port <- list(...)$port
    }

    # DB Connection -----------------------------------------------------------
    dbms.name <- "Teradata"
    dbConnectODBC <- getMethod("dbConnect", c("OdbcDriver"))
    observer <- getOption("connectionObserver")
    options(connectionObserver = NULL)
    tryCatch({
      con <- dbConnectODBC(
        drv, timezone = timezone, encoding = "", bigint = bigint,
        driver = driver, DBCName = DBCName, database = database,
        uid = uid, pwd = pwd, charset = charset, tmode = tmode, port = port,
        dbms.name = dbms.name,
        .connection_string = .connection_string, ...=...)
      info <- generate_connection_info(
        dbname = database, dbms.name = dbms.name, uid = uid, DBCName = DBCName,
        port = port, driver = driver, info = con@info)
      con@info <- info
      con@quote <- '"'
    }, finally = options(connectionObserver = observer))

    if (!is.null(getOption("connectionObserver"))) {
      addTaskCallback(function(expr, ...) {
        tryCatch({
          if (is.call(expr) && identical(expr[[1]], as.symbol("<-"))) {
            connection <- eval(expr[[2]])
            observer <- getOption("connectionObserver")
            observer$connectionOpened(
              type = info$dbms.name,
              displayName = sprintf("%s - %s@%s", info$dbname, info$username, info$servername),
              host = odbc:::computeHostName(connection),
              connectCode = paste(c("library(dplyr.teradata)", deparse(expr)), collapse = "\n"),
              disconnect = function() odbc::dbDisconnect(connection),
              listObjectTypes = function () odbc::odbcListObjectTypes(connection),
              listObjects = function(...) odbc::odbcListObjects(connection, ...),
              listColumns = function(...) odbc::odbcListColumns(connection, ...),
              previewObject = function(rowLimit, ...) odbcPreviewObject(connection, rowLimit, ...),
              actions = odbc::odbcConnectionActions(connection),
              connectionObject = connection
            )
          }
        }, error = function(e) {
          warning("Could not notify connection observer. ", e$message, call. = FALSE)
        })

        # always return false so the task callback is run at most once
        FALSE
      })
    } # nocov end

    con
  }
)

generate_connection_info <- function(dbname, dbms.name, uid, DBCName, port, driver, info) {
  info <- list(
    dbname = dbname,
    dbms.name = dbms.name,
    db.version = "",
    username = uid,
    host = DBCName,
    port = port,
    sourcename = "",
    servername = DBCName,
    drivername = driver,
    odbc.version = info$odbc.version,
    driver.version = "",
    odbcdriver.version = "",
    supports.transactions = info$supports.transactions)
  class(info) <- c(dbms.name, "driver_info", "list")
  info
}
