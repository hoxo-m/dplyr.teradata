#' Connect to Teradata.
#'
#' Use \code{src_teradata} to connect to an existing Teradata database,
#' and \code{tbl} to connect to tables within that database.
#' If you're connecting to a remote database,
#' ask your database administrator for the values of these variables.
#'
#' @param dbname Database name
#' @param host Host name of database
#' @param port Don't use
#' @param user,password User name and password (if needed)
#' @param charset Character Set. \code{"UTF8"}(default), \code{"ASCII"} or \code{"UTF16"}.
#' @param tmode TMODE. \code{"ANSI"}(default) or \code{"TERA"}.
#' @param type Connection Type. \code{"odbc"}(deafult), \code{"odbc2"} or \code{"jdbc"}. See detail.
#' @param src a Teradata src created with \code{src_teradata}.
#' @param from Either a string giving the name of table in database, or
#'   \code{\link{sql}} described a derived table or compound join.
#' @param ... Included for compatibility with the generic, but otherwise ignored.
#'
#' @importFrom rstudioapi isAvailable askForPassword
#' @export
src_teradata <- function(dbname = NULL, host = NULL, port = NULL, user = NULL,
                         password = NULL, charset = c("UTF8", "ASCII", "UTF16"),
                         tmode = c("ANSI", "TERA"), type = c("odbc", "odbc2", "jdbc")) {

  # Prepare Arguments -------------------------------------------------------
  if (is.null(dbname)) dbname <- Sys.getenv("TDDATABASE")
  if (is.null(host)) host <- Sys.getenv("TDHOST")
  if (is.null(user)) user <- Sys.getenv("TDUSER")
  if (is.null(password)) password <- Sys.getenv("TDPASSWORD")
  if (password == "" && isAvailable()) {
    password <- askForPassword("Input Password for Teradata")
  }
  charset <- match.arg(charset)
  tmode <- match.arg(tmode)
  type <- match.arg(type)

  # Create Connection -------------------------------------------------------
  if (type == "odbc") {
    con <- create_RODBC_connection(dbname, host, user, password, charset, tmode)
    #> class(con) -> TeradataODBCConnection, RODBC
    #> typeof(con) -> integer (S3)
  } else if (type == "odbc2") {
    if (requireNamespace("odbc")) {
      con <- create_odbc_connection(dbname, host, user, password, charset, tmode)
      #> class(con) -> TeradataOdbcConnection, OdbcConnection
      #> typeof(con) -> S4
    } else {
      stop("odbc not found")
    }
  } else if (type == "jdbc") {
    if (requireNamespace("RJDBC")) {
      con <- create_RJDBC_connection(dbname, host, user, password, charset, tmode)
      #> class(con) -> TeradataJDBCConnection, JDBCConnection
      #> typeof(con) -> S4
    } else {
      stop("RJDBC not found")
    }
  }
  attr(con, "dbname") <- dbname
  attr(con, "table_names") <- db_list_tables(con)

  td_version <- get_teradata_version(con)
  info <- list(td_version = td_version, user = user, host = host, dbname = dbname)

  src_sql("teradata", con = con, info = info, disco = db_disconnector(con, "teradata"))
}

#' @export
#' @rdname src_teradata
tbl.src_teradata <- function(src, from, ...) {
  tbl_sql("teradata", src = src, from = sql(from), ...)
}

#' @export
src_desc.src_teradata <- function(x) {
  info <- x$info
  if (info$dbname == "") {
    sprintf("Teradata %s [%s@%s]", info$td_version, info$user, info$host)
  } else {
    sprintf("Teradata %s [%s@%s/%s]", info$td_version, info$user, info$host, info$dbname)
  }
}


# Utility Functions for Create Connection ---------------------------------

#' @importFrom RODBC odbcDriverConnect odbcGetErrMsg
create_RODBC_connection <- function(dbname, host, user, password, charset, tmode) {
  connection_string <- create_odbc_connection_string(dbname, host, user, password, charset, tmode)
  con <- odbcDriverConnect(connection_string)
  if (con == -1L) {
    stop(sprintf("Failed to establish connection: %s@%s/%s", user, host, dbname))
  }
  class(con) <- c("TeradataODBCConnection", class(con))
  con
}

#' @importFrom methods new
#' @importFrom DBI dbConnect
create_odbc_connection <- function(dbname, host, user, password, charset, tmode) {
  connection_string <- create_odbc_connection_string(dbname, host, user, password, charset, tmode)
  if (requireNamespace("fixer")) {
    fixer::fix_params_persist_('methods::getClassDef(Class = `Class[Class != ""]`)', environment())
    con <- dbConnect(odbc::odbc(), .connection_string = connection_string)
    fixer::fix_reset(methods::getClassDef())
    con <- new("TeradataOdbcConnection", con)
    con
  } else {
    stop("fixer not found")
  }
}

create_odbc_connection_string <- function (dbname, host, user, password, charset, tmode) {
  connection_string <- sprintf("Driver=Teradata;DBCName=%s;Charset=%s;TMODE=%s", host, charset, tmode)
  if (dbname != "") connection_string <- sprintf("%s;Database=%s", connection_string, dbname)
  if (user != "") connection_string <- sprintf("%s;UID=%s", connection_string, user)
  if (password != "") connection_string <- sprintf("%s;PWD=%s", connection_string, password)
  sprintf("%s;", connection_string)
}

#' @importFrom methods new
#' @importFrom DBI dbConnect
create_RJDBC_connection <- function(dbname, host, user, password, charset, tmode) {
  drv <- tryCatch({
    RJDBC::JDBC("com.teradata.jdbc.TeraDriver")
  }, error = function(e) {
    rJava::.jpackage("RJDBC")
    add_jdbc_class_path()
    RJDBC::JDBC("com.teradata.jdbc.TeraDriver")
  })
  st <- sprintf("jdbc:teradata://%s", host)
  if (!is.null(dbname)) {
    st <- sprintf("%s/database=%s", st, dbname)
  }
  st <- sprintf("%s,charset=%s,tmode=%s", st, charset, tmode)
  con <- dbConnect(drv, st, user = user, password = password)
  con <- new("TeradataJdbcConnection", con)
  con
}

add_jdbc_class_path <- function(tdstudio_path = "/Applications/TeradataStudio") {
  dir <- list.files(paste0(tdstudio_path, "/plugins"), pattern = "terajdbc", full.names = TRUE)
  message(sprintf("Add Class Path: %s", file.path(dir, "terajdbc4.jar")))
  rJava::.jaddClassPath(file.path(dir, "terajdbc4.jar"))
  message(sprintf("Add Class Path: %s", file.path(dir, "tdgssconfig.jar")))
  rJava::.jaddClassPath(file.path(dir, "tdgssconfig.jar"))
}

if (requireNamespace("odbc")) {
  methods::setClass("TeradataOdbcConnection", contains="OdbcConnection")
}

if (requireNamespace("RJDBC")) {
  methods::setClass("TeradataJDBCConnection", contains="JDBCConnection")
}

# Utility Functions -------------------------------------------------------

#' @importFrom RODBC odbcGetInfo
#' @importFrom DBI dbGetQuery
get_teradata_version <- function(con) {
  td_version <- "??.??.??.??"
  if (inherits(con, "RODBC")) {
    info <- odbcGetInfo(con)
    if ("DBMS_Ver" %in% names(info)) {
      td_version <- strsplit(info["DBMS_Ver"], split = "\\s+")$DBMS_Ver
      td_version <- td_version[length(td_version)]
    }
  } else if (inherits(con, "OdbcConnection") || inherits(con, "JDBCConnection")) {
    sql <- "SELECT * FROM DBC.DBCINFO"
    dbcinfo <- dbGetQuery(con, sql)
    if ("VERSION" %in% dbcinfo$InfoKey) {
      td_version <- dbcinfo$InfoData[dbcinfo$InfoKey == "VERSION"]
    }
  }
  td_version
}

# Creates an environment that disconnects the database when it's
# garbage collected
#' @importFrom RODBC odbcClose
#' @importFrom DBI dbDisconnect
db_disconnector <- function(con, name, quiet = FALSE) {
  if (inherits(con, "RODBC")) {
    reg.finalizer(environment(), function(...) {
      if (!quiet) {
        message("Auto-disconnecting ", name, " connection ",
                "(", paste(con[1], collapse = ", "), ")")
      }
      tryCatch({
        odbcClose(con)
      }, error = function(e) {
        message("Already closed.")
      })
    })
  } else if (inherits(con, "OdbcConnection") || inherits(con, "JDBCConnection")) {
    reg.finalizer(environment(), function(...) {
      if (!quiet) {
        message("Auto-disconnecting ", name, " connection ",
                "(", paste("??", collapse = ", "), ")")
      }
      dbDisconnect(con)
    })
  }
  environment()
}
