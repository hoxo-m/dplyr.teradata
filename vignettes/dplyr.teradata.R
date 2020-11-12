## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-",
  eval = FALSE,
  message = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
#  library(dplyr.teradata)
#  
#  # Establish a connection to Teradata
#  con <- dbConnect(todbc(),
#                   driver = "{Teradata Driver}", DBCName = "host_name_or_IP_address",
#                   uid = "user_name", pwd = "*****")
#  my_table <- tbl(con, "my_table_name")
#  
#  # Build a query
#  q <- my_table %>%
#    filter(between(date, "2017-01-01", "2017-01-03")) %>%
#    group_by(date) %>%
#    summarise(n = n()) %>%
#    arrange(date)
#  
#  show_query(q)
#  #> <SQL>
#  #> SELECT "date", count(*) AS "n"
#  #> FROM "my_table_name"
#  #> WHERE ("date" BETWEEN '2017-01-01' AND '2017-01-03')
#  #> GROUP BY "date"
#  #> ORDER BY "date"
#  
#  # Send the query and get its result on R
#  df <- q %>% collect
#  df
#  #> # A tibble: 3 x 2
#  #>          date        n
#  #>        <date>    <int>
#  #>  1 2017-01-01   123456
#  #>  2 2017-01-02  7891011
#  #>  3 2017-01-03 12131415

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("dplyr.teradata")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("devtools") # if you have not installed "devtools" package
#  devtools::install_github("hoxo-m/dplyr.teradata")

## -----------------------------------------------------------------------------
#  # Establish a connection to Teradata
#  con <- dbConnect(odbc(),
#                   driver = "{Teradata Driver}", DBCName = "host_name_or_IP_address",
#                   uid = "user_name", pwd = "*****")

## -----------------------------------------------------------------------------
#  # Getting table
#  my_table <- tbl(con, "my_table_name")
#  
#  # Getting table in schema
#  my_table <- tbl(con, in_schema("my_schema", "my_table_name"))

## -----------------------------------------------------------------------------
#  # Build a query
#  q <- my_table %>%
#    filter(between(date, "2017-01-01", "2017-01-03")) %>%
#    group_by(date) %>%
#    summarise(n = n()) %>%
#    arrange(date)

## -----------------------------------------------------------------------------
#  show_query(q)
#  #> <SQL>
#  #> SELECT "date", count(*) AS "n"
#  #> FROM "my_table_name"
#  #> WHERE ("date" BETWEEN '2017-01-01' AND '2017-01-03')
#  #> GROUP BY "date"
#  #> ORDER BY "date"

## -----------------------------------------------------------------------------
#  # Send the query and get its result on R
#  df <- q %>% collect
#  df
#  #> # A tibble: 3 x 2
#  #>          date        n
#  #>        <date>    <int>
#  #>  1 2017-01-01   123456
#  #>  2 2017-01-02  7891011
#  #>  3 2017-01-03 12131415

## ----eval=TRUE, echo=FALSE----------------------------------------------------
library(dplyr.teradata)
trans <- function(x) {
  translate_sql(!!enquo(x), con = simulate_teradata())
}

## -----------------------------------------------------------------------------
#  mutate(is_positive = bool_to_int(x > 0L))

## ----echo=FALSE, eval=TRUE----------------------------------------------------
trans(bool_to_int(x > 0L))

## -----------------------------------------------------------------------------
#  summarize(n = count_if(x > 0L))

## ----echo=FALSE, eval=TRUE----------------------------------------------------
trans(count_if(x > 0L))

## -----------------------------------------------------------------------------
#  mutate(ts = to_timestamp(unixtime_column))

## ----echo=FALSE, eval=TRUE----------------------------------------------------
trans(to_timestamp(unixtime_column))

## ----eval=TRUE----------------------------------------------------------------
x <- 1:6
breaks <- c(0, 2, 4, 6)
cut(x, breaks)

## -----------------------------------------------------------------------------
#  breaks = c(0, 2, 4, 6)
#  mutate(y = cut(x, breaks))

## ----echo=FALSE, eval=TRUE----------------------------------------------------
trans(cut(x, c(0, 2, 4, 6)))

## -----------------------------------------------------------------------------
#  breaks = c(0, 2, 4, 6)
#  mutate(y = cut(x, breaks, labels = "-", include.lowest = TRUE))

## ----echo=FALSE, eval=TRUE----------------------------------------------------
trans(cut(x, c(0, 2, 4, 6), labels = "-", include.lowest = TRUE))

## ----eval=TRUE----------------------------------------------------------------
x <- blob::as_blob("Good morning")
x

# print raw data in blob
x[[1]]

blob_to_string(x)

