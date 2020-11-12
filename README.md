<!-- README.md is generated from README.Rmd. Please edit that file -->

A Teradata Backend for dplyr
============================

#### *Koji Makiyama (@hoxo-m)*

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.org/hoxo-m/dplyr.teradata.svg?branch=master)](https://travis-ci.org/hoxo-m/dplyr.teradata)
[![CRAN
Version](https://www.r-pkg.org/badges/version-ago/dplyr.teradata)](https://cran.r-project.org/package=dplyr.teradata)
[![](https://cranlogs.r-pkg.org/badges/dplyr.teradata)](https://cran.r-project.org/package=dplyr.teradata)
[![Coverage
Status](https://img.shields.io/coveralls/hoxo-m/dplyr.teradata.svg)](https://coveralls.io/github/hoxo-m/dplyr.teradata)
<!-- badges: end -->

1. Overview
-----------

The package provides a Teradata backend for **dplyr**.

It makes it possible to operate [Teradata
Database](https://www.teradata.com/products-and-services/teradata-database/)
in the same way as manipulating data frames with **dplyr**.

``` r
library(dplyr.teradata)

# Establish a connection to Teradata
con <- dbConnect(todbc(), 
                 driver = "{Teradata Driver}", DBCName = "host_name_or_IP_address",
                 uid = "user_name", pwd = "*****")
my_table <- tbl(con, "my_table_name")

# Build a query
q <- my_table %>% 
  filter(between(date, "2017-01-01", "2017-01-03")) %>% 
  group_by(date) %>%
  summarise(n = n()) %>%
  arrange(date)

show_query(q)
#> <SQL>
#> SELECT "date", count(*) AS "n"
#> FROM "my_table_name"
#> WHERE ("date" BETWEEN '2017-01-01' AND '2017-01-03')
#> GROUP BY "date"
#> ORDER BY "date"

# Send the query and get its result on R
df <- q %>% collect
df
#> # A tibble: 3 x 2
#>          date        n
#>        <date>    <int>
#>  1 2017-01-01   123456
#>  2 2017-01-02  7891011
#>  3 2017-01-03 12131415
```

2. Installation
---------------

You can install the **dplyr.teradata** package from CRAN.

``` r
install.packages("dplyr.teradata")
```

You can also install the development version of the package from GitHub.

``` r
install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("hoxo-m/dplyr.teradata")
```

The source code for **dplyr.teradata** package is available on GitHub at

-   <https://github.com/hoxo-m/dplyr.teradata>.

3. Motivation
-------------

The package provides a Teradata backend for **dplyr**. It makes it
possible to build SQL for [Teradata
Database](https://www.teradata.com/products-and-services/teradata-database/)
in the same way as manipulating data frames with the **dplyr** package.
It also can send the queries and then receive its results on R.

Therefore, you can complete data analysis with Teradata only on R. It
means that you are freed from troublesome switching of tools and
switching thoughts that cause mistakes.

4. Usage
--------

The package uses the **odbc** package to connect database and the
**dbplyr** package to build SQL.

First, you need to establish an ODBC connection to Teradata. See:

-   [README - **odbc**
    package](https://CRAN.R-project.org/package=odbc/readme/README.html).

``` r
# Establish a connection to Teradata
con <- dbConnect(odbc(), 
                 driver = "{Teradata Driver}", DBCName = "host_name_or_IP_address",
                 uid = "user_name", pwd = "*****")
```

Second, you need to specify a table to build SQL. See:

-   [Introduction to dbplyr •
    dbplyr](https://dbplyr.tidyverse.org/articles/dbplyr.html).

To specify a table, you can use `tbl()`:

``` r
# Getting table
my_table <- tbl(con, "my_table_name")

# Getting table in schema
my_table <- tbl(con, in_schema("my_schema", "my_table_name"))
```

Third, you build queries. It can do in the same way as manipulating data
frames with **dplyr**:

-   [A Grammar of Data Manipulation •
    dplyr](https://dplyr.tidyverse.org/).

For example, you can use follows:

-   `mutate()` adds new *columns* that are functions of existing
    *columns*.
-   `select()` picks *columns* based on their names.
-   `filter()` picks *rows* based on their values.
-   `summarise()` reduces multiple values down to a single summary.
-   `arrange()` changes the ordering of the rows.

``` r
# Build a query
q <- my_table %>% 
  filter(between(date, "2017-01-01", "2017-01-03")) %>% 
  group_by(date) %>%
  summarise(n = n()) %>%
  arrange(date)
```

`n()` is a function in **dplyr** to return the number of rows in the
current group but here it will be translated to `count(*)` as a SQL
function.

If you want to show built queries, use `show_query()`:

``` r
show_query(q)
#> <SQL>
#> SELECT "date", count(*) AS "n"
#> FROM "my_table_name"
#> WHERE ("date" BETWEEN '2017-01-01' AND '2017-01-03')
#> GROUP BY "date"
#> ORDER BY "date"
```

Finally, you send built queries and get its results on R using
`collect()`.

``` r
# Send the query and get its result on R
df <- q %>% collect
df
#> # A tibble: 3 x 2
#>          date        n
#>        <date>    <int>
#>  1 2017-01-01   123456
#>  2 2017-01-02  7891011
#>  3 2017-01-03 12131415
```

5. Translatable functions
-------------------------

The package mainly use **dbplyr** to translate manipulations into
queries.

*Translatable functions* are the available functions in manipulations
that it can translate into SQL functions.

For instance, `n()` is translated to `count(*)` in the above example.

To know translatable functions for Teradata, refer the following:

-   [Adds Teradata
    translation](https://github.com/tidyverse/dbplyr/pull/43)

Here, we introduce the special translatable functions that it becomes
available by **dplyr.teradata**.

### 5.1. Treat Boolean

Teradata does not have the boolean data type. So when you use boolean,
you need to write some complex statements. The package has several
functions to treat it briefly.

`bool_to_int` transforms boolean to integer.

``` r
mutate(is_positive = bool_to_int(x > 0L))
```

    #> <SQL> CASE WHEN (`x` > 0) THEN 1 WHEN NOT(`x` > 0) THEN 0 END

`count_if()` or `n_if()` counts a number of rows satisfying a condition.

``` r
summarize(n = count_if(x > 0L))
```

    #> <SQL> SUM(CASE WHEN (`x` > 0) THEN 1 WHEN NOT(`x` > 0) THEN 0 END)

### 5.2. `to_timestamp()`

When your tables has some columns stored UNIX time and you want to
convert it to timestamp, you need to write complex SQL.

`to_timestamp()` is a translatable function that makes it easy.

``` r
mutate(ts = to_timestamp(unixtime_column))
```

Such as above manipulation is translated into SQL like following:

    #> <SQL> CAST(DATE '1970-01-01' + (`unixtime_column` / 86400) AS TIMESTAMP(0)) + (`unixtime_column` MOD 86400) * (INTERVAL '00:00:01' HOUR TO SECOND)

### 5.3. `cut()`

`cut()` is very useful function that you can use in base R.

For example, you want to cut values of `x` into three parts of ranges by
break points 2 and 4:

``` r
x <- 1:6
breaks <- c(0, 2, 4, 6)
cut(x, breaks)
#> [1] (0,2] (0,2] (2,4] (2,4] (4,6] (4,6]
#> Levels: (0,2] (2,4] (4,6]
```

**dplyr.teradata** has a translatable function similar to this:

``` r
breaks = c(0, 2, 4, 6)
mutate(y = cut(x, breaks))
```

In the result, it is translated to a `CASE WHEN` statement as follows:

    #> <SQL> CASE
    #>  WHEN x > 0 AND x <= 2 THEN '(0,2]'
    #>  WHEN x > 2 AND x <= 4 THEN '(2,4]'
    #>  WHEN x > 4 AND x <= 6 THEN '(4,6]'
    #>  ELSE NULL
    #> END

Arguments of base `cut()` are also available:

``` r
breaks = c(0, 2, 4, 6)
mutate(y = cut(x, breaks, labels = "-", include.lowest = TRUE))
```

    #> <SQL> CASE
    #>  WHEN x >= 0 AND x <= 2 THEN '0-2'
    #>  WHEN x > 2 AND x <= 4 THEN '3-4'
    #>  WHEN x > 4 AND x <= 6 THEN '5-6'
    #>  ELSE NULL
    #> END

6. Other useful functions
-------------------------

### 6.1. `blob_to_string()`

The `blob` object from databases sometimes prevents manipulations with
**dplyr**.

You might want to convert them to string.

`blob_to_string()` is a function to make it easy:

``` r
x <- blob::as_blob("Good morning")
x
#> <blob[1]>
#> [1] blob[12 B]

# print raw data in blob
x[[1]]
#>  [1] 47 6f 6f 64 20 6d 6f 72 6e 69 6e 67

blob_to_string(x)
#> [1] "476f6f64206d6f726e696e67"
```

7. Related work
---------------

-   [A ‘dplyr’ Backend for Databases •
    dbplyr](https://dbplyr.tidyverse.org/)
