---
title: "A Teradata Backend for dplyr"
author: Koji MAKIYAMA (@hoxo_m)
output:
  html_document:
    keep_md: true
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



[![CRAN Version](http://www.r-pkg.org/badges/version/dplyr.teradata)](https://cran.r-project.org/package=dplyr.teradata)

## 1 Overview

The package provides a Teradata backend for **dplyr**. 

It makes it possible to operate [Teradata Database](https://www.teradata.com/products-and-services/teradata-database/) in the same way as manipulating data frames with **dplyr** package.


```r
library(dplyr.teradata)

# Establish a connection to Teradata
con <- dbConnect(todbc(), 
                 driver = "{Teradata Driver}", DBCName = "host_name_or_IP_address",
                 uid = "user_name", pwd = "*****")
my_table <- tbl(con, "my_table_name")

# Build a query
q <- my_table %>% 
  select(date) %>%
  filter(between(date, "2017-01-01", "2017-01-03")) %>% 
  group_by(date) %>%
  summarise(n = n()) %>%
  arrange(date)

show_query(q)
#> <SQL>
#> SELECT "date", count(*) AS "n"
#> FROM (SELECT "date" AS "date"
#> FROM "my_table") "jmagvgwapc"
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

## 2 Installation

You can install the package from GitHub.


```r
install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("hoxo-m/dplyr.teradata")
```

The source code for **dplyr.teradata** package is available on GitHub at

- https://github.com/hoxo-m/dplyr.teradata.

## 3 Details

The package provides a Teradata backend for **dplyr**. 
It makes it possible to build SQL for [Teradata Database](https://www.teradata.com/products-and-services/teradata-database/) in the same way as manipulating data frames with **dplyr** package.
It also can send the queries and then receive its results on R.

Therefore, you can complete data analysis with Teradata only on R.
It means that you are freed from troublesome switching of tools and switching thoughts that cause mistakes.

### 3.1 Usage

The package uses **odbc** package to connect database and **dbplyr** package to build SQL.

First, you need to establish an ODBC connection to Teradata. See: 

- [README - **odbc** package](https://cran.r-project.org/web/packages/odbc/README.html).

The package have special driver function `todbc()`. 


```r
# Establish a connection to Teradata
con <- dbConnect(todbc(), 
                 driver = "{Teradata Driver}", DBCName = "host_name_or_IP_address",
                 uid = "user_name", pwd = "*****")
```

Second, you need to specify a table to build SQL. See:

- [Introduction to dbplyr • dbplyr](http://dbplyr.tidyverse.org/articles/dbplyr.html).

To specify tables, you can use `tbl()`:


```r
# Getting table
my_table <- tbl(con, "my_table_name")

# Getting table in schema
my_table <- tbl(con, "my_schema_name.my_table_name")
```

Third, you build queries. It can do in the same way as manipulating data frames with **dplyr**:

- [A Grammar of Data Manipulation • dplyr](http://dplyr.tidyverse.org/).

For example, you can use follows:

- `mutate()` adds new *columns* that are functions of existing *columns*.
- `select()` picks *columns* based on their names.
- `filter()` picks cases based on their values.
- `summarise()` reduces multiple values down to a single summary.
- `arrange()` changes the ordering of the rows.


```r
# Build a query
q <- my_table %>% 
  select(date) %>%
  filter(between(date, "2017-01-01", "2017-01-03")) %>% 
  group_by(date) %>%
  summarise(n = n()) %>%
  arrange(date)
```

`n()` is a function in **dplyr** to return the number of observations in the current group but here it will be translated to `count(*)` as a SQL function.

If you want to show built queries, use `show_query()`:


```r
show_query(q)
#> <SQL>
#> SELECT "date", count(*) AS "n"
#> FROM (SELECT "date" AS "date"
#> FROM "my_table") "jmagvgwapc"
#> WHERE ("date" BETWEEN '2017-01-01' AND '2017-01-03')
#> GROUP BY "date"
#> ORDER BY "date"
```

Finally, you send built queries and get its results on R using `collect()`.


```r
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

### 3.2 Translatable functions

The package mainly use **dbplyr** to translate manipulations into queries.

*Translatable functions* are the available functions in manipulation that it can translate into SQL functions.

To know translatable functions for Teradata, refer the following:

- [Adds Teradata translation](https://github.com/tidyverse/dbplyr/pull/43)

Here, we introduce the special translatable functions that it becomes available by **dplyr.teradata**.



#### 3.2.1 **lubridate** friendly functions

You might familiar the **lubridate** package to manipulate date and time data.

**dplyr.teradata** has **lubridate** friendly functions:

- `year()`, `month()`, `day()`, `hour()`, `minutes()`, `second()`

For example, you can pick year from date type column.


```r
mutate(year = year(date_type_column))
```

Such as above manipulation is translated into SQL like following:


```
#> <SQL> EXTRACT(YEAR FROM `date_type_column`)
```

#### 3.2.2 `to_timestamp()`

If your table has a column stored UNIX time and you want to convert it to timestamp, you need to write complex SQL.

`to_timestamp()` is a translatable function that makes it easy.


```r
mutate(ts = to_timestamp(unixtime_column))
```

Such as above manipulation is translated into SQL like following:


```
#> <SQL> CAST(DATE '1970-01-01' + (`unixtime_column` / 86400) AS TIMESTAMP(0)) + (`unixtime_column` MOD 86400) * (INTERVAL '00:00:01' HOUR TO SECOND)
```

#### 3.2.3 `cut()`

`cut()` is very useful function that you can use in base R.

For example, you want to cut values of `x` into three parts of ranges by breaks points 2 and 4:


```r
x <- 1:6
breaks <- c(0, 2, 4, 6)
cut(x, breaks)
#> [1] (0,2] (0,2] (2,4] (2,4] (4,6] (4,6]
#> Levels: (0,2] (2,4] (4,6]
```

**dplyr.teradata** has a translatable function similar to this:


```r
breaks = c(0, 2, 4, 6)
mutate(y = cut(x, breaks))
```

In the result, it is translated into a SQL `CASE WHEN` statement as follows:


```
#> <SQL> CASE
#>  WHEN x > 0 AND x <= 2 THEN '(0,2]'
#>  WHEN x > 2 AND x <= 4 THEN '(2,4]'
#>  WHEN x > 4 AND x <= 6 THEN '(4,6]'
#>  ELSE NULL
#> END
```

Arguments of base `cut()` are also available:


```r
breaks = c(0, 2, 4, 6)
mutate(y = cut(x, breaks, labels = "-", include.lowest = TRUE))
```


```
#> <SQL> CASE
#>  WHEN x >= 0 AND x <= 2 THEN '0-2'
#>  WHEN x > 2 AND x <= 4 THEN '3-4'
#>  WHEN x > 4 AND x <= 6 THEN '5-6'
#>  ELSE NULL
#> END
```

### 3.3 Other useful functions

#### 3.3.1 `blob_to_string()`

`blob` object from databases prevents manipulation with **dplyr**.

You might want to convert them to string.

`blob_to_string()` is a function to make it easy:


```r
x <- blob::as.blob("Good morning")
x
#> [1] blob[12 B]

# print raw data in blob
x[[1]]
#>  [1] 47 6f 6f 64 20 6d 6f 72 6e 69 6e 67

blob_to_string(x)
#> [1] "476f6f64206d6f726e696e67"
```

## 4 Related work

- [A 'dplyr' Backend for Databases • dbplyr](http://dbplyr.tidyverse.org/)
- [A Teradata backend for dplyr](https://github.com/xiaodaigh/teradata.dplyr)
- [Dplyr backends: the ultimate collection](https://gist.github.com/piccolbo/3d8ac40291f4eaee644b)
