# A Teradata Backend for dplyr
Koji MAKIYAMA (@hoxo_m)  

<!-- README.md is generated from README.Rmd. Please edit that file -->



[![CRAN Version](http://www.r-pkg.org/badges/version/dplyr.teradata)](https://cran.r-project.org/package=dplyr.teradata)

## 1. Overview

The package provides a Teradata backend for **dplyr**.


```r
library(dplyr.teradata)

con <- dbConnect(dplyr.teradata::todbc(), 
                 driver = "{Teradata Driver}", DBCName = "localhost",
                 uid = "*****", pwd = "*****")
my_table <- tbl(con, "my_table")

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

df <- q %>% collect
df
#> # A tibble: 3 x 2
#>          date        n
#>        <date>    <int>
#>  1 2017-01-01   123456
#>  2 2017-01-02  7891011
#>  3 2017-01-03 12131415
```

## 2. Installation

You can install the package from GitHub.


```r
install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("hoxo-m/dplyr.teradata")
```

The source code for **dplyr.teradata** package is available on GitHub at

- https://github.com/hoxo-m/dplyr.teradata.

## 3. Details

## 4. Related work

- [A Teradata backend for dplyr](https://github.com/xiaodaigh/teradata.dplyr)
- [Dplyr backends: the ultimate collection](https://gist.github.com/piccolbo/3d8ac40291f4eaee644b)
