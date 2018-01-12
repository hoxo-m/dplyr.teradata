context("translate-teradata")

test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_teradata())
  }

  expect_equal(trans(case_when(x == 1L ~ 1L, x == 2L ~ 2L, TRUE ~ 3L)),
               sql('CASE WHEN "x" = 1 THEN 1 WHEN "x" = 2 THEN 2 ELSE 3 END'))
  expect_equal(trans(extract("YEAR", x)),   sql("EXTRACT(YEAR FROM `x`)"))
  expect_equal(trans(year(x)),              sql("EXTRACT(YEAR FROM `x`)"))
  expect_equal(trans(month(x)),             sql("EXTRACT(MONTH FROM `x`)"))
  expect_equal(trans(day(x)),               sql("EXTRACT(DAY FROM `x`)"))
  expect_equal(trans(hour(x)),              sql("EXTRACT(HOUR FROM `x`)"))
  expect_equal(trans(minute(x)),            sql("EXTRACT(MINUTE FROM `x`)"))
  expect_equal(trans(second(x)),            sql("EXTRACT(SECOND FROM `x`)"))
  expect_equal(trans(as_date(x)),           sql("CAST(`x` AS DATE)"))
  expect_equal(trans(cut(x, 1:3)),
               sql("CASE\n WHEN x > 1 AND x <= 2 THEN '(1,2]'\n WHEN x > 2 AND x <= 3 THEN '(2,3]'\n ELSE NULL\nEND"))
  expect_equal(trans(like(x, "%pattern_")), sql("`x` LIKE '%pattern_'"))
  expect_equal(trans(to_timestamp(x)),
               sql("CAST(DATE '1970-01-01' + (`x` / 86400) AS TIMESTAMP(0)) + (`x` MOD 86400) * (INTERVAL '00:00:01' HOUR TO SECOND)"))
  expect_equal(trans(x %% 5L), sql("`x` MOD 5"))
})
