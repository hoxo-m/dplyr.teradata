context("translate-teradata")

test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_teradata())
  }

  expect_equal(trans(case_when(x == 1L ~ 1L, x == 2L ~ 2L, TRUE ~ 3L)),
               sql('CASE\nWHEN (`x` = 1) THEN (1)\nWHEN (`x` = 2) THEN (2)\nELSE (3)\nEND'))
  expect_equal(trans(cut(x, 1:3)),
               sql("CASE\n WHEN x > 1 AND x <= 2 THEN '(1,2]'\n WHEN x > 2 AND x <= 3 THEN '(2,3]'\n ELSE NULL\nEND"))
  expect_equal(trans(like(x, "%pattern_")), sql("`x` LIKE '%pattern_'"))
  expect_equal(trans(to_timestamp(x)),
               sql("CAST(DATE '1970-01-01' + (`x` / 86400) AS TIMESTAMP(0)) + (`x` MOD 86400) * (INTERVAL '00:00:01' HOUR TO SECOND)"))
  expect_equal(trans(x %% 5L), sql("`x` MOD 5"))
  expect_equal(trans(count_if(x > 0L)), sql("SUM(CASE WHEN (`x` > 0) THEN 1 WHEN NOT(`x` > 0) THEN 0 END)"))
  expect_equal(trans(n_if(x > 0L)), sql("SUM(CASE WHEN (`x` > 0) THEN 1 WHEN NOT(`x` > 0) THEN 0 END)"))
  expect_equal(trans(bool_to_int(x > 0L)), sql("CASE WHEN (`x` > 0) THEN 1 WHEN NOT(`x` > 0) THEN 0 END"))
})
