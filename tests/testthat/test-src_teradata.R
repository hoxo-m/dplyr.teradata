context("src_teradata")

TEST_TABLE <- Sys.getenv("TD_TEST_TABLE")

test_that("Create src object", {
  skip_on_cran()
  skip_on_travis()
  src <- src_teradata()
  expect_s3_class(src, "src_teradata")
  expect_output(print(src), "src: .+tbls: ")
})

test_that("Create tbl object", {
  skip_on_cran()
  skip_on_travis()
  src <- src_teradata()
  table <- tbl(src, TEST_TABLE)
  expect_s3_class(table, "tbl_teradata")
  expect_output(print(table), "Source: .+Database: ")
})
