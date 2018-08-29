context("Tidy row sums & means")
library(cuPsychRTools)

a <- 1:5
b <- 1:5
c <- 1:5

df <- tibble::tibble(x1 = 1:5, x2 = 1:5, x3 = 1:5)

test_that("tidy_row_sums behaves as planned", {
  expect_equal(1:5 * 3, tidy_row_sums(a, b, c))
})

test_that("tidy_row_means behaves as planned", {
  expect_equal(1:5, tidy_row_means(a, b, c))
})

test_that("tidyselect_row_sums behaves as planned", {
  expect_equal(1:5 * 3, tidyselect_row_sums(df, .value = "this_sum", tidyselect::starts_with("x"))$this_sum)
})

test_that("tidyselect_row_means behaves as planned", {
  expect_equal(1:5, tidyselect_row_means(df, .value = "this_mean", tidyselect::starts_with("x"))$this_mean)
})