context("Tidy row sums & means")
library(cuPsychRTools)

a <- 1:5
b <- 1:5
c <- 1:5

test_that("tidy_row_sums behaves as planned", {
  expect_equal(1:5 * 3, tidy_row_sums(a, b, c))
})

test_that("tidy_row_means behaves as planned", {
  expect_equal(1:5, tidy_row_means(a, b, c))
})