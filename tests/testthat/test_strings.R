context("String manipulation helpers")
library(cuPsychRTools)

test_that("str_locate_whichever behaves as planned", {
  expect_equal(length(c("apple", "banana", "blueberry")),
               nrow(str_locate_whichever(c("apple", "banana", "blueberry"),
                                         c("ap", "ba", "bl"))))
})

test_that("str_locate_whichever throws warning for multiple matches", {
  expect_warning(str_locate_whichever(c("apple", "banana", "blueberry"),
                                         c("a", "b", "l")))
})
