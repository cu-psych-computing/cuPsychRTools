context("Multiple-file reading helpers")
library(cuPsychRTools)

test_that("read_delim returns long data", {
  expect_equal(30L, nrow(read_delim_multi(system.file("extdata",
                                                 paste0("test_data_", 1:3, ".csv"),
                                                 package = "cuPsychRTools"),
                                     delim = ",")))
})

test_that("read_delim allows unmatched column names in files", {
  expect_equal(40L, nrow(read_delim_multi(system.file("extdata",
                                                      paste0("test_data_", 1:4, ".csv"),
                                                      package = "cuPsychRTools"),
                                          delim = ",")))
  expect_equal(6L, ncol(read_delim_multi(system.file("extdata",
                                                      paste0("test_data_", 1:4, ".csv"),
                                                      package = "cuPsychRTools"),
                                          delim = ",")))
})

test_that("read_delim handles different data types in matched columns", {
  expect_equal(40L, nrow(read_delim_multi(system.file("extdata",
                                                      paste0("test_data_", c(1:3, 5), ".csv"),
                                                      package = "cuPsychRTools"),
                                          delim = ",")))
})

test_that("read_csv works", {
  expect_equal(30L, nrow(read_csv_multi(system.file("extdata",
                                                      paste0("test_data_", 1:3, ".csv"),
                                                      package = "cuPsychRTools"))))
})


test_that("read_tsv works", {
  expect_equal(30L, nrow(read_tsv_multi(system.file("extdata",
                                                    paste0("test_data_", 1:3, ".tsv"),
                                                    package = "cuPsychRTools"))))
})
