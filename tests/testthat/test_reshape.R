context("Tidy reshaping helpers")
library(cuPsychRTools)

test_that("super_spread allows any number of implicit grouping columns", {
  # All tested with two value columns
  # Equality expectation: number of total "observations"
  # One "grouping" column: id
  # One key column: condition
  expect_equal(10L, nrow(super_spread(expand.grid(id = 1L:10L,
                                                  condition = c("a", "b"),
                                                  value_1 = 0L,
                                                  value_2 = 1L),
                                      condition, value_1:value_2)))
  # Multiple "grouping" columns: id, condition_1
  # One key column: condition_2
  expect_equal(20L, nrow(super_spread(expand.grid(id = 1L:10L,
                                                  condition_1 = c("a", "b"),
                                                  condition_2 = c("c", "d"),
                                                  value_1 = 0L,
                                                  value_2 = 1L),
                                      condition_2, value_1:value_2)))
  # One "grouping" column: id
  # Multiple key columns: condition_1, condition_2
  expect_equal(10L, nrow(super_spread(expand.grid(id = 1L:10L,
                                                  condition_1 = c("a", "b"),
                                                  condition_2 = c("c", "d"),
                                                  value_1 = 0L,
                                                  value_2 = 1L),
                                      condition_1:condition_2, value_1:value_2)))
})

test_that("super_spread accepts any number of key columns", {
  # All tested with one grouping column and two value columns
  # Equality expectation: 2^n * 2 + 1 where
  # n is number of conditions to cross (2 is number of unique values per condition)
  # 2 is number of value cols
  # 1 is number of grouping cols
  # One key column: condition
  expect_equal(2^1 * 2 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                          condition = c("a", "b"),
                                                          value_1 = 0L,
                                                          value_2 = 1L),
                                              condition, value_1:value_2)))
  # Two key columns: condition_1, condition_2
  expect_equal(2^2 * 2 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                          condition_1 = c("a", "b"),
                                                          condition_2 = c("c", "d"),
                                                          value_1 = 0L,
                                                          value_2 = 1L),
                                              condition_1:condition_2, value_1:value_2)))
  # Three key columns: condition_1, condition_2, condition_3
  expect_equal(2^3 * 2 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                          condition_1 = c("a", "b"),
                                                          condition_2 = c("c", "d"),
                                                          condition_3 = c("e", "f"),
                                                          value_1 = 0L,
                                                          value_2 = 1L),
                                              condition_1:condition_3, value_1:value_2)))
})

test_that("super_spread accepts any number of value columns", {
  # All tested with one grouping column and one key column
  # Equality expectation: 2 * n + 1 where
  # 2 is total nunber of crossed condition values
  # n is number of value cols
  # 1 is number of grouping cols
  # One value column: value
  expect_equal(2 * 1 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                        condition = c("a", "b"),
                                                        value = 0L),
                                            condition, value)))
  # Two value columns: value_1, value_2
  expect_equal(2 * 2 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                        condition = c("a", "b"),
                                                        value_1 = 0L,
                                                        value_2 = 1L),
                                            condition, value_1:value_2)))
  # Three value columns: value_1, value_2, value_3
  expect_equal(2 * 3 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                        condition = c("a", "b"),
                                                        value_1 = 0L,
                                                        value_2 = 1L,
                                                        value_3 = 2L),
                                            condition, value_1:value_3)))
})

test_that("super_spread accepts tidyselect vars", {
  # discontinuous value columns
  expect_equal(2 * 3 + 1 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                            condition = c("a", "b"),
                                                            value_1 = 0L,
                                                            value_2 = 1L,
                                                            value_3 = 2L,
                                                            value_4 = 3L),
                                                condition, value_1:value_2, value_4)))
  # tidyselect helpers for value
  expect_equal(2 * 3 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                        condition = c("a", "b"),
                                                        value_1 = 0L,
                                                        value_2 = 1L,
                                                        value_3 = 2L),
                                            condition, dplyr::starts_with("value"))))
  # tidyselect helpers for key
  expect_equal(2^2 * 3 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                          condition_1 = c("a", "b"),
                                                          condition_2 = c("c", "d"),
                                                          value_1 = 0L,
                                                          value_2 = 1L,
                                                          value_3 = 2L),
                                              dplyr::starts_with("condition"),
                                              dplyr::starts_with("value"))))
  # c() in the key argument (since dots only go to value)
  expect_equal(2^2 * 3 + 1, ncol(super_spread(expand.grid(id = 1L:10L,
                                                          condition_1 = c("a", "b"),
                                                          condition_2 = c("c", "d"),
                                                          value_1 = 0L,
                                                          value_2 = 1L,
                                                          value_3 = 2L),
                                              c(condition_1, condition_2),
                                              dplyr::starts_with("value"))))
})

expect_in <- function (object, expected_in) {
  expect_true(object %in% expected_in,
              info = paste(object, "was not found in", tail(substitute(expected_in), 1)))
}

test_that("super_spread name_order works", {
  expect_in("a_value_1", names(super_spread(expand.grid(id = 1L:10L,
                                                        condition = c("a", "b"),
                                                        value_1 = 0L,
                                                        value_2 = 1L),
                                            condition, value_1:value_2,
                                            name_order = "key_first")))
  expect_in("value_1_a", names(super_spread(expand.grid(id = 1L:10L,
                                                        condition = c("a", "b"),
                                                        value_1 = 0L,
                                                        value_2 = 1L),
                                            condition, value_1:value_2,
                                            name_order = "value_first")))
})

test_that("super_gather works when fully specified", {
  expect_equal(10 * 2, nrow(super_gather(data.frame(id = 1:10,
                                                    value1_cond1 = "a",
                                                    value1_cond2 = "b",
                                                    value2_cond1 = "e",
                                                    value2_cond2 = "f",
                                                    value3_cond1 = "c",
                                                    value3_cond2 = "d"),
                                         key_names = c("cond1", "cond2"),
                                         name_order = "value_first")))
  expect_equal(10 * 2, nrow(super_gather(data.frame(id = 1:10,
                                                    value1_cond1 = "a",
                                                    value1_cond2 = "b",
                                                    value2_cond1 = "e",
                                                    value2_cond2 = "f",
                                                    value3_cond1 = "c",
                                                    value3_cond2 = "d"),
                                         value_names = c("value1", "value2", "value3"),
                                         name_order = "value_first")))
  expect_equal(10 * 2, nrow(super_gather(data.frame(id = 1:10,
                                                    cond1_value1 = "a",
                                                    cond2_value1 = "b",
                                                    cond1_value2 = "e",
                                                    cond2_value2 = "f",
                                                    cond1_value3 = "c",
                                                    cond2_value3 = "d"),
                                         key_names = c("cond1", "cond2"),
                                         name_order = "key_first")))
  expect_equal(10 * 2, nrow(super_gather(data.frame(id = 1:10,
                                                    cond1_value1 = "a",
                                                    cond2_value1 = "b",
                                                    cond1_value2 = "e",
                                                    cond2_value2 = "f",
                                                    cond1_value3 = "c",
                                                    cond2_value3 = "d"),
                                         value_names = c("value1", "value2", "value3"),
                                         name_order = "key_first")))
})

test_that("super_gather position-based separation works", {
  expect_equal(10 * 2, nrow(super_gather(data.frame(id = 1:10,
                                                    value_1_cond1 = "a",
                                                    value_1_cond2 = "b",
                                                    value_2_cond1 = "e",
                                                    value_2_cond2 = "f",
                                                    value_3_cond1 = "c",
                                                    value_3_cond2 = "d"),
                                         key_names = c("cond1", "cond2"),
                                         name_order = "value_first")))
})
