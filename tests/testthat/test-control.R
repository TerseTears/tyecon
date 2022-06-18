rm(list = ls())
library(testthat)

test_that("single evaluation works", {
  expect_equal(control(
    {
      max(val_vec)
    },
    val_vec = list(c(2, 3, 4), c(4, 2), 5)
  ), tibble::tibble(val_vec = list(c(2, 3, 4), c(4, 2), 5), .value = c(4, 4, 5)))
})

test_that("multiple evaluations work", {
  expect_equal(control(
    {
      max(val_vec) + max(val_vec2)
    },
    val_vec = list(c(2, 3, 4), c(4, 2), 5),
    val_vec2 = list(12, c(4, 9, 1), c(3, 5))
  ), tibble::tibble(
    val_vec = list(c(2, 3, 4), c(4, 2), 5),
    val_vec2 = list(12, c(4, 9, 1), c(3, 5)),
    .value = c(16, 13, 10)
  ))
})

test_that("multiple levels work", {
  expect_equal(control(
    {
      max(val_vec) + max(val_vec2)
    },
    val_vec = list(c(2, 3, 4), 5),
    val_vec2 = list(12, c(4, 9, 1), c(3, 5)) ~ 2
  ), tibble::tibble(
    val_vec = list(c(2, 3, 4), c(2,3,4), c(2,3,4), 5, 5, 5),
    val_vec2 = list(12, c(4, 9, 1), c(3, 5), 12, c(4,9,1), c(3,5)),
    .value = c(16, 13, 9, 17, 14, 10)
  ))
})

test_that("refiner works", {
  expect_equal(control(
    {
      val_vec * 2
    },
    val_vec = 1:5,
    .refiner = ~ dplyr::filter(., val_vec > 2)
  ), tibble::tibble(val_vec = 3:5, .value = (3:5) * 2))
})

test_that("prober works", {
  expect_equal(control(
    {
      val_vec * 2
    },
    val_vec = 1:5,
    .prober = ~ . > 4,
  ), tibble::tibble(
    val_vec = 1:5, .value = (1:5) * 2,
    .summary = c(FALSE, FALSE, TRUE, TRUE, TRUE)
  ))
})

test_that("selector works", {
  expect_equal(control(
    {
      val_vec * 2
    },
    val_vec = 1:5,
    .selector = ~ dplyr::filter(., .value > 7),
  ), tibble::tibble(val_vec = 4:5, .value = (4:5) * 2))
})

test_that("unnesting value works", {
  expect_equal(control(
    {
      list(val = val_vec, val2 = val_vec * 2)
    },
    val_vec = 1:5,
    .unnest_value = TRUE
  ), tibble::tibble(val_vec = 1:5, val = 1:5, val2 = (1:5) * 2))
})

test_that("unnesting summary works", {
  expect_equal(control(
    {
      val_vec * 2
    },
    val_vec = 1:5,
    .prober = ~ list(g4 = . > 4, l4 = . < 4),
    .unnest_summary = TRUE
  ), tibble::tibble(
    val_vec = 1:5, .value = (1:5) * 2, g4 = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    l4 = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  ))
})

test_that("variables outside of scope work", {
  expect_equal(
    {
      someval <- 2:5
      control(
        {
          val_vec * 2
        },
        val_vec = someval
      )
    },
    tibble::tibble(val_vec = 2:5, .value = (2:5) * 2)
  )
})

test_that("variables outside of scope work in expression", {
  expect_equal(
    {
      someval <- 2
      control(
        {
          val_vec * someval
        },
        val_vec = 1:5
      )
    },
    tibble::tibble(val_vec = 1:5, .value = (1:5) * 2)
  )
})

# TODO test that !! in expr works
