rm(list = ls())
library(testthat)

testvec <- c(12, 17, 19, 5, 9)
testdf <- tibble::tribble(
  ~x, ~y,
  1, 2,
  5, 9,
  12, 8
)

test_that("data building works", {
  expect_equal(
    testvec %$>% {
      val1 <- c(12, 2, 4)
    }, list(val1 = c(12, 2, 4))
  )
})

test_that("deeper named lists are not flattened", {
  expect_equal(
    testvec %$>% {
      val1 <- c(12, 2, 4)
      val2 <- list(a = 2, b = 3)
    }, list(val1 = c(12, 2, 4), val2 = list(a = 2, b = 3))
  )
})

test_that("dot pronoun works", {
  expect_equal(
    testvec %$>% {
      val1 <- max(.)
    }, list(val1 = 19)
  )
})

test_that("multiple assignments work", {
  expect_equal(
    testvec %$>% {
      val1 <- max(.)
      val2 <- min(.)
    }, list(val1 = 19, val2 = 5)
  )
})

test_that("lines without assignment are used as is", {
  expect_equal(
    testvec %$>% {
      val1 <- max(.)
      list(med = median(.), mean = mean(.))
      val2 <- min(.)
    }, list(val1 = 19, med = 12, mean = 12.4,  val2 = 5)
  )
})

test_that("assigning previous values works", {
  expect_equal(
    testvec %$>% {
      val1 <- max(.)
      val2 <- min(.) + val1
    }, list(val1 = 19, val2 = 24)
  )
})

test_that("data masking works", {
  expect_equal(
    testdf %$>% {
      val1 <- max(x)
      val2 <- min(y)
    }, list(val1 = 12, val2 = 2)
  )
})

test_that("local environment works", {
    someval <- 4
  expect_equal(
    testdf %$>% {
      val1 <- max(x)
      val2 <- min(y)
      insertedval <- someval
    }, list(val1 = 12, val2 = 2, insertedval = 4)
  )
})

# TODO doesn't check for names...
test_that("nested calls work", {
  expect_mapequal(
    testdf %$>% {
      dplyr::mutate(., x = x*2) %$>% {
        valx2 <- x
      }
      val1 <- max(x)
      val2 <- min(y)
    }, list(valx2 = c(2, 10, 24), val1 = 12, val2 = 2)
  )
})

# TODO error handling tests
