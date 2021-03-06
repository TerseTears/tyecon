rm(list = ls())
library(testthat)

testvec <- c(1, 2, 3, 7)
testvecNA <- c(1, 2, 3, NA)
testdf <- tibble::tribble(
  ~x, ~y,
  1, 2,
  5, 9,
  12, 8
)

test_that("single line works", {
  expect_equal(testvec %to% {
    mean ~ mean(.)
  }, tibble::tibble(mean = mean(testvec)))
})

test_that("multiple lines work", {
  expect_equal(testvec %to% {
    mean ~ mean(.)
    sd ~ sd(.)
  }, tibble::tibble(mean = mean(testvec), sd = sd(testvec)))
})

test_that("arguments for functions work", {
  expect_equal(testvecNA %to% {
    mean ~ mean(., na.rm = T)
    sd ~ sd(.)
  }, tibble::tibble(
    mean = mean(testvecNA, na.rm = T),
    sd = sd(testvecNA)
  ))
})

test_that("passing arguments works", {
  naval <- TRUE
  expect_equal(testvecNA %to% {
    mean ~ mean(., na.rm = naval)
    sd ~ sd(.)
  }, tibble::tibble(
    mean = mean(testvecNA, na.rm = T),
    sd = sd(testvecNA)
  ))
  func_mean <- mean
  expect_equal(testvec %to% {
    mean ~ func_mean(.)
    sd ~ sd(.)
  }, tibble::tibble(mean = mean(testvec), sd = sd(testvec)))
})

test_that("data frame indexing works", {
  expect_equal(testdf %to% {
    xcolmean ~ mean(.$x)
    ycolsd ~ sd(.$y)
  }, tibble::tibble(xcolmean = mean(testdf$x), ycolsd = sd(testdf$y)))
})

test_that("nested blocks work", {
  expect_equal(testvec %to% {
    mean2 ~ {
      x <- .
      mean(x^2)
    }
    sd ~ sd(.)
  }, tibble::tibble(mean2 = mean(testvec^2), sd = sd(testvec)))
})

test_that("expression other than block is error", {
  expect_error(testvec %to% (mean ~ mean(.)), "block")
})

test_that("specifying other than formula is error", {
  expect_error(testvec %to% {
    mean(.)
  }, "formula")
})
