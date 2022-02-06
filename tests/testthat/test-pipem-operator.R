rm(list=ls())
library(testthat)

testvec <- c(12, 17, 19, 5, 9)
testdf <- tibble::tribble(~x, ~y,
                  1, 2,
                  5, 9,
                  12, 8)

test_that("simple piping works", {
    expect_equal(
        testvec %->% {
            (function(x) x^2)()
        }, testvec^2)
})


test_that("multiline piping works", {
    expect_equal(
        testvec %->% {
            (function(x) x^2)()
            sum()
        }, sum(testvec^2))
})

test_that("intermediary variables work", {
    expect_equal(
        testvec %->% {
            (function(x) x^2)()
            vec2
            (function(x) x+5)()
            (function(x) x*sum(vec2))()
        }, sum(testvec^2) * (testvec^2 + 5))
})

test_that("data masks work", {
    expect_equal(
        testdf %->% {
            dplyr::mutate(x2 = x^2)
            dplyr::mutate(x6 = x2^3)
        }, testdf %>% dplyr::mutate(x2 = x^2) %>% dplyr::mutate(x6 = x2^3))
})

test_that("intermediary variables work in quasiquoting context", {
    expect_equal(
        testdf %->% {
            dplyr::mutate(x2 = x^2)
            mydf
            dplyr::mutate(x6 = (mydf$x2)^3)
        }, testdf %>% dplyr::mutate(x2 = x^2) %>% dplyr::mutate(x6 = x2^3))
})

test_that("piping from previous statements works", {
    expect_equal(
    testdf %>% dplyr::mutate(x2 = x^2) %->% {
        dplyr::mutate(x6 = x2^3)
    }, testdf %>% dplyr::mutate(x2 = x^2) %>% dplyr::mutate(x6 = x2^3))
})

# TODO write tests to check good error handling
