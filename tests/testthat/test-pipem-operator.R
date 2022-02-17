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
            vec2 <- .
            (function(x) x+5)()
            (function(x) x*sum(vec2))()
        }, sum(testvec^2) * (testvec^2 + 5))
})

# This extra test is needed due to the lazy evaluation nature of R
test_that("standalone intermediary variable work", {
    expect_equal(
        testvec %->% {
            (function(x) x + 2)()
            testvec2 <- .
            (function(x) {testvec2})()
        }, testvec + 2)
})

# TODO test that intermediary variable has local scope
test_that("intermediary variables work", {
        retvalue <- testvec %->% {
            (function(x) x^2)()
            vec2 <- .
            (function(x) x+5)()
            (function(x) x*sum(vec2))()
        }
        expect_error(vec2, "not found")
        testvec %->% {
            (function(x) x^2)()
            vec3 <- .
            (function(x) x+5)()
            (function(x) vec3)()
        }
        expect_error(vec3, "not found")
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
            mydf <- .
            dplyr::mutate(x6 = (mydf$x2)^3)
        }, testdf %>% dplyr::mutate(x2 = x^2) %>% dplyr::mutate(x6 = x2^3))
})

test_that("automatic column subsetting works", {
    expect_equal(
        testdf %->% {
            dplyr::mutate(x2 = x^2)
            myx2col <- x2
            dplyr::mutate(x6 = (myx2col)^3)
        }, testdf %>% dplyr::mutate(x2 = x^2) %>% dplyr::mutate(x6 = x2^3))
})

test_that("verbose column subsetting works", {
    expect_equal(
        testdf %->% {
            dplyr::mutate(x2 = x^2)
            myx2col <- .$x2
            dplyr::mutate(x6 = (myx2col)^3)
        }, testdf %>% dplyr::mutate(x2 = x^2) %>% dplyr::mutate(x6 = x2^3))
})

test_that("multiline assignment works", {
    expect_equal(
        testdf %->% {
            dplyr::mutate(x2 = x^2)
            myx2col <- {
                smth <- x2
                smthelse <- x2 * x
                smthelse / x
            }
            dplyr::mutate(x6 = (myx2col)^3)
        }, testdf %>% dplyr::mutate(x2 = x^2) %>% dplyr::mutate(x6 = x2^3))
})

test_that("piping from previous statements works", {
    expect_equal(
    testdf %>% dplyr::mutate(x2 = x^2) %->% {
        dplyr::mutate(x6 = x2^3)
    }, testdf %>% dplyr::mutate(x2 = x^2) %>% dplyr::mutate(x6 = x2^3))
})

# TODO write tests to check good error handling
