rm(list=ls())
library(tidyverse)
library(rlang)
library(testthat)
source("../../R/dsl.R")

test_that("functions with differently named arguments work", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              yeksared <- yeksar(..(..a = , ..b = ),
                                 bar(b2 = ..b, a2 = ..a) ~ .) +
                                 (foo(a1 = ..a, b1 = ..b) ~ .)
              expect_equal(yeksared(9,5,interface="foo"), bar(5,9))
})

test_that("functions with same argument names work", {
              foo <- function(a, b) { a/b }
              bar <- function(b, a) { a/b }
              yeksared <- yeksar(..(a = , b = ), bar(b = b, a = a) ~ .) +
                  (foo(a = a, b = b) ~ .)
              expect_equal(yeksared(9,5,interface="foo"), bar(5,9))
})

test_that("interface switching works", {
              foo <- function(a1, b1) { a1/b1 }
              # multiply by 2 so that foo gives smaller values
              bar <- function(b2, a2) { a2/b2*2 }
              yeksared <- yeksar(..(..a = , ..b = ),
                                 foo(a1 = ..a, b1 = ..b) ~ .) +
                                 (bar(b2 = ..b, a2 = ..a) ~ .)
              expect_lt(yeksared(9,5), bar(5,9))
              expect_lt(yeksared(9,5, interface="foo"), bar(5,9))
              expect_equal(yeksared(9,5, interface="bar"), bar(5,9))
})

test_that("functions with extra essential arguments work", {
              foo <- function(a1, b1, message) {
                  print(message)
                  a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              message <- "hello friend"
              yeksared <- yeksar(..(..a = , ..b = ),
                                 bar(b2 = ..b, a2 = ..a) ~ .) +
                                 (foo(a1 = ..a, b1 = ..b, message=message) ~ .)
              expect_equal(yeksared(9,5,interface="foo"), bar(5,9))
              # alternatively, supply with function itself:
              message <- "hello again"
              yeksared <- yeksar(..(..a = , ..b = ),
                                 bar(b2 = ..b, a2 = ..a) ~ .) +
                                 (foo(a1 = ..a, b1 = ..b) ~ .)
              expect_equal(yeksared(9,5,interface="foo",foo.message=message),
                           bar(5,9))
})

test_that("functions with optionally supplied arguments work", {
              foo <- function(a1, b1, round=FALSE) {
                  if(round) round(a1/b1) else a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              yeksared <- yeksar(..(..a = , ..b = ),
                                 bar(b2 = ..b, a2 = ..a) ~ .) +
                                 (foo(a1 = ..a, b1 = ..b) ~ .)
              expect_equal(yeksared(9,5,interface="foo", foo.round=FALSE),
                           bar(5,9))
              expect_equal(yeksared(9,5,interface="foo", foo.round=TRUE),
                                    round(bar(5,9)))
})

test_that("more than two functions work", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              baz <- function(a3, b3) { a3/b3 }
              yeksared <- yeksar(..(..a = , ..b = ),
                                 foo(a1 = ..a, b1 = ..b) ~ .) +
                                 (bar(b2 = ..b, a2 = ..a) ~ .) +
                                 (baz(a3 = ..a, b3 = ..b) ~ .)
              expect_equal(yeksared(9,5, interface="foo"), bar(5,9))
              expect_equal(yeksared(9,5, interface="baz"), bar(5,9))
              expect_equal(yeksared(9,5, interface="bar"), bar(5,9))
})

test_that("optional argument with no function prefix is ignored", {
              foo <- function(a1, b1, terminate=FALSE) {
                  if (terminate) return(NULL) else a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              yeksared <- yeksar(..(..a = , ..b = ),
                                 bar(b2 = ..b, a2 = ..a) ~ .) +
                                 (foo(a1 = ..a, b1 = ..b) ~ .)
              expect_equal(yeksared(9,5,interface="foo",terminate=TRUE),
                           bar(5,9))
              expect_null(yeksared(9,5,interface="foo",foo.terminate=TRUE))
})

test_that("quoting function works", {
              testdf <- tribble(~x, ~y,
                                1, 5,
                                4, 3)
              foo <- function(df, col) {
                  col <- as_string(ensym(col))
                  sum(df[[col]]) }
              bar <- function(df, colstring) { sum(df[[colstring]]) }
              yeksared <- yeksar(..(..df = , ..col = ),
                                 bar(df = ..df, col = ..colstring) ~ .) +
                                 (foo(df = ..df, col = !!..col) ~ .)
              expect_equal(yeksared(testdf, "y",interface="foo"),
                           bar(testdf, "y"))
})

test_that("parameters in post-processing functions work", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              multfactor <- 2
              yeksared <- yeksar(..(..a = , ..b = ), 
                                 bar(b2 = ..b, a2 = ..a) ~ .*multfactor) +
                                 (foo(a1 = ..a, b1 = ..b) ~ .*multfactor)
              expect_equal(yeksared(9,5,interface="foo"), bar(5,9)*multfactor)
})

test_that("supplied default arguments work", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              bval <- 2
              yeksared <- yeksar(..(..a = 3, ..b = bval),
                                 bar(b2 = bval, a2 = ..a) ~ .) +
                                 (foo(a1 = ..a, b1 = 2) ~ .)
              expect_equal(yeksared(9,4), yeksared(9,7,interface="bar"))
              expect_equal(yeksared(interface="foo"), bar(bval, 3))
})

# TODO testing for conditions to throw errors

test_that("unknown optional argument is printed nicely", {
              foo <- function(a1, b1, simplify=TRUE) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              yeksared <- yeksar(..(..a = , ..b = ),
                                 bar(b2 = ..b, a2 = ..a) ~ .) +
                                 (foo(a1 = ..a, b1 = ..b) ~ .)
              expect_error(yeksared(9,5,interface="foo",foo.smplfy=TRUE),
                           "unused argument")
              expect_equal(yeksared(9,5,interface="foo",foo.simplify=TRUE),
                           bar(5,9))
})

test_that("supplying other than two-sided formulas is error", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              # TODO bellow doesn't go into the not a formula error because
              # the expression foo(...) is evaluated first and there seems to
              # be no way for me to delay its evaluation for some reason
              expect_error(yeksar(..(..a = , ..b = ),
                                 bar(b2 = ..b, a2 = ..a) ~ .) +
                                 (foo(a1 = ..a, b1 = ..b)), "not found")
})
