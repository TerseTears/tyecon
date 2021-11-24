rm(list=ls())
library(testthat)

test_that("functions with differently named arguments work", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              convoked <- convoke(\(a, b) "",
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .)
              expect_equal(convoked(9,5), bar(5,9))
})

test_that("functions with same argument names work", {
              foo <- function(a, b) { a/b }
              bar <- function(b, a) { a/b }
              convoked <- convoke(\(a, b) "",
                                 foo(a = a, b = b) ~ .,
                                 bar(b = b, a = a) ~ .)
              expect_equal(convoked(9,5), bar(5,9))
})

test_that("interface switching works", {
              foo <- function(a1, b1) { a1/b1 }
              # multiply by 2 so that foo gives smaller values
              bar <- function(b2, a2) { a2/b2*2 }
              convoked <- convoke(\(a, b) "",
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .)
              expect_lt(convoked(9,5), bar(5,9))
              expect_lt(convoked(9,5, interface="foo"), bar(5,9))
              expect_equal(convoked(9,5, interface="bar"), bar(5,9))
})

test_that("functions with extra essential arguments work", {
              foo <- function(a1, b1, message) {
                  print(message)
                  a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              message <- "hello friend"
              convoked <- convoke(\(a, b) "",
                                 foo(a1 = a, b1 = b, message=message) ~ .,
                                 bar(b2 = b, a2 = a) ~ .)
              expect_equal(convoked(9,5), bar(5,9))
              # alternatively, supply with function itself:
              message <- "hello again"
              convoked <- convoke(\(a, b) "",
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .)
              expect_equal(convoked(9,5,foo.message=message), bar(5,9))
})

test_that("functions with optionally supplied arguments work", {
              foo <- function(a1, b1, round=FALSE) {
                  if(round) round(a1/b1) else a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              convoked <- convoke(\(a, b) "",
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .)
              expect_equal(convoked(9,5, foo.round=FALSE), bar(5,9))
              expect_equal(convoked(9,5, foo.round=TRUE), round(bar(5,9)))
})

test_that("more than two functions work", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              baz <- function(a3, b3) { a3/b3 }
              convoked <- convoke(\(a, b) "",
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .,
                                 baz(a3 = a, b3 = b) ~ .)
              expect_equal(convoked(9,5, interface="foo"), bar(5,9))
              expect_equal(convoked(9,5, interface="baz"), bar(5,9))
              expect_equal(convoked(9,5, interface="bar"), bar(5,9))
  expect_equal(2 * 2, 4)
})

test_that("optional argument with no function prefix is ignored", {
              foo <- function(a1, b1, terminate=FALSE) {
                  if (terminate) return(NULL) else a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              convoked <- convoke(\(a, b) "",
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .)
              expect_equal(convoked(9,5, terminate=TRUE), bar(5,9))
              expect_null(convoked(9,5, foo.terminate=TRUE))
})

test_that("quoting function works", {
              testdf <- tibble::tribble(~x, ~y,
                                1, 5,
                                4, 3)
              foo <- function(df, col) {
                  col <- rlang::as_string(rlang::ensym(col))
                  sum(df[[col]]) }
              bar <- function(df, colstring) { sum(df[[colstring]]) }
              convoked <- convoke(\(df, col) "",
                                 foo(df = df, col = !!col) ~ .,
                                 bar(df = df, col = colstring) ~ .)
              expect_equal(convoked(testdf, "y"), bar(testdf, "y"))
})

test_that("parameters in post-processing functions work", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              multfactor <- 2
              convoked <- convoke(\(a, b) "", 
                                 foo(a1 = a, b1 = b) ~ .*multfactor,
                                 bar(b2 = b, a2 = a) ~ .*multfactor)
              expect_equal(convoked(9,5), bar(5,9)*multfactor)
              multfunc <- function(x) x*2
              convoked <- convoke(\(a, b) "", 
                                 foo(a1 = a, b1 = b) ~ multfunc(.),
                                 bar(b2 = b, a2 = a) ~ multfunc(.))
              expect_equal(convoked(9,5), bar(5,9)*multfactor)
})

test_that("supplied default arguments work", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              bval <- 2
              convoked <- convoke(\(a = 3, b = bval) "",
                                 foo(a1 = a, b1 = 2) ~ .,
                                 bar(b2 = bval, a2 = a) ~ .)
              expect_equal(convoked(9,4), convoked(9,7,interface="bar"))
              expect_equal(convoked(), bar(bval, 3))
})

# TODO testing for conditions to throw errors

test_that("unknown optional argument is printed nicely", {
              foo <- function(a1, b1, simplify=TRUE) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              convoked <- convoke(\(a, b) "",
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .)
              expect_error(convoked(9,5,foo.smplfy=TRUE), "unused argument")
              expect_equal(convoked(9,5,foo.simplify=TRUE), bar(5,9))
})

test_that("supplying other than two-sided formulas is error", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              expect_error(convoke(\(a, b) "",
                                 foo(a1 = a, b1 = b),
                                 bar(b2 = b, a2 = a) ~ .), "formula")
})

test_that("supplying unifying interface other than function(a,b) is error", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              expect_error(convoke(foobar(a, b),
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .), "call")
              expect_error(convoke({a; b},
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .), "call")
})

test_that("unified interface body other than string is error", {
              foo <- function(a1, b1) { a1/b1 }
              bar <- function(b2, a2) { a2/b2 }
              expect_error(convoke(\(a, b) myfunc,
                                 foo(a1 = a, b1 = b) ~ .,
                                 bar(b2 = b, a2 = a) ~ .), "string")
})

