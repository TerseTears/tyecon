# unifying functions with swapped arguments
foo <- function(a1, b1) { a1/b1 }
bar <- function(b2, a2) { a2/b2 }
convoked <- convoke(list(a, b),
                   foo(a1 = a, b1 = b) ~ .,
                   bar(b2 = b, a2 = a) ~ .)
c(convoked(9,5), bar(5,9))
c(convoked(9,5,interface="bar"), foo(9,5))

# supplying optional arguments
foo <- function(a1, b1, round=FALSE) { if(round) round(a1/b1) else a1/b1 }
bar <- function(b2, a2) { a2/b2 }
convoked <- convoke(list(a, b),
                   foo(a1 = a, b1 = b) ~ .,
                   bar(b2 = b, a2 = a) ~ .)
c(convoked(9,5, foo.round=FALSE), bar(5,9))
c(convoked(9,5, foo.round=TRUE), round(bar(5,9)))

# post-processing specification
foo <- function(a1, b1) { a1/b1 }
bar <- function(b2, a2) { a2/b2 }
multfactor <- 2
convoked <- convoke(list(a, b), 
                   foo(a1 = a, b1 = b) ~ .*multfactor,
                   bar(b2 = b, a2 = a) ~ .*multfactor)
c(convoked(9,5), bar(5,9)*multfactor)
multfunc <- function(x) x*2
convoked <- convoke(list(a, b), 
                   foo(a1 = a, b1 = b) ~ multfunc(.),
                   bar(b2 = b, a2 = a) ~ multfunc(.))
c(convoked(9,5), bar(5,9)*multfactor)

# adding further functions later
foo <- function(a1, b1) { a1/b1 }
bar <- function(b2, a2) { a2/b2 }
baz <- function(a3, b3) { a3/b3 }
(convoked <- convoke(list(a, b), foo(a1 = a, b1 = b) ~ .))
(convoked <- convoked + (bar(b2 = b, a2 = a) ~ .) +
    (baz(a3 = a, b3 = b) ~ .))
c(convoked(9,5, interface="baz"), bar(5,9))
