# unifying functions with swapped arguments
foo <- function(a1, b1) {
  a1 / b1
}
bar <- function(b2, a2) {
  a2 / b2
}
convoked <- convoke(
  list(a, b),
  foo(a1 = a, b1 = b),
  bar(b2 = b, a2 = a)
)
c(convoked(9, 5), bar(5, 9))
c(convoked(9, 5, interface = "bar"), foo(9, 5))

# supplying optional arguments
foo <- function(a1, b1, round = FALSE) {
  if (round) round(a1 / b1) else a1 / b1
}
bar <- function(b2, a2) {
  a2 / b2
}
convoked <- convoke(
  list(a, b),
  foo(a1 = a, b1 = b),
  bar(b2 = b, a2 = a)
)
c(convoked(9, 5, foo.round = FALSE), bar(5, 9))
c(convoked(9, 5, foo.round = TRUE), round(bar(5, 9)))

# TODO fix and show this for three sequences
# adding further functions later
foo <- function(a1, b1) {
  a1 / b1
}
bar <- function(b2, a2) {
  a2 / b2
}
(convoked <- convoke(list(a, b), foo(a1 = a, b1 = b)))
(convoked <- convoked + ~ bar(b2 = b, a2 = a))
c(convoked(9, 5, interface = "bar"), foo(5, 9))
