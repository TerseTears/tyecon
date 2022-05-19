# use piping context instead of sequentially using the `magrittr` pipe:
testdf <- tibble::tribble(
  ~x, ~y,
  1, 2,
  5, 9,
  12, 8
)
mydf <- testdf %->% {
  dplyr::mutate(x2 = x^2)
  dplyr::mutate(x6 = x2^3)
}
all.equal(
  mydf,
  testdf %>% dplyr::mutate(x2 = x^2) %>% dplyr::mutate(x6 = x2^3)
)


# bind intermediate values for later use
mydf2 <- testdf %->% {
  dplyr::mutate(x2 = x^2)
  somedf <- .
  dplyr::mutate(x6 = (somedf$x2)^3)
}
all.equal(mydf, mydf2)
## intermediate values do not remain after the pipe is done
as.character(tryCatch(somedf, error = function(e) e))

# automatic data masking in assignment
mydf <- testdf %->% {
  dplyr::mutate(x2 = x^2)
  colx2xy <- x2 + x + y
  dplyr::mutate(x2xyval = colx2xy)
  dplyr::mutate(x2xy = x2 + x + y)
}
all.equal(mydf$x2xyval, mydf$x2xy)
