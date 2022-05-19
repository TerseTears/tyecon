# use the pipe to build data in steps:
testdf <- tibble::tribble(
  ~x, ~y,
  3, 2,
  5, 9,
  12, 8
)
testdf %$>% {
  minx <- min(x)
  miny <- min(y)
  minxy <- min(minx, miny)
}

# use the dot pronoun to refer to the entire data
c(5, 9, 10) %$>% {
  min <- min(.)
  max <- max(.)
  whole <- .
}

# use curly braces for multiline instructions
testdf %$>% {
  minxy <- {
    minx <- min(x)
    miny <- min(y)
    min(minx, miny)
  }
  maxall <- max(.)
  minall <- min(.)
}

# Unnamed instructions can only be accessed by position
val <- testdf %$>% {
  min(x)
  xval <- x
  min(y)
}
val$xval
val[[1]]
val[[3]]
