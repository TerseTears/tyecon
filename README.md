# tyecon <img src='man/figures/logo.svg' align="right" height="139" />

A package to make assembling functions and results easier and quicker.

## Main Idea

The `tyecon` package is a set of macros to allow conciser code for using
functions and objects from different packages together. 

There are essentially two existing approaches to dealing with differences in
package interfaces: 1. Employing a sort of object-oriented approach with the use
of generic functions and 2. Writing meta packages that unify the interfaces. The
`tyecon` package proposes a data/code alternative: Utilizing meta functions with
a simple DSL so that the user can take care of such tasks on their own. This
allows greater flexibility yet avoids the disparity that comes with the
differing shapes of package models, plots, etc. while also eliminating the need
for meta-packages (or simplifying writing them).

## The Macros

### `convoke`

Unifies function interfaces.

Consider two functions with swapped arguments that we would want to use together
without extra hassle. Then we would want to be able to simply change the
"interface" with a single argument:

```r
foo <- function(a1, b1) {
  a1 / b1
}
bar <- function(b2, a2) {
  a2 / b2
}
foobar <- convoke(
  # First argument is the list of shared arguments.
  list(a, b),
  # Following arguments are the translation of the 
  # shared arguments to the # function interfaces.
  foo(a1 = a, b1 = b),
  bar(b2 = b, a2 = a)
)
# This allows using the two functions in a map with minimal hassle.
purrr::map_dfr(
  rlang::set_names(c("foo", "bar")),
  ~ foobar(3, 9, interface = .)
)
```

### `conflate`

Unifies generic function interfaces.

Another issue is when one tries to use dissimilar generic methods together, yet
each method comes with peripheral arguments. The `conflate` function allows one
to supply all such arguments together:

```r
lm_model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
glm_model <- glm(Sepal.Length ~ Sepal.Width, data = iris)

summary_lm_glm <- conflate(summary(x, correlation = TRUE))

purrr::map(
  list(lm_model, glm_model),
  ~ summary_lm_glm(., lm.symbolic.cor = TRUE)
)
```

### `%->%` convey operator

Passes data created at each step to the next function.

Transforming data can require passing it consecutively to multiple functions.
Nevertheless, some intermediate results may need to be kept for later stages.
This is a single context and thus requires its own macro:

```r
df <- tibble::tribble(
  ~x, ~y,
  1, 2,
  5, 9,
  12, 8
)
# Apply multiple functions to the data without specifying extraneous pipes.
df %->% {
  dplyr::mutate(x2 = x^2)
  dplyr::mutate(x3 = x^3)
  # Keep intermediate values in the context of the data for use at later stages.
  colx2 <- x2
  dplyr::mutate(x4 = x2^2)
  dplyr::mutate(x6 = (colx2)^3)
}
```

### `%$>%` construct operator

This pipe facilitates transforming data into a new structure with concise 
syntax while also enforcing a "bottom-up" approach:

```r
df <- tibble::tribble(
  ~x, ~y,
  1, 2,
  5, 9,
  12, 8
)
df %$>% {
  # Each assignment is kept as the named element of a final list
  minx <- min(x)
  miny <- min(y)
  # Assignments are in context and thus can be used for the construction
  # of later elements.
  minxy <- min(minx, miny)
}
```

### `%<-%` consign operator

R does not support destructing data natively. The consign operator offers that
possibility. Simply separating the variable names to be assigned by a colon:

```r
x : y : z %<-% list(c(9,12), 7, "hello")
x; y; z
```


### Other macros to come

I'm experimenting with my R workflow trying to optimize the process of mingling
different parts as concisely and thus more macros are to come. If you
have any suggestions please do open an issue.

## Installation

The package still has a long way to go. If you want to try it for yourself, here
is the instruction:

```r
# Install remotes package if not installed.
install.packages("remotes")
# Install tyecon using the remotes package.
remotes::install_github("TerseTears/tyecon")
```

To uninstall the package, run the usual `remove.packages` command.

## Name

The idea of the package was originally to straighten out tasks common to
econometrics, hence "**t**id**y** **econ**ometrics"" (other abbreviations didn't
look as good). Still, I realized early on that the problems where common with
general data analysis tasks, yet the name remained. Now, it can be considered
"tidy *con*" functions, functions that simplify joint operations with conciser
statements.

## Pending Tasks

- [ ] Better error handling
- [ ] Writing a vignette explaining possible uses in package development
- [ ] Writing tests for `conflate`
- [ ] (QoL) allowing supplying custom arguments with `c()`, e.g. `stan_glm
  = c(chains=5, iter=500)`.
- [ ] Add assignment of multiple elements to `construct` at once (using map for
  instance).

## License

MIT
