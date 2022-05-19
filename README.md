# tyecon <img src='man/figures/logo.svg' align="right" height="139" />

A utility package presently aimed at making assembling functions and results
easier and quicker.

The package's macro-like functions unify below cases:

* functions having different names for same argument
* functions lacking an argument
* functions having extra arguments

## Main Idea

The ultimate goal of the package is to act as an API, for model builders and
package writers, to make their functions accessible without having
to do rewrites themselves.

This could be achieved by using a single interface, text-based, to convert all
argument calls to a unified method, essentially making all packages accessible
from a unified interface. This is without the need to write separate APIs as
package itself. Authors will be able to specify in a single file (or via
annotations of source code), how their functions map to the unified interface
provided by `tyecon` or suggested by the community for instance.

## Examples

### `convoke`

Consider two functions with swapped arguments that we would want to use together
without the hassle of writing extraneous lines of code, storing the functions in
lists, storing results in different variables etc. We want to be able to change
the "interface" with a simple argument:

```r
foo <- function(a1, b1) { a1/b1 }
bar <- function(b2, a2) { a2/b2 }
convoked <- convoke(list(a, b),
                   foo(a1 = a, b1 = b),
                   bar(b2 = b, a2 = a))
purrr::map_dfr(rlang::set_names(c("foo", "bar")),
              ~ convoked(3,9, interface = .))
```

Which would return the result of each function as a dataframe column value.
Storing the functions in lists or as anonymous functions would have become
rather verbose, for instance when one needs to supply different arguments at
different stages of a model-building script. Consult `convoke` examples and the
vignettes for further information on the facilities of `convoke`.

### `conflate`

Another issue is when one tries to use dissimilar generic methods together, yet
each method comes with peripheral arguments. The `conflate` function allows one
to supply all said arguments in one place (useful in `purrr::map`,
`dplyr::mutate`, etc.):

```r
glm.model <- glm(Sepal.Length ~ Sepal.Width, data = iris)
lm.model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
(conflated_summary <- conflate(summary(x, correlation = TRUE)))

purrr::map(list(glm.model, lm.model), ~ 
           conflated_summary(., lm.symbolic.cor = TRUE))
```

### `%->%` pipem operator

With this operator, instructions are chained while also keeping any of the
intermediate values. The syntax also omits the recurrent extraneous `magrittr`
pipes at the end:

```r
testdf <- tibble::tribble(~x, ~y,
                  1, 2,
                  5, 9,
                  12, 8)
testdf %->% {
    dplyr::mutate(x2 = x^2)
    dplyr::mutate(x3 = x^3)
    colx2 <- x2
    dplyr::mutate(x4 = x2^2)
    dplyr::mutate(x6 = (colx2)^3)
}
```

### %$>% construct operator

This pipe facilitates transforming data into a new structure with concise 
syntax:

```r
testdf <- tibble::tribble(~x, ~y,
                  1, 2,
                  5, 9,
                  12, 8)
testdf %$>% {
  minx <- min(x)
  miny <- min(y)
}
```

### `%to%` operator

The `%to%` operator, is a convenience function, omitting the process of having
to build a named vector of directives, then pass it to something like
`purrr::map` to get the results of applying different functions to the same
object. Even so, this is often the need of any data analysis task. For instance,
once a linear regression model is built, one might need to extract the diverse
parts of the result of `summary(lm.model)`. The `%to%` operator allows this with
concise syntax:

```r
lm.model <- lm(Sepal.Width ~ Sepal.Length, data = iris)
summary(lm.model) %to% {
    res.std.err ~ .$sigma
    dfs ~ .$df[2]
    pval ~ coef(.)[,"Pr(>|t|)"]
    fstat ~ .$fstatistic[1]
    terms ~ .$terms
}
```

Which again, returns the results in a single dataframe row with column names
being the name of each directive.

## Installation

The package still has a long way to go. If you want to try it for yourself, here
is the instruction:

```r
# install remotes package if not installed
install.packages("remotes")
# install tyecon using the remotes package
remotes::install_github("TerseTears/tyecon")
```

I am somewhat uncertain of the present limits of the package since there are
various ways to write a function in R. Consequently, if you do write a function
that doesn't behave well with the package, please do open an issue.

To uninstall the package, run the usual `remove.packages` command.

## Name

The idea of the package was originally to straighten out tasks common to
econometrics, hence "**t**id**y** **econ**ometrics"" (other abbreviations didn't
look as good). Still, I realized early on that the problems where common with
general data analysis tasks, yet the name remained. Now, it can be considered
"tidy *con*" functions, functions that simplify joint operations with conciser
statements.

## Pending Tasks

* [ ] Better error handling
* [ ] Deciding on an import method or format
* [ ] Writing tests for `conflate`
* [ ] (QoL) allowing supplying custom arguments with `c()`, e.g. `stan_glm
  = c(chains=5, iter=500)`.

## License

MIT
