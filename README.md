# tyecon <img src='man/figures/logo.svg' align="right" height="139" />

A utility package presently aimed at making assembling functions and results easier and quicker.

To this aim, the `yeksar` function, with its special specification, is used to unify interfaces between various functions as desired.
The `%to%` operator allows one to return multiple results from the same object in a single dataframe row with columns being the names of each result instance.

The ultimate goal of the package is to act as an API, for model builders and package writers, to make their functions more easily accessible without having to do rewrites themselves.

## Example

Consider two functions with swapped arguments that we would want to use together without the hassle of writing multiple lines of code, storing the functions in lists, storing results in different variables etc. We just want to be able to change the "interface" with a simple argument:

```r
foo <- function(a1, b1) { a1/b1 }
bar <- function(b2, a2) { a2/b2 }
yeksared <- yeksar(..(..a = , ..b = ),
                   foo(a1 = ..a, b1 = ..b) ~ .,
                   bar(b2 = ..b, a2 = ..a) ~ .)
purrr::map_dfr(rlang::set_names(c("foo", "bar")),
              ~ yeksared(3,9, interface = .))
```

Which would return the result of each function as a dataframe column value. Storing the functions in lists or as anonymous functions would have quickly become rather verbose, especially when one needs to supply different arguments at different stages of a model-building script for instance. Consult `yeksar` examples and the vignettes for further information on the facilities of `yeksar`.

As for the `%to%` operator, it is more of a convenience function, omitting the process of having to build a named vector of directives, then pass it to something like `purrr::map` just to get the results of applying various functions to the same object. Nevertheless, this is often the need of any data analysis task. For instance, once a linear regression model is built, one might need to extract the various parts of the result of `summary(lm.model)`. The `%to%` operator allows this with concise syntax:

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

Which again, returns the results in a single dataframe row with column names being the name of each directive.

## Installation

The package still has a long way to go. Nevertheless, if you want to try it for yourself, here is the instruction:

```r
# install remotes package if not installed
install.packages("remotes")
# install tyecon using the remotes package
remotes::install_github("TerseTears/tyecon")
```

I am somewhat uncertain of the present limits of the package since there are many ways to write a function in R. Therefore, if you do write a function that doesn't behave well with `yeksar` please do open an issue.

To uninstall the package, run the usual `remove.packages` command.

## Main Idea

Use a single interface, text-based, to convert all argument calls to a unified method, essentially making all packages accessible from a unified interface. This is without the need to write separate APIs as package itself. Authors will be able to specify in a single file, how their functions map to the unified interface provided by `tyecon` or suggested by the community for instance.

The package's DSL should be able to unify all the below cases

- [x] functions having different names for same argument
- [x] functions lacking an argument
* [x] functions having extra arguments

## Pending Tasks

* [x] Need to add examples and documentation
* [x] Need to add tests somehow (tricky, since working with language constructs themselves)
* [ ] TODO QoL Need to make dsl nicer, remove unintuitive interfaces by wrapping functions (e.g. currently, need ..() in yeksar)
* [x] Better errors when things fail
* [x] There should be a way to compose expressions. That is, instead of having just one function that takes all functions to unify at the same time, the function can be applied individually and then each individual component can be be composed into the general function. This helps the API building part, since we'd want to let package developers add their own interfaces based on a standard, and then for the user to use all of them.
* [x] Still need a nice interface for post-processing of the result, so as to make the output the same as well. Pretty essential component...
* [x] Some sort of let block for R, where each line uses exactly the data, without needing to specify it.
* [x] Modify printing of `yeksar` functions as well and include list of functions and arguments or something.
* [ ] TODO Need to decide on how to read function argument transformations from a file and what file format (likely yaml) to use for this purpose.
* [ ] TODO Need to decide on coupling or decoupling of standard interfaces with the project. Preference being decoupling and keeping the package as general as possible.

## License 

MIT
