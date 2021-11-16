# tyecon

A package to act as the API, for model builders and package writers, to make their functions more easily accessible without having to do rewrites. 

## Main Idea

Use a single interface, text-based, to convert all argument calls to a unified method, essentially making all packages accessible from a unified interface. This is without the need to write separate APIs as a package itself. Authors will be able to specify in a simple DSL, how their functions map to the unified interface provided by `tyecon`. 

The package's dsl should be able to unify all the below cases

- [x] functions having different names for same argument
- [x] functions lacking an argument
* [x] functions having extra arguments

## Pending Tasks

* [ ] TODO Need to add examples and documentation
* [x] Need to add tests somehow (tricky, since working with language constructs themselves)
* [ ] TODO QoL Need to make dsl nicer, remove unintuitive interfaces by wrapping functions (e.g. currently, need ..() in yeksar)
* [ ] Better errors when things fail
* [ ] TODO QoL There should be a way to compose expressions. That is, instead of having just one function that takes all functions to unify at the same time, the function can be applied
  individually and then each individual component can be be composed into the general function. This helps the API building part, since we'd want to let package developers add their own interfaces based on a standard, and then for the user to use all of them.
* [x] Still need a nice interface for post-processing of the result, so as to make the output the same as well. Pretty essential component...
* [ ] Some sort of let block for R, where each line uses exactly the data, without needing to specify it.
* [ ] Modify printing of yeksar functions as well and include list of functions and arguments or something.
