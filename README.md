# tyecon

A utility package presently aimed at making assembling functions and results easier and quicker.

To this aim, the `yeksar` function, with its special specification, is used to unify interfaces between various functions as desired.
The `%to%` operator allows one to return multiple results from the same object in a single dataframe row with columns being the names of each result instance.

The ultimate goal of the package is to act as an API, for model builders and package writers, to make their functions more easily accessible without having to do rewrites themselves.

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
