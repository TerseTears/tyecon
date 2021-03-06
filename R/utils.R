# helper function to get from dots only arguments prefixed with the interface
# name followed by a . character (e.g. basemeancalc.na.rm)
get_interface_args <- function(arguments, interface) {
  interface_args <- arguments[stringr::str_detect(
    names(arguments),
    paste0("^", interface, "\\.")
  )]
  names(interface_args) <- stringr::str_extract(
    names(interface_args),
    paste0("(?<=", interface, "\\.).*")
  )
  return(interface_args)
}

# helper function for dealing with unnamed arguments captured in expressions
# e.g. plot(x,y) that are meant to mean the actual x, y arguments of the
# function (not x, y variables).
set_call_args_names <- function(args) {
  new_names <- purrr::map2_chr(
    rlang::names2(args),
    rlang::names2(rlang::set_names(args)),
    ~ if (.x != "") .x else .y
  )
  new_args <- purrr::map2(rlang::names2(args), args, ~
    if (.x != "") .y else rlang::missing_arg())
  rlang::set_names(new_args, new_names)
}

# functions to get lhs and rhs of quosures with formula structure
f_lhs_quo <- function(quosure) {
  rlang::quo_set_expr(quosure, rlang::f_lhs(rlang::quo_get_expr(quosure)))
}
f_rhs_func <- function(quosure) {
  func <- rlang::as_function(rlang::new_formula(
    NULL,
    rlang::f_rhs(rlang::quo_get_expr(quosure))
  ))
  rlang::fn_env(func) <- rlang::quo_get_env(quosure)
  return(func)
}

pretty_func_args <- function(func, extra_args = NULL) {
  arg_names <- rlang::fn_fmls_names(func)
  arg_names <- c(arg_names[1:(length(arg_names) - 2)], extra_args)
  arg_vals <- rlang::fn_fmls(func)
  arg_vals <- c(arg_vals[1:(length(arg_vals) - 2)], "")
  arg_vals <- purrr::map_chr(arg_vals, ~ if (. == "") "" else paste(" =", .))
  paste0(arg_names, arg_vals)
}

# TODO %to% should also work with map and multiple data

# function to pass object to multiple lines of code and return results together

#' Pass object to multiple directives
#'
#' `%to%` allows a single object to be passed through multiple commands, and for
#' the results to be returned in a single dataframe.
#'
#' # Passing object to multiple instructions:
#' The syntax to use for this purpose is to have the object on the LHS of the
#' `%to%` operator, and wrap all instructions in a braces pair that effectively
#' acts as multiple lines of regular R code. However, each line needs to be
#' named, in a formula syntax fashion:
#' ```r
#' obj %to% {
#'  inst1 ~ inst1expressions(.)
#'  inst2 ~ inst2expressions(.)
#' }
#' ```
#' The right side of the formula is to be specified similar to how instructions
#' in `purrr:map` are, that is, by the `.`, `.x` syntax.
#'
#' If multiple lines are needed for each instruction, one can simply wrap them
#' in yet another braces pair.
#'
#' @param obj Object to be passed to multiple instructions
#' @param blocks formula-like instructions wrapped in a pair of braces `{}`.

#' @return Dataframe with column names being the formula LHS values, and a
#' single row containing the results of each column's respective instructions.
#'
#' @example examples/examples-to-operator.R
#'
#' @family result assemblers
#' @export
#' @rdname to-operator
`%to%` <- function(obj, blocks) {
  blocks <- rlang::enquo0(blocks)
  if (!isTRUE(all.equal(rlang::quo_get_expr(blocks)[1], rlang::expr({})))) {
    rlang::abort("expressions need to be in a code block")
  }
  if (!all(purrr::map_lgl(
    rlang::quo_get_expr(blocks)[-1],
    rlang::is_formula
  ))) {
    rlang::abort("expressions needs to be in formula form")
  }
  blocks <- purrr::map(rlang::quo_get_expr(blocks)[-1], ~
    rlang::new_quosure(., rlang::quo_get_env(blocks)))
  resnames <- purrr::map(blocks, ~ rlang::as_string(
    rlang::f_lhs(rlang::quo_get_expr(.))
  ))
  funcs <- purrr::map(blocks, f_rhs_func)
  reslist <- purrr::map(rlang::set_names(funcs, resnames), ~ .(obj))
  # make non-singleton elements a list to use in dataframe
  reslist <- purrr::map(reslist, ~ if (length(.) == 1) . else I(list(.)))

  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(reslist)
  } else {
    as.data.frame(reslist)
  }
}

#' Prettier Printing of object overview
#'
#' `str_pretty` is a wrapper around `str` that tries to use better, more
#' distinct identifiers in the object overview.
#'
#' @param x \[R `object`\] Any object.
#' @param ... \[named arguments\] Extra arguments to pass to `str`.
#'
#' @return Nothing. Only side effect being output to stdout. 
#'
#' @examples
#' str_pretty(lm(Sepal.Length ~ ., data = iris))
#'
#' nst_lst <- list()
#' nst_lst$a <- list(c(9,2,4, 12), g=c(2,4,9,7), h=list(m=4,n=7,d=12))
#' nst_lst$b <- c(4, 3, 9, 10, 3, 16, 1, 7)
#' nst_lst$f <- c("a", "b")
#' nst_lst$c <- 4
#' nst_lst$d <- c(7,9)
#' str_pretty(nst_lst)
#'
#' # Pass `str` arguments to modify behavior
#' str_pretty(lm(Sepal.Length ~ ., data = iris), give.attr = TRUE)
#'
#' @export
str_pretty <- function(x, ...) {
  arglist <- rlang::list2(...)
  if (!"give.attr" %in% names(arglist)) arglist$give.attr <- FALSE
  if (!"comp.str" %in% names(arglist)) arglist$comp.str <- "|- "
  out <- capture.output(rlang::inject(str(x, !!!arglist)))
  cat(
    stringr::str_replace_all(out, "(?<= ):", " = ") %!>%
      stringr::str_replace_all(":(?= )", " = ") %!>%
      stringr::str_replace_all("(List of )(\\d*)", " lst [\\2]") %!>%
      stringr::str_replace_all("\\$", " -") %!>%
      stringr::str_replace_all("(?<!\\.)\\.\\.(?!\\.)", ".") %!>%
      stringr::str_replace_all("- attr\\(\\*, \"(.*)\"\\)=", " attr: \\1 "),
    sep = "\n"
  )
}

#' Deconstruct data into multiple values
#'
#' `consign` pipe allows destructing data into multiple variables at once.
#' Internally `rlang::set_names` is used to name the RHS elements to current
#' scope.
#'
#' @family result assemblers
#'
#' @param vars \[R names\] Valid R names separated by colon.
#' @param data \[object\] R object of the same length as the number of `vars`
#' names.
#'
#' @return Nothing. Assigns LHS names to current scope.
#'
#' @rdname consign
#' @examples
#' val1 : val_2 : someval %<-% c(2, 4, 9)
#' c(val1, val_2, someval)
#'
#' x:y:z %<-% list(c(4,5,12), TRUE, list(a=5,b=9))
#' x; y; z
#'
#' @export
`%<-%` <- function(vars, data){
  vars <- rlang::enexpr(vars)
  vars <- stringr::str_trim(
    stringr::str_split(rlang::expr_text(vars), "\\:")[[1]])
  if(!all(make.names(vars) == vars)) {
    rlang::abort("Nonvalid names in %<-% assignment")
  }
  rlang::env_bind(rlang::caller_env(), !!!rlang::set_names(data, vars))
}
