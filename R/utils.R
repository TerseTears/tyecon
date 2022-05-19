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
#' ```
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
