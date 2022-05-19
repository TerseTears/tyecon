#' Unify function interfaces
#'
#' `convoke` unifies functions along a single specification using
#' statements on function argument transformations that would be needed. The
#' result is a single function whose *interface* can be changed simply by
#' changing the respective argument in the generated function.
#'
#' # Unifying Functions
#'
#' ## Specifying the unifying interface
#'
#' The unifying interface needs to be specified as arglist for the new function
#'
#' ```
#' list(arg1 = default1, arg2 = default2, etc.)
#' ```
#'
#' ## Specifying function argument transformations
#'
#' Transformations follows the function specification format
#' ```
#' func1(func1arg1 = transformed_arg1, func1arg2 = transformed_arg2)
#' ```
#' Essentially, pass the function as if it is to take
#' arguments from the unified function.
#'
#' # Composing functions
#'
#' It is also possible to progressively add functions to a convoke function
#' simply by adding to the convoke function the new specifications:
#' ```
#' convoke_func + ~func_spec
#' ```
#'
#' @family function assemblers
#' @example examples/examples-convoke.R
#'
#' @param argslist \[`list`\] The desired unifying arguments. A list call.
#'
#' @param ... \[`function`\] All the various specifications for function argument
#' transformations.
#'
#' @return Function with additional class *convoke* with arguments being
#' ```
#' convoke_func(specified_args, interface, ..., evaluate = TRUE)
#' ```
#' The `evaluate` argument is useful for debugging purposes. Extra arguments can
#' be passed in the format `interface.arg`.
#'
#' @export
convoke <- function(argslist, ...) {
  func_specs <- rlang::enquos0(...)
  # TODO possibly would benefit from also capturing environment
  argslist <- rlang::enexpr(argslist)
  if (!all(purrr::map_lgl(
    func_specs,
    ~ rlang::is_call_simple(rlang::quo_get_expr(.))
  ))) {
    rlang::abort("specifications need to be function calls")
  }
  if (!rlang::is_call(argslist, "list")) {
    rlang::abort("unified interface specification not a list call")
  }
  func_envs <- rlang::quo_get_env(func_specs[[1]])
  # functions to convoke start from second argument
  func_names <- purrr::map(func_specs, rlang::call_name)
  # transformation instructions are directly given as named arguments for
  # each function
  func_args_transforms <- purrr::map(func_specs, rlang::call_args)
  # name components to access them with the interface argument
  names(func_args_transforms) <- func_names
  # add function args as ones specified as first argument, plus an interface
  # argument, and whether to return produced function (for debugging) or
  # evaluate in place
  convoke_func_args <- c(
    set_call_args_names(rlang::call_args(argslist)),
    list(interface = func_names[[1]]),
    rlang::pairlist2(... = ), list(evaluate = TRUE)
  )
  # TODO fix needing to use quote instead of rlang functions, resulting from
  # the necessaity to not expand !!! and absence of quo0
  # TODO now need to check dots inside of body and add extra arguments to
  # call2 arguments. Arguments should be say, basemeancalc.na.rm = T, where
  # I splice the first part, keeping second part and adding to
  # function_args_transforms
  convoke_func_body <- quote({
    func_to_call <-
      rlang::call2(
        interface, !!!func_args_transforms[[interface]],
        !!!get_interface_args(rlang::list2(...), interface)
      )
    # TODO there seems to be issue with env when the name is also defined
    # in the top (global) environment
    if (evaluate == TRUE) {
      rlang::eval_tidy(func_to_call,
        env = rlang::env_poke_parent(rlang::current_env(), func_envs)
      )
    } else {
      return(func_to_call)
    }
  })
  retfunc <- rlang::new_function(convoke_func_args, convoke_func_body)
  class(retfunc) <- c("convoke", "function")
  return(retfunc)
}

#' @export
`+.convoke` <- function(e1, e2) {
  if ("convoke" %in% class(e1)) {
    convoke_func <- e1
    func_spec <- rlang::enquo0(e2)
  } else {
    convoke_func <- e2
    func_spec <- rlang::enquo0(e1)
  }
  # TODO instead refactor the first part of convoke e.g.func_specify$func_specs
  # TODO in below, enforce only parentheses and no brackets or braces
  # remove parentheses
  func_spec <- rlang::quo_set_expr(
    func_spec,
    rlang::quo_get_expr(func_spec)[[2]]
  )
  if (!rlang::is_call_simple(rlang::quo_get_expr(func_spec))) {
    rlang::abort("specification needs to be function call")
  }
  func_env <- rlang::quo_get_env(func_spec)
  # wrap in list since can't assign names to singletons otherwise
  func_args_transform <- list(rlang::call_args(func_spec))
  func_name <- rlang::call_name(func_spec)
  names(func_args_transform) <- func_name
  # TODO below could be written shorter
  # it is essential to clone the environment otherwise is inherited
  convoke_env <- rlang::env_clone(rlang::fn_env(convoke_func))
  func_args_transforms <- c(rlang::env_get(
    convoke_env,
    "func_args_transforms"
  ), func_args_transform)
  func_envs <- rlang::env_clone(func_env,
    parent =
      rlang::env_get(convoke_env, "func_envs")
  )
  rlang::env_bind(convoke_env,
    func_args_transforms = func_args_transforms,
    func_envs = func_envs
  )
  rlang::fn_env(convoke_func) <- convoke_env
  return(convoke_func)
}

#' @export
print.convoke <- function(x, ...) {
  func_args_transforms <- rlang::env_get(
    rlang::fn_env(x),
    "func_args_transforms"
  )
  func_names <- names(func_args_transforms)
  interfaces <- paste(
    " ", "interfaces:",
    paste(func_names, "()", sep = "", collapse = ", ")
  )
  arguments <- paste(
    " ", "args:",
    paste(pretty_func_args(x, "interface.args"),
      collapse = ", "
    )
  )
  cat("convoke function", interfaces, arguments, sep = "\n")
}

#' @export
names.convoke <- function(x) {
  names(rlang::env_get(rlang::fn_env(x), "func_args_transforms"))
}
