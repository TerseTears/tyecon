# TODO QoL support glue for automatic injection of object class in variable names

#' Combine generic method interfaces
#'
#' `conflate` combines methods for any S3 standard generic allowing arguments
#' of various methods to be specified in a single function that it returns.
#'
#' # Combining Methods
#'
#' The generic_spec argument is the generic function as applied to
#' shared arguments between methods that are to be combined. Once this is
#' decided, all additional arguments passed to the generated function need to
#' be of the form `object_class.arg` that are arguments unique to each method
#' as dispatched for each object class.
#'
#' @family function assemblers
#' @example examples/examples-conflate.R
#'
#' @param generic_spec \[`call`\] Generic function as applied to arguments
#' shared between all methods to be combined.
#'
#' @return Function with additional class *conflate* with arguments being
#'
#' ```r
#' conflate_func(generic_spec_args, ..., evaluate = TRUE)
#' ```
#' Where `...` specifies additional arguments to be supplied in the form
#' `object_class.arg`. The `evaluate` argument is useful for debugging purposes.
#'
#' @export
conflate <- function(generic_spec) {
  generic_spec <- rlang::enquo0(generic_spec)
  if (!isS3stdGeneric(rlang::call_name(generic_spec))) {
    rlang::abort("function needs to be standard S3 generic")
  }
  conflate_func_args <- set_call_args_names(rlang::call_args(generic_spec))
  spec_env <- rlang::quo_get_env(generic_spec)
  conflate_func_body <- substitute(
    {
      interface <- class(firstarg)[[1]]
      retcall <- rlang::call2(
        func_name, !!!baseargs,
        !!!get_interface_args(rlang::list2(...), interface)
      )
      if (evaluate) {
        return(rlang::eval_tidy(
          retcall,
          env = rlang::env_poke_parent(rlang::current_env(), spec_env)
        ))
      } else {
        return(retcall)
      }
    },
    list(
      firstarg = rlang::sym(rlang::names2(conflate_func_args[1])),
      baseargs = rlang::call_args(generic_spec),
      func_name = rlang::call_name(generic_spec)
    )
  )
  retfunc <- rlang::new_function(
    c(conflate_func_args,
      rlang::pairlist2(... = ),
      evaluate = TRUE
    ),
    conflate_func_body
  )
  class(retfunc) <- c("conflate", "function")
  return(retfunc)
}

#' @export
print.conflate <- function(x, ...) {
  generic_spec <- rlang::env_get(rlang::fn_env(x), "generic_spec")
  header <- paste("conflate function for", rlang::call_name(generic_spec))
  arguments <- paste(
    " args:", paste(pretty_func_args(x, "object.args"), collapse = ", ")
  )
  cat(header, arguments, sep = "\n")
}
