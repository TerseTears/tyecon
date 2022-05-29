#' Piping environment for brevity and coalescence
#'
#' `%->%` `convey` operator allows omission of the `%>%` pipe operator in its
#' environment as well as setting local bindings that can be used at later
#' stages of the sequence of functions.
#'
#' # Writing Shorter, More Integrated Pipes
#'
#' Piping is usually one single context, therefore all the extra pipe operators
#' at the end of each instruction seems extraneous. Nevertheless, one may need
#' to record the result of a pipe up to a certain stage, to later build the
#' whole object out of the simpler modifications of the original object. This
#' too is Something that the simple pipe operator can't handle. Therefore,
#' the solution is to define a context for these two cases, perform the
#' operations therein, and return the desired result. That is what the "convey"
#' operator does.
#'
#' # Usage of the *convey* Operator
#'
#' ```
#' object %->% code
#' ```
#'
#' The object part can be any single object or the result of previous piping
#' operations. The instructions are exactly as if each command was sequentially
#' passed to the next via the conventional *magrittr* pipe. Each time a function
#' instruction is followed by a binding, the respective symbol is bound to the
#' specified computation. See examples and \code{\link{conserve}}.
#'
#' @family result assemblers
#' @example examples/examples-convey-operator.R
#'
#' @param obj \[R `object`\] Any object, specifically data or results of
#' previous pipes.
#' @param code \[individual `bindings` and R `commands`\] Instructions
#' wrapped in curly braces to encapsualte the context of the pipe.
#'
#' @return An object resulting from the transformations applied to it by the
#' `code`.
#'
#' @rdname convey-operator
#' @export
# TODO perhaps adding tyecon::conserve fixes requirement of exporting conserve.
`%->%` <- function(obj, code) {
  code <- rlang::enquo(code)
  code_env <- rlang::quo_get_env(code)
  code_exprs <- rlang::quo_get_expr(code)[-1]
  code_exprs <- purrr::modify_if(
    code_exprs,
    ~ rlang::is_call(., "<-"), ~ rlang::expr(conserve(., !!.[[2]], !!.[[3]]))
  )
  rlang::eval_tidy(
    purrr::reduce(code_exprs, ~ rlang::expr(!!.x %!>% !!.y),
      .init = rlang::expr(!!obj)
    ),
    env = code_env
  )
}

#' Maintain name as binding in local environment
#'
#' `conserve` allows its third argument to be bound to the name given as its
#' second argument in the local environment, while returning the first argument.
#' A sort of side-effect function which binds to the caller environment.
#'
#' # Conserving Intermediate Objects
#'
#' The best use of `conserve` is when one needs to keep an object in a long
#' sequence of commands to be used later in the same sequence (so mostly pipes).
#' It is essential to provide this local context (e.g. using `local` or inside
#' functions) and avoid altering the global environment, since internally,
#' `rlang::local_bindings` is used. The `conserve` function is what is used with
#' the "convey" pipe to bind intermediary objects to provided symbols. Exported
#' for the rare occasion it may be useful on its own. One can also specifically
#' set `conserve(name, value)` directives among the `convey` instructions.
#'
#' The `value` argument can be specified in as a `magrittr` pipe context. That
#' is, automatic data masking as well as the `.` symbol representing the
#' original object.
#'
#' @family result assemblers
#'
#' @param obj \[R `object`\] Any object, specifically data.
#' @param name \[`symbol` or `string`\] The name to which a value is to be
#' bound locally.
#' @param value \[R `object`\] The value for the name.
#'
#' @return The `obj` itself. The side effect being local binding of `value` to
#' the specified `name`.
#'
#' @export
conserve <- function(obj, name, value) {
  name <- rlang::enexpr(name)
  value <- rlang::enexpr(value)
  # TODO NULL can be problematic below. Use empty list as data mask instead.
  data_mask <- if (rlang::is_named(obj)) obj else NULL
  # TODO there should be a simpler way than below to captures symbols in rlang
  # TODO below seems very hacky to me. There's got to be a better way
  # to be able to use dot as the reference to the original object
  # TODO there seems to be a bug with rlang where supplying data
  # mask ignores the default env argument so need to resupply it.
  rlang::local_bindings(
    !!!rlang::set_names(
      list(rlang::eval_tidy(rlang::expr(
        !!obj %!>% {
          !!value
        }
      ),
      data = data_mask, env = rlang::caller_env()
      )),
      rlang::as_name(name)
    ),
    .frame = rlang::caller_env()
  )
  return(obj)
}
#' Piping environment for transforming data into new data format
#'
#' `%$>%` construct operator allows taking apart the elements of a data to build
#' a new data type more quickly. This is done by data masking and access to the
#' original object by the `.` pronoun from `magrittr`. Each line is a component
#' of the new data. If the line is an assignment, the new component name is the
#' assigned variable name, otherwise, its position is determined by how many
#' previous variables have been assigned. See examples.
#'
#' @family result assemblers
#' @example examples/examples-construct-operator.R
#'
#' @param data \[R `object`\] Any object, specifically data or results of
#' previous pipes.
#' @param code \[individual `bindings` and R `commands`\] Instructions
#' wrapped in curly braces to encapsualte the context of the pipe.
#'
#' @return list with fields specified by any assignments inside `code`.
#'
#' @rdname construct-operator
#' @export
# TODO maybe later I can add annotations inside for the idea of a graph of
# relations among model elements for visualization
`%$>%` <- function(data, code) {
  code <- rlang::enquo(code)
  code_env <- rlang::quo_get_env(code)
  code_exprs <- rlang::quo_get_expr(code)[-1]
  data_mask <- if (rlang::is_named(data)) {
    rlang::as_data_mask(data)
  } else {
    rlang::as_data_mask(list())
  }
  ret_data <- purrr::map(code_exprs, ~
    rlang::eval_tidy(
      rlang::expr(
        !!data %!>% {
          !!.
        }
        ),
      data = data_mask,
      env = code_env
      ))
ret_data <- rlang::set_names(
  ret_data,
  purrr::map(code_exprs, ~ if (rlang::is_call(., "<-")) .[[2]] else "")
)
  # Use unnamed lists as is
  ret_data <- purrr::reduce2(ret_data, names(ret_data),
    function(prev, .x, .y) {
      if (.y == "") {
        c(prev, .x)
      } else {
        c(prev, rlang::set_names(list(.x), .y))
      }
    },
    .init = list()
  )
  return(ret_data)
}
