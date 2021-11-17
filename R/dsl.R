# The goal of this file is to take the dsl-specified input to convert object/function to the unified interface. Ultimately, it should be able to do some automations with the help from structure definition, and also general text inputs (as authored by package writers as the dsl verbs and statements.

# helper function to get from dots only arguments prefixed with the interface 
# name followed by a . character (e.g. basemeancalc.na.rm)
get_interface_args <- function(arguments, interface){
    interface_args <- arguments[stringr::str_detect(names(arguments),
                                    paste0("^", interface, "\\."))]
    names(interface_args) <- stringr::str_extract(names(interface_args),
                                 paste0("(?<=", interface, "\\.).*"))
    return(interface_args)
}

# functions to get lhs and rhs of quosures with formula structure
f_lhs_quo <- function(quosure){
    rlang::quo_set_expr(quosure, rlang::f_lhs(rlang::quo_get_expr(quosure)))
}
f_rhs_func <- function(quosure){
    func <- rlang::as_function(rlang::new_formula(NULL,
                                   rlang::f_rhs(rlang::quo_get_expr(quosure))))
    rlang::fn_env(func) <- rlang::quo_get_env(quosure)
    return(func)
}

pretty_func_args <- function(func) {
    arg_names <- rlang::fn_fmls_names(func)
    arg_names <- c(arg_names[1:(length(arg_names)-2)], "interface.arg")
    arg_vals <- rlang::fn_fmls(func)
    arg_vals <- c(arg_vals[1:(length(arg_vals)-2)], "")
    paste(arg_names, "=", arg_vals)
}

#' Unify functions interfaces
#'
#' `yeksar` unifies functions along a single specification using
#' statements on function argument transformations that would be needed. The
#' result is a single function whose *interface* can be changed simply by
#' changing the respective argument in the generated function.
#'
#' # Unifying Functions
#'
#' ## Specifying the unifying interface
#'
#' The unifying interface needs to be specified in the format
#' ``` 
#' ..(arg1 = default1, arg2 = default2, etc.)
#' ```
#' Where the `..` prefix and parentheses are required and an error is
#' thrown otherwise. The equality sign in arguments specifications is also
#' necessary. Simply omit the default value on the RHS if not needed.
#'
#' ## Specifying function argument transformations
#'
#' Transformations follow the formula format
#' ```
#' func1(func1arg1 = transformed_arg1, func1arg2 = transformed_arg2) ~
#' postprocess(result)
#' ```
#' Essentially, pass the function as if it is to take
#' arguments from the unified function. 
#' ### Postprocessing function
#' The postprocess function on the right of the formula specifies what
#' postprocessing is required on the result of applying `func1`. It is to be 
#' specified similarly to how functions in `purrr::map` are specified (that is
#' by the `.` and `.x` syntax.
#'
#' # Composing functions
#'
#' It is also possible to progressively add functions to a yeksar function
#' simply by adding to the yeksar function the new specifications:
#' ```
#' yeksar_func + (func_spec ~ postfunc)
#' ```
#'
#' @param ... First element is the desired unifying interface. Remaining
#' elements are all the various specifications for function argument
#' transformations.
#'
#' @return Function with additional class *yeksar* with arguments being
#' ```
#' yeksar_func(specified_args, interface, evaluate = TRUE, ...)
#' ```
#' The `evaluate`
#' argument is useful for debugging purposes. Extra arguments can be passed in 
#' the format `interface.arg`.
#'
#' @example examples/examples-yeksar.R
#'
#' @export
yeksar <- function(...){
    dots <- rlang::enquos0(...)
    if (!all(purrr::map_lgl(dots[-1],
                            ~rlang::is_formula(rlang::quo_get_expr(.))))) {
        rlang::abort("specification needs to be in formula form")
    }
    if(!rlang::is_call(rlang::quo_get_expr(dots[[1]]), c(".", ".."))) {
        rlang::abort("unified interface specification not a call,
              or not one to . or .. (required for consistency)")
    }
    if(!rlang::is_named(rlang::call_args(rlang::quo_get_expr(dots[[1]])))) {
        rlang::abort("unified interface specification must contain = for args,
              if no default is required, leave RHS empty")
    }
    func_specs <- purrr::map(dots[-1], f_lhs_quo)
    func_envs <- rlang::quo_get_env(func_specs[[1]])
    post_funcs <- purrr::map(dots[-1], f_rhs_func)

    # functions to yeksar start from second argument
    func_names <- purrr::map(func_specs, rlang::call_name)

    # transformation instructions are directly given as named arguments for 
    # each function
    func_args_transforms <- purrr::map(func_specs, rlang::call_args)

    # name components to access them with the interface argument
    names(func_args_transforms) <- func_names
    names(post_funcs) <- func_names

    # add function args as ones specified as first argument, plus an interface
    # argument, and whether to return produced function (for debugging) or
    # evaluate in place
    yeksar_func_args <- c(rlang::call_args(dots[[1]]), 
                          list(interface=func_names[[1]], evaluate=TRUE), 
                               rlang::pairlist2(...=))

    # TODO fix needing to use quote instead of rlang functions, resulting from
    # the necessaity to not expand !!! and absence of quo0
    # TODO now need to check dots inside of body and add extra arguments to 
    # call2 arguments. Arguments should be say, basemeancalc.na.rm = T, where
    # I splice the first part, keeping second part and adding to 
    # function_args_transforms
    yeksar_func_body <- quote({
        func_to_call <- 
            rlang::call2(interface, !!!func_args_transforms[[interface]],
                  !!!get_interface_args(rlang::list2(...), interface));
        # TODO make the func_specs[[1]] more readable
        if (evaluate==TRUE){
            post_funcs[[interface]](rlang::eval_tidy(func_to_call,
             env = rlang::env_clone(func_envs, parent = rlang::current_env())))}
        else return(func_to_call)})

    retfunc <- rlang::new_function(yeksar_func_args, yeksar_func_body)
    class(retfunc) <- c("yeksar", "function")

    return(retfunc)
}

#' @export
`+.yeksar` <- function(e1, e2) {
    if("yeksar" %in% class(e1)) {
        yeksar_func <- e1
        func_quo <- rlang::enquo0(e2)
    }
    else {
        yeksar_func <- e2
        func_quo <- rlang::enquo0(e1)
    }

    # TODO instead refactor the first part of yeksar e.g.func_specify$func_specs
    # TODO in below, enforce only parentheses and no brackets or braces
    # remove parentheses
    func_quo <- rlang::quo_set_expr(func_quo,
                                    rlang::quo_get_expr(func_quo)[[2]])

    if (!rlang::is_formula(rlang::quo_get_expr(func_quo))) {
        rlang::abort("specification needs to be in formula form")
    }
    func_spec <- f_lhs_quo(func_quo)
    func_env <- rlang::quo_get_env(func_spec)
    # wrap in list since can't assign names to singletons otherwise
    post_func <- list(f_rhs_func(func_quo))
    func_args_transform <- list(rlang::call_args(func_spec))
    func_name <- rlang::call_name(func_spec)

    names(func_args_transform) <- func_name
    names(post_func) <- func_name

    # TODO below could be written shorter
    # it is essential to clone the environment otherwise is inherited
    yeksar_env <- rlang::env_clone(rlang::fn_env(yeksar_func))
    func_args_transforms <- c(rlang::env_get(yeksar_env,
                                  "func_args_transforms"), func_args_transform)
    func_envs <- rlang::env_clone(func_env, parent = 
                                  rlang::env_get(yeksar_env, "func_envs"))
    post_funcs <- c(rlang::env_get(yeksar_env, "post_funcs"), post_func)

    rlang::env_bind(yeksar_env, func_args_transforms = func_args_transforms,
             func_envs = func_envs, post_funcs = post_funcs)

    rlang::fn_env(yeksar_func) <- yeksar_env

    return(yeksar_func)
}

#' @export
print.yeksar <- function(x, ...) {
    func_args_transforms <- rlang::env_get(rlang::fn_env(x),
                                           "func_args_transforms")
    func_names <- names(func_args_transforms)

    header <- paste("yeksar function with", length(func_names), "interfaces")
    interfaces <- paste(" ", "\033[1minterfaces:\033[22m", 
                        paste(func_names, "()", sep="", collapse=", "))
    arguments <- paste(" ", "\033[1margs:\033[22m", 
                       paste(pretty_func_args(x), collapse="\n\t"))

    cat(header, interfaces, arguments, sep="\n")
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
#' @export
#' @rdname to-operator
`%to%` <- function(obj, blocks) {
    blocks <- rlang::enquo0(blocks)
    if(!isTRUE(all.equal(rlang::quo_get_expr(blocks)[1], rlang::expr({})))) {
        rlang::abort("expressions need to be in a code block")
    }
    if (!all(purrr::map_lgl(rlang::quo_get_expr(blocks)[-1],
                            rlang::is_formula))) {
        rlang::abort("expressions needs to be in formula form")
    }
    blocks <- purrr::map(rlang::quo_get_expr(blocks)[-1], ~
                  rlang::new_quosure(., rlang::quo_get_env(blocks)))
    resnames <- purrr::map(blocks, ~ rlang::as_string(
                                         rlang::f_lhs(rlang::quo_get_expr(.))))
    funcs <- purrr::map(blocks, f_rhs_func)
    reslist <- purrr::map(rlang::set_names(funcs, resnames), ~.(obj))
    # make non-singleton elements a list to use in dataframe
    reslist <- purrr::map(reslist, ~ if(length(.)==1) . else I(list(.)))

    if (requireNamespace("tibble", quietly = TRUE)) {
        tibble::as_tibble(reslist)
    }
    else {
        as.data.frame(reslist)
    }
}
