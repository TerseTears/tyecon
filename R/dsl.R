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

# helper function for dealing with unnamed arguments captured in expressions
# e.g. plot(x,y) that are meant to mean the actual x, y arguments of the 
# function (not x, y variables).
set_call_args_names <- function(args) {
    new_names <- purrr::map2_chr(rlang::names2(args),
                                rlang::names2(rlang::set_names(args)),
                                ~ if(.x!="") .x else .y)
    new_args <- purrr::map2(rlang::names2(args), args, ~
                    if(.x!="") .y else rlang::missing_arg())
    rlang::set_names(new_args, new_names)
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

pretty_func_args <- function(func, extra_args = NULL) {
    arg_names <- rlang::fn_fmls_names(func)
    arg_names <- c(arg_names[1:(length(arg_names)-2)], extra_args)
    arg_vals <- rlang::fn_fmls(func)
    arg_vals <- c(arg_vals[1:(length(arg_vals)-2)], "")
    arg_vals <- purrr::map_chr(arg_vals, ~ if(.=="") "" else paste(" =", .))
    paste0(arg_names, arg_vals)
}

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
#' The unifying interface needs to be specified in the format
#' ``` 
#' \(arg1 = default1, arg2 = default2, etc.) "description"
#' ```
#' This uses R's recent anonymous function syntax.
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
#' It is also possible to progressively add functions to a convoke function
#' simply by adding to the convoke function the new specifications:
#' ```
#' convoke_func + (func_spec ~ postfunc)
#' ```
#'
#' @family function assemblers
#' @example examples/examples-convoke.R
#'
#' @param uniface \[`function`\] The desired unifying interface. Function returning
#' a string as description
#'
#' @param ... \[`function` ~ `purrr_lambda`\] All the various specifications for function argument
#' transformations.
#'
#' @return Function with additional class *convoke* with arguments being
#' ```
#' convoke_func(specified_args, interface, evaluate = TRUE, ...)
#' ```
#' The `evaluate` argument is useful for debugging purposes. Extra arguments
#' be passed in the format `interface.arg`.
#'
#' @export
convoke <- function(uniface, ...){
    dots <- rlang::enquos0(...)
    if (!all(purrr::map_lgl(dots,
                            ~rlang::is_formula(rlang::quo_get_expr(.))))) {
        rlang::abort("specification needs to be in formula form")
    }
    if(!rlang::is_function(uniface)) {
        rlang::abort("unified interface specification not a function call")
    }
    if(!rlang::is_string(rlang::fn_body(uniface)[[2]])) {
        rlang::abort("unified function description needs to be string")
    }
    func_specs <- purrr::map(dots, f_lhs_quo)
    func_envs <- rlang::quo_get_env(func_specs[[1]])
    post_funcs <- purrr::map(dots, f_rhs_func)

    # functions to convoke start from second argument
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
    convoke_func_args <- c(rlang::fn_fmls(uniface),
                          list(interface=func_names[[1]]),
                          rlang::pairlist2(...=), list(evaluate=TRUE))

    # TODO fix needing to use quote instead of rlang functions, resulting from
    # the necessaity to not expand !!! and absence of quo0
    # TODO now need to check dots inside of body and add extra arguments to 
    # call2 arguments. Arguments should be say, basemeancalc.na.rm = T, where
    # I splice the first part, keeping second part and adding to 
    # function_args_transforms
    convoke_func_body <- quote({
        func_to_call <- 
            rlang::call2(interface, !!!func_args_transforms[[interface]],
                  !!!get_interface_args(rlang::list2(...), interface));
        # TODO there seems to be issue with env when the name is also defined
        # in the top (global) environment
        if (evaluate==TRUE){
            post_funcs[[interface]](rlang::eval_tidy(func_to_call,
                env = rlang::env_poke_parent(rlang::current_env(), func_envs)))}
        else return(func_to_call)})

    retfunc <- rlang::new_function(convoke_func_args, convoke_func_body)
    class(retfunc) <- c("convoke", "function")

    return(retfunc)
}

#' @export
`+.convoke` <- function(e1, e2) {
    if("convoke" %in% class(e1)) {
        convoke_func <- e1
        func_quo <- rlang::enquo0(e2)
    }
    else {
        convoke_func <- e2
        func_quo <- rlang::enquo0(e1)
    }

    # TODO instead refactor the first part of convoke e.g.func_specify$func_specs
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
    convoke_env <- rlang::env_clone(rlang::fn_env(convoke_func))
    func_args_transforms <- c(rlang::env_get(convoke_env,
                                  "func_args_transforms"), func_args_transform)
    func_envs <- rlang::env_clone(func_env, parent = 
                                  rlang::env_get(convoke_env, "func_envs"))
    post_funcs <- c(rlang::env_get(convoke_env, "post_funcs"), post_func)

    rlang::env_bind(convoke_env, func_args_transforms = func_args_transforms,
             func_envs = func_envs, post_funcs = post_funcs)

    rlang::fn_env(convoke_func) <- convoke_env

    return(convoke_func)
}

#' @export
print.convoke <- function(x, ...) {
    func_args_transforms <- rlang::env_get(rlang::fn_env(x),
                                           "func_args_transforms")
    func_names <- names(func_args_transforms)

    uniface <- rlang::env_get(rlang::fn_env(x), "uniface")

    header <- paste("convoke function", rlang::fn_body(uniface)[[2]])
    interfaces <- paste(" ", "interfaces:", 
                        paste(func_names, "()", sep="", collapse=", "))
    arguments <- paste(" ", "args:",
                       paste(pretty_func_args(x, "interface.args"),
                             collapse=", "))

    cat(header, interfaces, arguments, sep="\n")
}

#' @export
names.convoke <- function(x) {
    names(rlang::env_get(rlang::fn_env(x), "func_args_transforms"))
}

# TODO QoL support glue for automatic injection of object class in variable names
#' Combine generic method interfaces
#'
#' `conflate` combines methods for any S3 standard generic allowing arguments
#' of various methods to be specified in a single function that it returns.
#'
#' # Combining Methods
#'
#' The `generic_spec` argument is the generic function as applied to
#' shared arguments between methods that are to be combined. Once this is
#' decided, all additional arguments passed to the generated function need to
#' be of the form `object_class.arg` that are arguments unique to each method
#' as dispatched for each object class.
#' 
#' @family function assemblers
#' @example examples/examples-conflate.R
#'
#' @param `generic_spec` \[`call`\] Generic function as applied to arguments 
#' shared between all methods to be combined.
#'
#' @return Function with additional class *conflate* with arguments being
#'
#' ```
#' conflate_func(generic_spec_args, ..., evaluate = TRUE)
#' ```
#' Where `...` specifies additional arguments to be supplied in the form
#' `object_class.arg`. The `evaluate` argument is useful for debugging purposes.
#'
#' @export
conflate <- function(generic_spec) {
    generic_spec <- rlang::enquo0(generic_spec)
    if(!isS3stdGeneric(rlang::call_name(generic_spec))) {
        rlang::abort("function needs to be standard S3 generic")
    }
    conflate_func_args <- set_call_args_names(rlang::call_args(generic_spec))
    spec_env <- rlang::quo_get_env(generic_spec)
    conflate_func_body <- substitute({
        interface <- class(firstarg)[[1]]
        retcall <- rlang::call2(func_name, !!!baseargs,
                          !!!get_interface_args(rlang::list2(...), interface))
    if (evaluate) {
        return(rlang::eval_tidy(retcall, env =
                    rlang::env_poke_parent(rlang::current_env(), spec_env)))
    } else return(retcall)
    }, list(firstarg=rlang::sym(rlang::names2(conflate_func_args[1])),
            baseargs=rlang::call_args(generic_spec),
            func_name=rlang::call_name(generic_spec)))

    retfunc <- rlang::new_function(c(conflate_func_args, 
                                     rlang::pairlist2(...=),evaluate=TRUE),
                                   conflate_func_body)
    class(retfunc) <- c("conflate", "function")

    return(retfunc)
}

#' @export
print.conflate <- function(x, ...) {
    generic_spec <- rlang::env_get(rlang::fn_env(x), "generic_spec")

    header <- paste("conflate function for", rlang::call_name(generic_spec))
    arguments <- paste(" ", "args:",
                       paste(pretty_func_args(x, "object.args"), collapse=", "))

    cat(header, arguments, sep="\n")
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
