# The goal of this file is to take the dsl-specified input to convert object/function to the unified interface. Ultimately, it should be able to do some automations with the help from structure definition, and also general text inputs (as authored by package writers as the dsl verbs and statements.

# helper function to get from dots only arguments prefixed with the interface 
# name followed by a . character (e.g. basemeancalc.na.rm)
get_interface_args <- function(arguments, interface){
    interface_args <- 
        arguments[str_detect(names(arguments), paste0("^", interface, "\\."))]
    names(interface_args) <- 
        str_extract(names(interface_args), paste0("(?<=", interface, "\\.).*"))
    return(interface_args)
}

# functions to get lhs and rhs of quosures with formula structure
f_lhs_quo <- function(quosure){
    quo_set_expr(quosure, f_lhs(quo_get_expr(quosure)))
}
f_rhs_func <- function(quosure){
    func <- rlang::as_function(new_formula(NULL, f_rhs(quo_get_expr(quosure))))
    fn_env(func) <- quo_get_env(quosure)
    return(func)
}

# TODO currently, doesn't support extra arguments that are to be ignored for 
# irrelevant functions
yeksar <- function(...){
    dots <- enquos0(...)
    if (!all(map_lgl(dots[-1], ~is_formula(quo_get_expr(.))))) {
        abort("specification needs to be in formula form")
    }
    if(!is_call(quo_get_expr(dots[[1]]), c(".", ".."))) {
        abort("unified interface specification not a call, 
              or not one to . or .. (required for consistency)")
    }
    if(!is_named(call_args(quo_get_expr(dots[[1]])))) {
        abort("unified interface specification must contain = for args,
              if no default is required, leave RHS empty")
    }
    func_specs <- map(dots[-1], f_lhs_quo)
    post_funcs <- map(dots[-1], f_rhs_func)

    # functions to yeksar start from second argument
    func_names <- map(func_specs, call_name)

    # transformation instructions are directly given as named arguments for 
    # each function
    func_args_transforms <- map(func_specs, call_args)

    # name components to access them with the interface argument
    names(func_args_transforms) <- func_names
    names(post_funcs) <- func_names

    # add function args as ones specified as first argument, plus an interface
    # argument, and whether to return produced function (for debugging) or
    # evaluate in place
    yeksar_func_args <- c(call_args(dots[[1]]), 
                          list(interface=func_names[[1]], evaluate=TRUE), 
                               pairlist2(...=))

    # TODO fix needing to use quote instead of rlang functions, resulting from
    # the necessaity to not expand !!! and absence of quo0
    # TODO now need to check dots inside of body and add extra arguments to 
    # call2 arguments. Arguments should be say, basemeancalc.na.rm = T, where
    # I splice the first part, keeping second part and adding to 
    # function_args_transforms
    yeksar_func_body <- quote({
        func_to_call <- 
            call2(interface, !!!func_args_transforms[[interface]],
                  !!!get_interface_args(list2(...), interface));
        # TODO make the func_specs[[1]] more readable
        if (evaluate==TRUE){
            post_funcs[[interface]](eval_tidy(func_to_call,
             env = env_clone(quo_get_env(func_specs[[1]]),
                             parent = current_env())))}
        else return(func_to_call)})

    retfunc <- new_function(yeksar_func_args, yeksar_func_body)

    return(retfunc)
}

# TODO should also work with map and multiple data
# function to pass object to multiple lines of code and return results together
`%to%` <- function(obj, blocks) {
    blocks <- enquo0(blocks)
    if(!isTRUE(all.equal(quo_get_expr(blocks)[1], expr({})))) {
        abort("expressions need to be in a code block")
    }
    if (!all(map_lgl(quo_get_expr(blocks)[-1], is_formula))) {
        abort("expressions needs to be in formula form")
    }
    blocks <- map(quo_get_expr(blocks)[-1], ~
                  new_quosure(., quo_get_env(blocks)))
    resnames <- map(blocks, ~ as_string(f_lhs(quo_get_expr(.))))
    funcs <- map(blocks, f_rhs_func)
    reslist <- map(set_names(funcs, resnames), ~.(obj))
    # TODO currently, can't have unnamed list element name if to use 
    # map_dfr, hence .=.
    map_dfr(reslist, ~ if(length(.)==1) . else list(.=.))
}
