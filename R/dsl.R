# The goal of this file is to take the dsl-specified input to convert object/function to the unified interface. Ultimately, it should be able to do some automations with the help from structure definition, and also general text inputs (as authored by package writers as the dsl verbs and statements.

# helper function to get from dots only arguments prefixed with the interface 
# name followed by a . character (e.g. basemeancalc.na.rm)
get_interface_args <- function(arguments, interface){
    interface_args <- 
        arguments[str_detect(names(arguments), paste0("^", interface, "\\."))]
    names(interface_args) <- 
        str_extract(names(interface_args), paste0("(?<=", interface, "\\.)."))
    return(interface_args)
}

# TODO currently, doesn't support extra arguments that are to be ignored for 
# irrelevant functions
yeksar <- function(...){
    dots <- enquos0(...)

    # functions to yeksar start from second argument
    function_names <- map(dots[-1], call_name)

    # transformation instructions are directly given as named arguments for 
    # each function
    function_args_transforms <- map(dots[-1], call_args)
    names(function_args_transforms) <- function_names

    # add function args as ones specified as first argument, plus an interface
    # argument, and whether to return produced function (for debugging) or
    # evaluate in place
    yeksar_func_args <- c(call_args(dots[[1]]), 
                          list(interface=function_names[[1]], evaluate=TRUE), 
                               pairlist2(...=))

    # TODO fix needing to use quote instead of rlang functions, resulting from
    # the necessaity to not expand !!! and absence of quo0
    # TODO now need to check dots inside of body and add extra arguments to 
    # call2 arguments. Arguments should be say, basemeancalc.na.rm = T, where
    # I splice the first part, keeping second part and adding to 
    # function_args_transforms
    yeksar_func_body <- quote({
        function_to_call <- 
            call2(interface, !!!function_args_transforms[[interface]], 
                  !!!get_interface_args(list2(...), interface));
        if (evaluate==TRUE){
            # TODO this needs to be eval_tidy. check if anything breaks then
            eval(function_to_call)}
        else return(function_to_call)})

    retfunc <- new_function(yeksar_func_args, yeksar_func_body)

    return(retfunc)
}
