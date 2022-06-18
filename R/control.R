#' Evaluate expression over multiple values
#'
#' `control` allows specifying multiple values for variables in an expression,
#' returning all results in a `tibble`.
#'
#' @family result assemblers
#' @example examples/examples-control.R
#'
#' @param code \[`expr`\] Expression to be evaluated.
#' @param ... \[`argument value pairs`\] Variables in `code` with the multiple
#' values assigned to them.
#' @param .refiner \[`function`\] Preprocessing the value tree over which
#' evaluation of `code` is to be performed. Takes the tree `tibble` as input.
#' @param .prober \[`function`\] Extracting extra information from the results
#' of evaluating `code`. Takes the list of such values as input.
#' @param .selector \[`function`\] Modifying the final `tibble` or extracting
#' what's needed from it. Takes the refined, evaluated, probed `tibble` as
#' input.
#' @param unnest_value \[`boolean`\] Whether to unnest the results inside the
#' `tibble`.
#' @param unnest_summary \[`boolean`\] Whether to unnest the results from
#' `.prober` inside the `tibble`.
#'
#' @return A `tibble` with information on the evaluation `tree`, and the
#' columns `.value`, and `.summary` if probed and not unnested.
#'
#' @export
control <- function(code, ...,
                    .refiner = identity, .prober, .selector = identity,
                    unnest_value = FALSE, unnest_summary = FALSE) {
  # TODO Needs to be enquo0 to allow !! inside "code"
  code <- rlang::enquo0(code)
  code_env <- rlang::quo_get_env(code)
  code <- rlang::quo_get_expr(code)
  values_df <- tree_expand(...)
  values_df <- rlang::as_function(.refiner)(values_df)

  values_df <- tibble::add_column(values_df,
    .value =
      purrr::pmap(
        values_df,
        rlang::new_function(
          set_call_args_names(names(values_df)), code, code_env
        )
      )
  )
  # TODO (this could be more optimized) Do not list up if of length one
  if (all(purrr::map_int(values_df$.value, length) == 1)) {
    values_df$.value <- unlist(values_df$.value)
  }
  if (unnest_value) {
    values_df <- tidyr::unnest_wider(values_df, .value, strict = TRUE)
  }

  if (!rlang::is_missing(.prober)) {
    values_df <- tibble::add_column(values_df,
      .summary = purrr::map(values_df$.value, ~ rlang::as_function(.prober)(.))
    )
    # TODO (same as above) Do not list up if of length one
    if (all(purrr::map_int(values_df$.summary, length) == 1)) {
      values_df$.summary <- unlist(values_df$.summary)
  }
  }
  if (unnest_summary) {
    values_df <- tidyr::unnest_wider(values_df, .summary, strict = TRUE)
  }

  return(rlang::as_function(.selector)(values_df))
}

tree_expand <- function(...) {
  values_levels <- rlang::enquos(...)
  values_env <- rlang::quo_get_env(values_levels[[1]])
  values_levels <- purrr::map(values_levels, rlang::quo_get_expr)
  values_levels <- purrr::modify_if(
    values_levels, ~ !rlang::is_formula(.), ~ rlang::expr(!!. ~ 1)
  )
  values <- purrr::map(
    values_levels,
    ~ rlang::eval_tidy(rlang::f_lhs(.), env = values_env)
  )
  levels <- purrr::map_dbl(values_levels, rlang::f_rhs)

  level_values <- purrr::map(sort(unique(levels)), ~ values[levels == .])

  # TODO below unique check on outer map_int could use better error handling.
  value_lengths <-
    purrr::map_int(level_values, ~ unique(purrr::map_int(., length)))

  times_vec <- c(1, head(purrr::accumulate(value_lengths, `*`), -1))
  # TODO is each just the reverse of times?
  each_vec <-
    c(tail(purrr::accumulate(value_lengths, `*`, .dir = "backward"), -1), 1)

  expanded_list <- purrr::flatten(purrr::pmap(
    list(level_values, times_vec, each_vec),
    function(values, times, each) {
      purrr::map(values, ~ rep(., times = times, each = each))
    }
  ))

  return(tibble::tibble(!!!expanded_list))
}
