set.seed(123)
# Use control to apply multiple arguments to the same expression
control(
  {
    lm(Sepal.Length ~ ., data = rsample::analysis(fold$splits))
  },
  fold = purrr::transpose(rsample::vfold_cv(iris, 5))
)

# Use .selector to alter output
control(
  {
    lm(Sepal.Length ~ ., data = rsample::analysis(fold$splits))
  },
  fold = purrr::transpose(rsample::vfold_cv(iris, 5)),
  .selector = ~ tidyr::unnest_wider(., fold, strict = TRUE)
)

# Use .prober to extract information from results
control(
  {
    lm(Sepal.Length ~ ., data = rsample::analysis(fold$splits))
  },
  fold = purrr::transpose(rsample::vfold_cv(iris, 5)),
  .prober = ~ summary(.)$r.squared
)

# Use list() and unnest_value to return multiple results
control(
  {
    model <- lm(Sepal.Length ~ ., data = rsample::analysis(fold$splits))
    holdout <- rsample::assessment(fold$splits)
    holdout$.fit <- predict(model, holdout)
    rmse_value <- yardstick::rmse(holdout, Sepal.Length, .fit)
    list(model = model, rmse = rmse_value)
  },
  fold = purrr::transpose(rsample::vfold_cv(iris, 5)),
  .unnest_value = TRUE
)

# Use multiple levels with the formula syntax
control(
  {
    model <- earth::earth(
      Sepal.Length ~ ., rsample::analysis(fold$splits), degree = degree)
    holdout <- rsample::assessment(fold$splits)
    holdout$.fit <- predict(model, holdout, "response")[, 1]
    rmse_value <- yardstick::rmse(holdout, Sepal.Length, .fit)
    list(model = model, rmse = rmse_value)
  },
  fold = purrr::transpose(rsample::vfold_cv(iris, 3)) ~ 1,
  degree = 1:5 ~ 2,
  .unnest_value = TRUE,
  .selector = ~ dplyr::group_by(., degree) %>%
    dplyr::summarise(
      model = list(dplyr::first(model)),
      rmse = mean(rmse$.estimate)
    )
)

# Example with tuning a ridge model
control(
  {
    keepin <- rsample::analysis(fold$splits)
    model <- elasticnet::enet(
      Sepal.Length ~ .,
      x = model.matrix(Sepal.Length ~ . - 1, keepin),
      y = keepin[["Sepal.Length"]],
      lambda = lambda
    )
    holdout <- rsample::assessment(fold$splits)
    holdout$.fit <- predict(
      model,
      model.matrix(Sepal.Length ~ . - 1, holdout),
      s = 1, mode = "fraction"
    )$fit
    rmse_value <- yardstick::rmse(holdout, Sepal.Length, .fit)
    list(model = model, rmse = rmse_value)
  },
  fold = purrr::transpose(rsample::vfold_cv(iris, 5)) ~ 1,
  lambda = seq(0, 0.1, 0.01) ~ 2,
  .unnest_value = TRUE,
  .selector = ~ dplyr::group_by(., lambda) %>%
    dplyr::summarise(
      model = list(dplyr::first(model)),
      rmse = mean(rmse$.estimate)
    )
)
