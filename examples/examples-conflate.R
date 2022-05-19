# first construct two models
glm.model <- glm(Sepal.Length ~ Sepal.Width, data = iris)
lm.model <- lm(Sepal.Length ~ Sepal.Width, data = iris)

# create summary with extra object arguments using `conflate`
(conflated_summary <- conflate(summary(x)))

purrr::map(
  list(glm.model, lm.model),
  ~ conflated_summary(.,
    lm.correlation = TRUE, glm.correlation = TRUE,
    glm.symbolic.cor = TRUE
  )
)

# alternatively, you could supply `correlation` as a default in conflate itself
(conflated_summary <- conflate(summary(x, correlation = TRUE)))

purrr::map(
  list(glm.model, lm.model),
  ~ conflated_summary(., lm.symbolic.cor = TRUE)
)
