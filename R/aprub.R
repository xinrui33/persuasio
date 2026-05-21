#' Upper bound of Average Persuasion Rate
#'
#' @export
aprub <- function(data, Y, T, Z, X = NULL, model = "no_interaction") {

  if (is.null(X)) {

    fA <- lm(as.formula(paste(Y, "~", Z)), data = data)
    fB <- lm(as.formula(paste("I(1 -", Y, ")", "~", Z)), data = data)

    alpha_0 <- coef(fA)["(Intercept)"]
    alpha_1 <- coef(fA)[Z]
    beta_0  <- coef(fB)["(Intercept)"]

    ub <- (alpha_0 + alpha_1 - beta_0) / (1 - beta_0)

    return(list(
      ub_coef = ub,
      ub_se = NA,
      ci_lb = NA,
      ci_ub = NA,
      outcome = Y,
      instrument = Z,
      covariates = X,
      model = model,
      n = nrow(data),
      class = "aprub"
    ))

  } else {

    fmla <- as.formula(paste(Y, "~", Z, "+", paste(X, collapse = "+")))
    fit <- lm(fmla, data = data)

    ub <- mean(predict(fit))

    return(list(
      ub_coef = ub,
      ub_se = NA,
      ci_lb = NA,
      ci_ub = NA,
      outcome = Y,
      instrument = Z,
      covariates = X,
      model = model,
      n = nrow(data),
      class = "aprub"
    ))
  }
}
