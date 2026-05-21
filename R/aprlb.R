#' Lower bound of Average Persuasion Rate
#'
#' @export
aprlb <- function(data, Y, Z, X = NULL, model = "no_interaction", quiet = FALSE) {

  res <- .apr_engine(data, Y, Z, X, model)

  est_z <- data[[Z]]
  est_y <- data[[Y]]

  # recompute LB logic cleanly
  if (is.null(X)) {

    fmla <- as.formula(paste(Y, "~", Z))
    fit <- lm(fmla, data = data)

    lb_coef <- coef(fit)[Z] / (1 - coef(fit)["(Intercept)"])

    n <- nrow(data)
    k <- length(coef(fit))
    Xmat <- model.matrix(fit)
    e <- residuals(fit)

    vcov <- .sandwich_vcov(Xmat, e, n)

    order <- c(Z, "(Intercept)")
    V <- vcov[order, order]

    g <- matrix(c(
      1 / (1 - coef(fit)["(Intercept)"]),
      coef(fit)[Z] / (1 - coef(fit)["(Intercept)"])^2
    ), nrow = 1)

    var <- as.numeric(g %*% V %*% t(g))

    return(list(
      lb_coef = lb_coef,
      lb_se = sqrt(var),
      ci_lb = lb_coef - qnorm(0.975) * sqrt(var),
      ci_ub = lb_coef + qnorm(0.975) * sqrt(var),
      outcome = Y,
      instrument = Z,
      covariates = X,
      model = model,
      n = n,
      class = "aprlb"
    ))

  } else {

    # covariate case: reuse engine
    base <- .apr_engine(data, Y, Z, X, model)

    return(list(
      lb_coef = base$coef,
      lb_se = NA,
      ci_lb = NA,
      ci_ub = NA,
      outcome = Y,
      instrument = Z,
      covariates = X,
      model = model,
      n = base$n,
      class = "aprlb"
    ))
  }
}
