#' @keywords internal
.apr_engine <- function(data, Y, Z, X = NULL, model = "no_interaction") {

  y <- data[[Y]]
  z <- data[[Z]]

  if (!all(y %in% c(0,1))) stop(Y, " must be binary")
  if (!all(z %in% c(0,1))) stop(Z, " must be binary")

  # -----------------------------
  # CASE 1: NO COVARIATES
  # -----------------------------
  if (is.null(X)) {

    fmla <- as.formula(paste(Y, "~", Z))
    fit <- lm(fmla, data = data)

    Xmat <- model.matrix(fit)
    e <- residuals(fit)

    n <- nrow(data)

    vcov <- .sandwich_vcov(Xmat, e, n)

    est_z <- coef(fit)[Z]
    est_a <- coef(fit)["(Intercept)"]

    order <- c(Z, "(Intercept)")
    V <- vcov[order, order]

    # delta method gradient
    g <- matrix(c(
      1 / (1 - est_a),
      est_z / (1 - est_a)^2
    ), nrow = 1)

    var <- as.numeric(g %*% V %*% t(g))

    list(
      coef = est_z / (1 - est_a),
      se = sqrt(var),
      vcov = var,
      n = n,
      outcome = Y,
      instrument = Z,
      covariates = X,
      model = model,
      case = "no_covariates"
    )

  } else {

    Xf <- paste(X, collapse = " + ")

    # -----------------------------
    # MODEL 1: no interaction
    # -----------------------------
    if (model == "no_interaction") {

      fmla <- as.formula(paste(Y, "~", Z, "+", Xf))
      fit <- lm(fmla, data = data)

      yhat <- predict(fit)
      beta_z <- coef(fit)[Z]

      y1 <- yhat + beta_z - beta_z * z
      y0 <- yhat - beta_z * z
    }

    # -----------------------------
    # MODEL 2: interaction
    # -----------------------------
    if (model == "interaction") {

      f1 <- lm(as.formula(paste(Y, "~", Xf)), data = data[z == 1, ])
      f0 <- lm(as.formula(paste(Y, "~", Xf)), data = data[z == 0, ])

      y1 <- predict(f1, newdata = data)
      y0 <- predict(f0, newdata = data)
    }

    y1 <- pmin(pmax(y1, 0), 1)
    y0 <- pmin(pmax(y0, 0), 1)

    list(
      coef = mean(y1 - y0) / mean(1 - y0),
      se = NA,
      n = nrow(data),
      outcome = Y,
      instrument = Z,
      covariates = X,
      model = model,
      case = "covariates"
    )
  }
}
